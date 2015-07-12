;   _                    _           
;  | |__   ___ _ __   __| | ___ _ __ 
;  | '_ \ / _ \ '_ \ / _` |/ _ \ '__|
;  | |_) |  __/ | | | (_| |  __/ |   
;  |_.__/ \___|_| |_|\__,_|\___|_|
;
; bender -  a 6502/2A03 emulator written in x64 assembly
;			with plenty of help from others
; 			eli dayan 2013-2014
bits 64

section .text

%define BENDER_2A03	(0)	; 1 for NES, 0 for stock 6502 (with decimal mode)
%define MAX_PAGES (64)	; pages 1K-64K supported, in powers of two. (typically 8 for NES)
%define NSF_PLAYER (0)	; NSF player
%define HALT_ON_JAM (1)

%include "util.inc"

; important addresses
; usually standard across normal 6502 systems
%define ZERO_PAGE		0x0
%define STACK 			0x100
%define NMI_VECTOR 		0xfffa
%define RESET_VECTOR 	0xfffc
%define IRQ_VECTOR 		0xfffe


; reset and interrupt latencies.  i didn't know where to put these.
%define RESET_CYCLES 		6
%define INTERRUPT_CYCLES	7


; register map
%define PC r15
%define P r14b
%define A r13b
%define X r12b
%define Y r11b
%define S r10b


; 6502 registers (as available to the programmer)
globl rPC		; program counter
globl rP		; P (flags) register (nv-bdizc)
globl rA		; accumulator
globl rX		; x index
globl rY		; y index
globl rS		; stack


; cpu data (available to the programmer)
globl bender_memory				; pointer to paged memory (64K)
globl bender_needs_interrupt	; necessity for interrupt
globl bender_is_jammed			; jam status
globl bender_remaining_cycles	; # cycles left
globl bender_total_cycles		; # cycles executed
globl bender_cycles_to_eat		; # cycles to eat
globl bender_read_hook			; io read hook
globl bender_write_hook			; io write hook 
globl bender_jam_hook			; jam hook


; exported functions (provided to programmer)
globl bender_abort			; abort execution
globl bender_clearirq		; clear cpu interrupt flag and return
globl bender_dma			; dma a byte from memory and return
globl bender_eatcycles		; eat # of cycles and return
globl bender_elapsedcycles	; return # of cycles run so far
globl bender_init 			; initialise.  must be called before doing anything
globl bender_irq			; software interrupt
globl bender_nmi			; non-maskable interrupt
globl bender_reset			; set all regs to zero, maintenance, and go
globl bender_run			; run cpu # of cycles
globl bender_step			; run one instruction and return


; init data
section .data

; register set
align 8
	rPC dq 0x0
	rA 	db 0x0
	rX 	db 0x0
	rY  db 0x0
	rP  db 0x0
	rS 	db 0x0

; cpu data	
align 8
	bender_memory times MAX_PAGES	dq 0x0
	bender_needs_interrupt			db 0x0
	bender_is_jammed				db 0x0
	bender_remaining_cycles			dq 0
	bender_total_cycles				dq 0
	bender_cycles_to_eat			dq 0
	bender_read_hook				dq 0x0
	bender_write_hook				dq 0x0
	bender_jam_hook					dq 0x0

; flags
align 8
	nFlag db 0x0
	vFlag db 0x0
	bFlag db 0x0
	dFlag db 0x0
	iFlag db 0x0
	zFlag db 0x0
	cFlag db 0x0


; instruction jumptable
align 8
instruction_set:
	;   x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF  
	dq _00,_01,_02,_03,_04,_05,_06,_07,_08,_09,_0A,_0B,_0C,_0D,_0E,_0F ; 0x
	dq _10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_1A,_1B,_1C,_1D,_1E,_1F ; 1x
	dq _20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_2A,_2B,_2C,_2D,_2E,_2F ; 2x
	dq _30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_3A,_3B,_3C,_3D,_3E,_3F ; 3x
	dq _40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_4A,_4B,_4C,_4D,_4E,_4F ; 4x
	dq _50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_5A,_5B,_5C,_5D,_5E,_5F ; 5x
	dq _60,_61,_62,_63,_64,_65,_66,_67,_68,_69,_6A,_6B,_6C,_6D,_6E,_6F ; 6x
	dq _70,_71,_72,_73,_74,_75,_76,_77,_78,_79,_7A,_7B,_7C,_7D,_7E,_7F ; 7x
	dq _80,_81,_82,_83,_84,_85,_86,_87,_88,_89,_8A,_8B,_8C,_8D,_8E,_8F ; 8x
	dq _90,_91,_92,_93,_94,_95,_96,_97,_98,_99,_9A,_9B,_9C,_9D,_9E,_9F ; 9x
	dq _A0,_A1,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_AA,_AB,_AC,_AD,_AE,_AF ; Ax
	dq _B0,_B1,_B2,_B3,_B4,_B5,_B6,_B7,_B8,_B9,_BA,_BB,_BC,_BD,_BE,_BF ; Bx
	dq _C0,_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9,_CA,_CB,_CC,_CD,_CE,_CF ; Cx
	dq _D0,_D1,_D2,_D3,_D4,_D5,_D6,_D7,_D8,_D9,_DA,_DB,_DC,_DD,_DE,_DF ; Dx
	dq _E0,_E1,_E2,_E3,_E4,_E5,_E6,_E7,_E8,_E9,_EA,_EB,_EC,_ED,_EE,_EF ; Ex
	dq _F0,_F1,_F2,_F3,_F4,_F5,_F6,_F7,_F8,_F9,_FA,_FB,_FC,_FD,_FE,_FF ; Fx


section .text

; get the next instruction and execute it		
%macro _next 0
	cmp qword [bender_remaining_cycles], 0	; any cycles left
	jle _run_end							; nope, we're out of here		
	
	readmem byte, PC						; next instruction sits at PC
	inc PC									; PC++
	lea rdi, [instruction_set+rcx*8]		; load address of next instruction
	jmp [rdi]								; go!
%endmacro


; write a byte to memory
; %1: address
; %2: data
%macro writemem 2	
	save						; save regs
	mov rdi, %1					; rdi = address
	movzx rsi, %2				; rsi = data
	and rdi, 0xffff				; address is 16-bits wide
	call [bender_write_hook]	; call the write hook
	restore						; restore regs
%endmacro


; read from memory
; %1: size (byte/word)
; %2: address
; returns: cl (byte)/rcx (word)
%macro readmem 2
	save							; save regs
	mov rdi, %2						; get address into rdi
	and rdi, 0xffff					; address is 16-bits wide
	call [bender_read_hook]			; call the read hookls 
%ifidn %1, byte						; bytewise read					
	restore							; restore regs
	movzx rcx, al					; we need our byte in cl
%elifidn %1, word
	mov bl, al						; low byte
	mov rdi, %2						; reload from source reg
	inc rdi	
	and rdi, 0xffff						; advance address
	call [bender_read_hook]			; call the read hook
	restore							; restore regs
	movzx rcx, bl					
	mov ch, al						; high byte
%else
	%error "readmem: invalid size.  valid sizes are byte or word."
%endif
%endmacro


; write a byte to zero page.
; %1: address
; %2: byte to write
%macro zpwrite 2
	mov rbp, [bender_memory]		; rbp = [bender_memory]
	and %1, 0xff					; ensure address is 8-bits wide
	lea rdi, [rbp+ZERO_PAGE+%1]		; load destination address from zero page
	mov [rdi], %2					; write the byte
%endmacro


; read from zero page.    
; %1: size (byte/word)
; %2: address to read from
; returns: cl (byte)/rcx (word)
%macro zpread 2
	mov rbp, [bender_memory]			; rbp = [bender_memory]
	and %2, 0xff						; ensure address is 8-bits
	lea rdi, [rbp+ZERO_PAGE+%2]			; load destination address from zero page
	movzx rcx, %1 [rdi]					; byte/word
%endmacro


; restore 6502 registers and unpack the flags
%macro restore 0
	mov A, [rA]
	mov X, [rX]
	mov Y, [rY]
	mov P, [rP]
	mov S, [rS]
	mov PC, [rPC]
	unpackflags
%endmacro


; save 6502 registers and pack the flags back up
%macro save 0
	packflags
	mov byte [rA], A
	mov byte [rX], X
	mov byte [rY], Y
	mov byte [rP], P
	mov byte [rS], S
	mov qword [rPC], PC
%endmacro


; this is what happens when a jam instruction is encountered
%macro jam 0
	cmp qword [bender_jam_hook], 0x0	; is there a hook to call
	je %%no_hook						; no, skip it
	
	save
	call [bender_jam_hook]				; otherwise call the hook
	restore									; restore regs
	
%%no_hook:
	
	mov qword [bender_remaining_cycles], 0	; dump cycles
	mov byte [bender_is_jammed], 0x1		; we're jammed
%endmacro


; takes individual flags and 'packs' them into a suitable 'P' (flags) register for the 6502.
%macro packflags 0
	mov P, 0x20		; make sure we set R
	
	; each flag is guaranteed to be 0 or 1, so we can just shift them around
	mov al, [nFlag]
	shl al, 7
	or P, al
	
	mov al, [vFlag]
	shl al, 6
	or P, al
	
	; skip R
	
	mov al, [bFlag]
	shl al, 4
	or P, al
	
	mov al, [dFlag]
	shl al, 3
	or P, al
	
	mov al, [iFlag]
	shl al, 2
	or P, al
	
	mov al, [zFlag]
	shl al, 1
	or P, al
	
	or P, [cFlag]
%endmacro


; this does the oppposite of packflags.  it will take a properly formed 6502 'P'
; register, and separate it into individual flags, as our operations expect
; them to be.  each flag is shifted into the carry flag, then set by setc.
%macro unpackflags 0
	mov dl, P		
	
	shr dl, 1
	setc [cFlag]
	
	shr dl, 1
	setc [zFlag]
	
	shr dl, 1
	setc [iFlag]
	
	shr dl, 1
	setc [dFlag] 
	
	shr dl, 1
	setc [bFlag]
	
	; skip R
	
	shr dl, 2
	setc [vFlag]
	
	shr dl, 1
	setc [nFlag]
%endmacro


; pushes a byte onto the 6502 stack.
; %1: byte to push
%macro pushstack 1
	; stack is r10b, but we must use the full register for addressing
	mov rbp, [bender_memory]	; rbp = [bender_memory]
	and r10, 0xff				; ensure adddress is 8 bits
	lea rdi, [rbp+STACK+r10]
	mov [rdi], %1				; push byte
	dec S 						; stack grows down, so decrement S
%endmacro


; pops a byte off the 6502 stack.
; %1: byte to pop
%macro popstack 1
	; stack is r10b, but we must use the full register for addressing
	mov rbp, [bender_memory]	; rbp = [bender_memory]
	inc S 						; stack grows down, so incremement S when we pop
 	and r10, 0xff				; ensure adddress is 8-bits
 	lea rdi, [rbp+STACK+r10]
 	mov %1, [rdi] 				; pop byte
%endmacro


; use this for counting cycles.
%macro clock 1
	add qword [bender_total_cycles], %1			; increase total cycles
	sub qword [bender_remaining_cycles], %1		; decrease remaining cycles
%endmacro


; use for se* instructions
%macro setflag 1
	mov byte %1, 0x1	; set the flag
	clock 2				; clock two cycles
%endmacro


; use for cl* instructions
%macro clearflag 1
	mov byte %1, 0x0	; clear the flag
	clock 2				; clock 2 cycles
%endmacro


; many instructions set both N and Z based on the previous operation
%macro setnzflags 1
	test %1, 0xff	; touch RFLAGS (recall mov, inc, dec, etc. do not affect flags)
	sets [nFlag]
	setz [zFlag]
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       addressing         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; absolute address in rcx
%macro absoluteaddress 0
	readmem word, PC	; read a word at PC
	add PC, 2			; PC += 2
%endmacro


; immediate byte in cl
%macro immediate 0
	readmem byte, PC	; read a byte at PC
	inc PC				; PC++
%endmacro


; absolute-X address in rcx 
%macro absolutexaddress 0
	absoluteaddress		; calculate absolute address
	add cl, X			; add X offset 
	jnc %%done			; check for page cross (if we got a carry)
	
	inc ch				; increment address high byte
	clock 1				; add a cycle

%%done: 		
%endmacro


; absolute-x byte in cl
%macro absolutexbyte 0
	absolutexaddress	; caclulate absolutex address
	readmem byte, rcx	; read a byte at that address and stash it in cl
%endmacro


; absolute byte in cl
%macro absolutebyte 0
	absoluteaddress		; calculate absolute addresss
	readmem byte, rcx	; read a byte at that address and stash it in cl
%endmacro


; absolute read for RMW operations
; cl: byte to write
; r8: address written to
%macro readabsolute 0
	absoluteaddress		; calculate absolute address
	mov r8, rcx			; stash address in r8
	readmem byte, r8	; read a byte from stashed address
%endmacro


; absolutex read for RMW operations
; cl: byte to write
; r8: address written to
%macro readabsolutex 0
	readmem word, PC	; read a word at PC
	mov r8, rcx			; stash address in r8
	add r8b, X			; add X offset
	jnc %%done			; check for page cross (if we got a carry)

	add r8w, 0x100		; increment address high byte
%%done:
	readmem byte, r8	; read a byte at the address
	add PC, 2			; PC += 2
%endmacro


; absolutey read for RMW operations
; cl: byte to write
; r8: address written to
%macro readabsolutey 0
	readmem word, PC
	mov r8, rcx			; save address
	add r8b, Y			; add Y offset
	jnc %%done			; check for page cross (if we got a carry)

	add r8w, 0x100		; increment upper address byte
	
%%done:
	readmem byte, r8	; read a byte at the address
	add PC, 2			; PC += 2
%endmacro


; zero page address in cl
%macro zpaddress 0
	readmem byte, PC	; read a byte at PC
	inc PC				; PC++
%endmacro


; read zero page byte in cl
%macro zpbyte 0
	zpaddress			; caclulate zeropage addresss
	zpread byte, rcx	; read zeropage address into cl
%endmacro


; absolute-y address in rcx
%macro absoluteyaddress 0
	absoluteaddress	; calculate absolute address
	add cl, Y		; add Y offset
	jnc %%done		; check for page cross (if we got a carry)
	
	inc ch			; increment high byte
	clock 1			; one extra cycle

%%done:
%endmacro


; absolute-y byte in cl
%macro absoluteybyte 0
	absoluteyaddress	; caclulate absolute y address
	readmem byte, rcx	; return a byte at that addresss in cl
%endmacro


; indirect-y address in rcx
%macro indirectyaddress 0
	zpaddress			; caclulate zeropage adddress
	zpread word, rcx	; read zeropage word at that address
	add cl, Y			; add Y offset
	jnc %%done			; check for page cross (if we got a carry
	
	inc ch				; increment high byte
	clock 1				; one cycle penalty

%%done:
%endmacro


; indirect-y byte in cl
%macro indirectybyte 0
	indirectyaddress	; calculate indirecty adddress
	readmem byte, rcx	; read a byte at that adress, and return in cl
%endmacro


; indirect-x address in rcx
%macro indirectxaddress 0
	zpaddress			; calculate zp adddress
	add cl, X			; add X offset
	and rcx, 0xff		; ensure adddress is 8-bits
	zpread word, rcx	; read a word at that address, and return in rcx
%endmacro


; indirect-x read for RMW operations
; cl: byte to write
; r8: address written to
%macro readindirectx 0
	indirectxaddress	; calculate indirectx adddress
	mov r8, rcx			; stash address in r8 for later
	readmem byte, rcx	; read a byte at the address
%endmacro


; indirect-y read for RMW operations
; cl: byte to write
; r8: address written to
%macro readindirecty 0
	indirectyaddress	; calculate indirecty addresss
	mov r8, rcx			; stash address in r8 for later
	readmem byte, rcx	; read a byte at that address
%endmacro


; indirect-x byte in cl
%macro indirectxbyte 0
	indirectxaddress	; calculate indirectx adddress
	readmem byte, rcx	; read a byte at that address and stash in cl
%endmacro


; zeropage read for RMW operations
; cl: byte to write
; r8: address written to
%macro readzp 0
	zpaddress		; caclulate zeropage address
	mov r8, rcx		; stash addresss in r8 for later
	zpread byte, r8	; read zeropage from stashed address
%endmacro


; zeropage-x address in cl
%macro zpxaddress 0
	zpaddress	; caclulate zeropage adddress
	add cl, X	; add X offset, address is in cl
%endmacro


; zeropage-x byte in cl
%macro zpxbyte 0
	zpxaddress			; caclulate zpx address
	zpread byte, rcx	; read a byte at that address, and return in cl
%endmacro


; zeropagex read for RMW operations
; cl: byte to write
; r8: address written to
%macro readzpx 0
	zpxaddress			; caclulate zpx address
	mov r8, rcx			; stash in r8 for later
	zpread byte, r8		; read a byte at that address
%endmacro


; zeropage-y address in cl
%macro zpyaddress	0
	zpaddress	; caclulate zeropage address
	add cl, Y	; add Y offset, return address in cl
%endmacro


; zeropage-y read for RMW operations
; cl: byte to write
; r8: address written to
%macro readzpy 0
	zpyaddress			; calculate zeropagey address
	mov r8, rcx			; stash address for later
	zpread byte, r8		; read a byte at that address into cl
%endmacro


; zeropage-y byte in cl
%macro zpybyte 0
	zpyaddress			; calculate zeropagey address
	zpread byte, rcx	; read zeropage address into cl
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        instructions        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%if 0
; debuggging breadcrumbs
extrn printf
extrn debug_dump_registers

str_unimp db 13,10,"Unimplemented instruction [%02x]",13,10,0

%macro UNIMP 0
	mov rdi, str_unimp	
	mov rsi, rcx			; opcode (should be) in cl
	xor rax, rax
	save
	call printf	
	call debug_dump_registers
	restore
	jam
%endmacro
%endif

%macro addc 2
	%1
%if BENDER_2A03 == 0	
	test byte [dFlag], 0x1	; check for decimal mode
	jnz %%decimal
%endif
	; binary mode
	mov P, 0xff				; prepare carry flag
	add P, [cFlag]
	adc A, cl				; do the add
	sets [nFlag]			; adjust the flags accordingly
	seto [vFlag]
	setz [zFlag]
	setc [cFlag]
	jmp %%done	

%%decimal:
	movzx rdx, A				; dl = A
	and dl, 0xf					; dl = A & 0xf	
	mov rdi, rdx				; rdi = (A & 0xf)
	
	mov dl, cl					; dl = data
	and dl, 0xf					; dl = data & 0xf
	add rdi, rdx 				; rdi += (data & 0xf)	
	add dil, [cFlag]			; rdi = (A & 0xf) + (data & 0xf) + C
	cmp rdi, 0x9				; past 9?
	jg %%adjust					; yes, add 6
	
	jmp %%continue
	
%%adjust:
	add rdi, 0x6
	
%%continue:
	mov rax, rdi	; rax = temp
	and al, 0xf		; rax = temp & 0x0f
	movzx rdx, A	; rdx = A
	and rdx, 0xf0	; rdx = A & 0xf0
	add rax, rdx	; rdx += (A & 0xf0)
	mov dl, cl		; dl = data
	and dl, 0xf0	; dl = data & 0xf0
	add rax, rdx	; temp = (temp & 0xf) + (A & 0xf0) + (data & 0xf0)
	
	; check for carry
	cmp rdi, 0xf
	jg %%fix
	
	jmp %%nzflags
	
%%fix:
	add rax, 0x10	; apply carry from before
	
%%nzflags:	
	mov rbx, rax		; rbx = temp
	test rbx, 0x80
	setne [nFlag]		; set N
	
	mov dl, A			; A
	movzx rax, cl		; data
	add rax, rdx		; A + data
	add rax, [cFlag]	; rax = A + data + C
	and rax, 0xff
	setz [zFlag]		; set Z
	
    ; overflow is a bit tricky.
    mov rax, rbx	; rax = sum
    movzx rdx, A    ; rdx = A
    xor rdx, rax	; A = A ^ sum
    and rdx, 0x80	; ((A ^ sum) & 0x80)

    mov al, A
    xor al, cl		; (A ^ data)
    and al, 0x80	; ((A ^ data) & 0x80))
    xor al, 0x80
    and al, dl
    setnz [vFlag]	; set V

	mov rax, rbx
	and rax, 0x1f0
	cmp rax, 0x90	; check upper nybble past 9
	jg %%fixh
	jmp %%checkh
	
%%fixh:
	add rbx, 0x60	; yes, add 6
	
%%checkh:
	mov rax, rbx
	and rax, 0xff0
	cmp rax, 0x90	; check for past 9 overflow to set carry
	setg [cFlag]

	mov A, bl
	
%%done:
	clock %2
%endmacro


; bit (bit test)
%macro bit 2
	%1
	mov al, cl		; save operand
	shr cl, 7		; get d6 into carry
	setc [vFlag]	; v is set according to d6
	shr cl, 1		; get d7 into carry
	setc [nFlag]	; set sign
	and al, A		; and A with operand
	setz [zFlag]	; set Z
	clock %2
%endmacro


; branch (b**)
; %1: jz/jnz
; %2: flag
%macro branch 2	
	test byte %2, 0x1	; test the flag
	%1 %%branch			; take selected branch
	
	inc PC				; no branch, move along and clock
	clock 2				
	jmp %%done

%%branch:
	readmem byte, PC	; get offset
	inc PC				;
	movsx rcx, cl		; sign extend it to rcx
	mov rax, PC			; rax gets PC
	add al, cl			; apply offset
	jnc %%nopenalty		; 1-cycle penalty for crossing a page
	
	clock 1

%%nopenalty:
	add rcx, PC			; get our PC back
	mov PC, rcx
	clock 3
	
%%done:
%endmacro


; brk (break / (software interrupt))
%macro brk 0	 
	mov rax, PC						; save PC
	inc rax							; increment it
	mov rbx, rax					; save incremented PC
	shr rax, 8
	pushstack al					; push pc high
	pushstack bl					; push pc low
	mov byte [bFlag], 0x1			; set B
	packflags						; pack flags
	pushstack P						; push flags with B set
	mov byte [iFlag], 0x1			; set I
	readmem word, IRQ_VECTOR		; read from IRQ vector
	mov PC, rcx						; get new PC
	clock INTERRUPT_CYCLES
%endmacro


; compare macro for cmp,cpx,cpy
; %1: A/X/Y
; %2: read
; %3: write
%macro compare 3
	%2				; load operand
	cmp %1, cl		; perform the compare
	sets [nFlag]	; n flag
	setz [zFlag]	; z flag
	setnc [cFlag]	; c flag (inverted)
	clock %3
%endmacro


; dec (decrement memory)
; %1: read
; %2: write
; %3: cycles
%macro decm 3
	%1				; load operand	
	dec cl			; do the decrement
	setnzflags cl	; set n and z
	%2 r8, cl		; write it back
	clock %3
%endmacro


; dex (decrement X)
%macro dex 0		
	dec X			; decrement X	
	setnzflags X	; set N and Z
	clock 2
%endmacro


; dey (decrement Y)
%macro dey 0
	dec Y			; decrement Y
	setnzflags Y	; set N and Z
	clock 2
%endmacro


; inc (increment memory)
; %1: read
; %2: write
; %3: cycles
%macro incm 3
	%1				; read a byte
	inc cl			; increment it
	setnzflags cl	; flags
	%2 r8, cl		; write it back (destination in r8)
	clock %3
%endmacro


; inx (increment X)
%macro inx 0
	inc X			; icrement X
	setnzflags X	; set N and Z
	clock 2
%endmacro


; iny (increment Y)
%macro iny 0	
	inc Y			; increment Y
	setnzflags Y	; set N and Z
	clock 2
%endmacro


; jmp $nnnn (jump to address $nnnn)
%macro jmpa 0	
	readmem word, PC	 	; read a word at PC
	mov PC, rcx				; jump there
	clock 3
%endmacro


; jmp ($nnnn) (jump to address pointed to by $nnnn).  as a result of hardware design,
; there is an apparent 'bug' in this instruction.  any destination address  with a 
; low byte of $ff, the 6502 will not jump to $xxff and $xxff+1, but rather $xxff 
; and $xx00.  this behaviour is the result of the MOS engineers not wanting to waste
; gates to increment the high byte of the pointer.
%macro jmpi 0	
	readmem word, PC
	cmp cl, 0xff			; check low byte
	je %%fix				; if it's 0xff we need to adjust the address
	
	mov r8, rcx
	readmem word, r8		; read a word
	mov PC, rcx				; set PC
	jmp %%done				; we're done

%%fix:
	mov rbx, rcx			; save address
	and rbx, 0xff00			; clear out low byte
	readmem byte, rcx 		; read low byte	
	push rcx
	 readmem byte, rbx		; read a byte
	pop rsi
	shl rcx, 8				; shift address into upper byte
	mov PC, rcx				; get PC back
	or PC, rsi				; merge

%%done:
	clock 5
%endmacro



; jsr $nnnn	(jump to subroutine at $nnnn)
%macro jsr 0	
	mov rcx, PC				; save PC in rcx
	inc rcx					; increment it
	mov al, ch				; stash high byte in al
	
	pushstack al			; push high byte
	pushstack cl			; and low byte
	
	readmem word, PC		; read a word at PC
	mov PC, rcx				; get PC back
		
	clock 6
%endmacro


; load macro for lda/ldx/ldy
; %1: byte to read (A/X/Y)
; %2: read
; %3: cycles
%macro load 3	
	%2					; read
	mov %1, cl			; A/X/Y in cl
	setnzflags %1		; set N and Z
	clock %3
%endmacro


; store macro for sta/stx/sty
; %1: byte to write (A/X/Y)
; %2: read
; %3: write
; %4: cycles
%macro store 4	
	%2				; read
	%3 rcx, %1		; write A/X/Y
	clock %4
%endmacro


; shift macro for asl/lsr
; %1: direction
; %2: read
; %3: write
; %4: clock
%macro shift 2-4
%if %0 == 2 					; if 2 arguments were supplied, then A is implied
	%ifidn %1, right			; get shift direction
		shr A, 1				; shift
	%else
		shl A, 1				; same as above but left shift
	%endif
	
	setc [cFlag]				; flags
	setnzflags A
	clock 2	
%else							; four arguments, operand must be fetched
	%2
	%ifidn %1, right			; get shift direction
		shr cl, 1
		mov byte [nFlag], 0x0	; clear N (incoming zero)
	%else 						; same as above but left shift
		shl cl, 1
		sets [nFlag]			; but here we set N
	%endif
	
	setz [zFlag]				; remaining flags
	setc [cFlag]
	%3 r8, cl					; write it back
	
	clock %4
%endif			
%endmacro


; noop (no operation)
; does nothing, but PC and clock offsets can be supplied
; %1: PC offset
; %2: clock offset
%macro noop 2	
	add PC, %1
	clock %2
%endmacro


; pha (push accumulator to stack)
%macro pha 0	
	pushstack A
	clock 3
%endmacro


; php (push P (flags) to stack)
%macro php 0	
	packflags		; pack flags
	or P, 0x10		; ensure B is set
	pushstack P		; push P
	clock 3
%endmacro


; pla (pull accumulator from stack)
%macro pla 0
	popstack A
	setnzflags A
	clock 4
%endmacro


; plp (pull P (flags) from stack)
%macro plp 0
	popstack P		; pop P
	or P, 0x20		; ensure R is set
	unpackflags		; unpack flags
	clock 4
%endmacro


; rotate macro for rol (rotate left), ror (rotate right)
; %1: direction
; %2: read
; %3: write
; %4: clocks
%macro rotate 2-4
	%if %0 > 2	; !A
		%2		; fetch operand
	%endif
	
	mov P, byte 0xff
	add P, byte [cFlag]	; prepare carry flag
	
	%ifidn %1, right	; find rotate direction
		%if %0 == 2		; two params means A
			rcr A, 1	; rotate A
		%else
			rcr cl, 1	; otherwise rotate operand
		%endif
	%elifidn %1, left
		%if %0 == 2		; two params means A
			rcl A, 1	; rotate A
		%else
			rcl cl, 1	; otherwise rotate operand
		%endif
	%endif
	
	setc [cFlag]		; take care of carry flag
	
	%if %0 == 2 ; A
		setnzflags A	; set flags based on A
		clock 2
	%else
		setnzflags cl	; set flags based on operand
		%3 r8, cl		; write it back
		clock %4
	%endif
%endmacro


; rti (return from interrupt)
%macro rti 0	
	popstack P		; pull flags from stack
	unpackflags		; unpack flags
	or P, 0x20		; make sure R is set
	xor rax, rax	; clear these out for our pop
	xor rbx, rbx
	popstack al		; pull low byte
	popstack bl		; pull high byte
	shl rbx, 8		; form a word
	or rbx, rax
	mov PC, rbx		; save it in PC
	clock 6
%endmacro


; rts (return from subroutine)
%macro rts 0	
	xor rax, rax	; clear these
	xor rbx, rbx
	popstack al		; pop PC low byte
	popstack bl		; pop PC high byte
	shl rbx, 8		; PC high
	or rbx, rax		; combine with PC low
	inc rbx			; increment it
	mov PC, rbx		; stash PC here
	clock 6
%endmacro

	
; sbc (subtract with carry (borrow))
; %1: read
; %2: clock
%macro sbc 2
	%1
%if BENDER_2A03 == 0
	test byte [dFlag], 0x1	; same as above, but with D flag logic   
	jnz %%decimal                
%endif
	; binary mode
	mov P, byte [cFlag]		; set up P as 0 or -1 to get the carry logic right	 		
	sub P, 1
	sbb A, cl				; do the subtract
	sets [nFlag]			; take care of flags
	seto [vFlag]
	setz [zFlag]
	setnc [cFlag]	
	jmp %%done

%%decimal:
	mov P, byte [cFlag]		; set up P as 0 or -1 to get the carry logic right
	sub P, 1			
	
	; broadcast operand to a word, and split nybbles
	mov ah, cl	
	mov al, cl
	and rax, 0xf00f
	mov rdi, rax
	
	; broadcast A to a word and split nybbles
	; note we can't use ah here because A requires REX
	movzx rax, A
	shl rax, 8
	mov al, A
	and rax, 0xf00f
	sub rax, rdi	; if the low nybble borrows, it will do the - 0x10 on the high nybble for us
	
	movsx rdi, P
	add rax, rdi		; applies the borrow bit, so lda #0 / sbc #0 gets to be #$ff before the fixup
	test rax, 0x10		; check for low borrow
	jz %%nofixl
	
	sub al, 0x6				; adjust lower nybble accordingly

%%nofixl:
	test rax, 0x10000		; check for high borrow
	jz %%nofixh
	
	sub ah, 0x60			; adjust upper nybble accordingly

%%nofixh:
	and rax, 0xf00f			; mask to the result
	or al, ah				; combine the nybbles back together
	mov P, [cFlag]			; redo the subtract without fixups to do flags
	sub P, 1				; and throw the result away
	mov dl, A
	sbb dl, cl
	sets [nFlag]			; flags
	seto [vFlag]
	setz [zFlag]
	setnc [cFlag]
	
	mov A, al				; answer in al

%%done:
	clock %2
%endmacro


; transfer macro for all t** operations
%macro transfer 2
%ifidn %2, S 	; txs
	mov S, X
%else
	; any other transfer
	mov %2, %1
	setnzflags %2
%endif	
	clock 2
%endmacro


; logic operation (and/or/xor)
%macro logicop 3	
	%2				; fetch operand
	%1 A, cl		; and/or/xor A with cl
	setnzflags A	; set n and z
	clock %3
%endmacro



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; undocumented instructions ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; asr: this opcode ANDs the contents of the A register with an immediate value and 
; then LSRs the result.
%macro asrimm 0
	immediate		; get immediate
	and cl, A		; and with A
	shr cl, 1		; now shift right
	setc [cFlag]
	mov A, cl		; get result
	setnzflags A	; flags
	clock 2
%endmacro


; slo: this opcode ASLs the contents of a memory location and then ORs the result 
; with the accumulator.
%macro slo 3
	%1					; read from memory
	shl cl, 1			; shift left
	setc [cFlag]		; set C
	or A, cl			; or the result
	setnzflags A		; set N and Z
	%2 r8, cl			; write it back (destaddr in r8)
	clock %3
%endmacro


; sax: ANDs the contents of the A and X registers (without changing the 
; contents of either register) and stores the result in memory.
; does not affect any flags in the processor status register
%macro sax 3	
	%1			; read a byte
	mov cl, A	; get A
	and cl, X	; and with X
	; no flags
	%2 r8, cl	; write it back (destaddr in r8)
	clock %3
%endmacro


; dcp: this opcode DECs the contents of a memory location and then CMPs the result 
; with the A register.
%macro dcp 3	
	%1				; get a byte
	dec cl			; dec it
	cmp A, cl		; compare with A
	sets [nFlag]	; set flags
	setz [zFlag]
	setnc [cFlag]	; carry is inverted
	%2 r8, cl		; write it back (destaddr in r8)
	clock %3
%endmacro


; isc: this opcode INCs the contents of a memory location and then SBCs the result 
; from the A register.
%macro isc 3	
	%1			; read a byte	
	inc cl		; increment it
	%2 r8, cl	; write it back
	
%if BENDER_2A03 == 0
	test byte [dFlag], 0x1	; same as above, but with D flag logic   
	jnz %%decimal                
%endif
	; binary mode
	mov P, byte [cFlag]		; set up P as 0 or -1 to get the carry logic right	 		
	sub P, 1
	sbb A, cl				; do the subtract
	sets [nFlag]			; take care of flags
	seto [vFlag]
	setz [zFlag]
	setnc [cFlag]	
	jmp %%done

%%decimal:
	mov P, byte [cFlag]		; set up P as 0 or -1 to get the carry logic right
	sub P, 1			
	
	; broadcast operand to a word, and split nybbles
	mov ah, cl	
	mov al, cl
	and rax, 0xf00f
	mov rdi, rax
	
	; broadcast A to a word and split nybbles
	; note we can't use ah here because A requires REX
	movzx rax, A
	shl rax, 8
	mov al, A
	and rax, 0xf00f		; split nybbles
	
	sub rax, rdi		; if the low nybble borrows, it will do the - 0x10 on the high nybble for us
	movsx rdi, P
	add rax, rdi		; applies the borrow bit, so lda #0 / sbc #0 gets to be #$ff before the fixup
	test rax, 0x10		; check for low borrow
	jz %%nofixl
	
	sub al, 0x6			; adjust lower nybble accordingly

%%nofixl:
	test rax, 0x10000		; check for high borrow
	jz %%nofixh
	
	sub ah, 0x60			; adjust upper nybble accordingly

%%nofixh:
	and rax, 0xf00f			; mask to the result
	or al, ah				; combine the nybbles back together
	mov P, [cFlag]			; redo the subtract without fixups to do flags,
	sub P, 1				; and throw the result away
	mov dl, A
	sbb dl, cl
	sets [nFlag]
	seto [vFlag]
	setz [zFlag]
	setnc [cFlag]
	
	mov A, al				; answer in al
	
%%done:
	clock %3
%endmacro


; lax: this opcode loads both the accumulator and the X register with the contents 
; of a memory location.
%macro lax 2	
	%1				; read a byte
	mov A, cl		; into A
	mov X, cl		; and X as well
	setnzflags A
	clock %2
%endmacro


; sre: LSRs the contents of a memory location and then EORs the result with 
; the accumulator.
%macro sre 3
	%1				; get a byte
	shr cl, 1		; shift it right
	setc [cFlag]
	xor A, cl		; xor it with A
	setnzflags A
	%2 r8, cl		; write it back
	clock %3
%endmacro


; rla: ROLs the contents of a memory location and then ANDs the result with 
; the accumulator
%macro rla 3
	%1					; get a byte
	mov P, 0xff			; prepare C flag
	add P, [cFlag]
	rcl cl, 1			; rotate it
	setc [cFlag]
	and A, cl			; do the and
	setnzflags A
	%2 r8, cl			; write it back
	clock %3
%endmacro


; rra: RORs the contents of a memory location and then ADCs the result with 
; the accumulator.
%macro rra 3	
	%1
	mov P, 0xff	; prepare carry
	add P, [cFlag]
	rcr cl, 1			; do the rotate
	setc [cFlag]
	%2 r8, cl
	
%if BENDER_2A03 == 0
	test byte [dFlag], 0x1
	jnz %%decimal
%endif
	; binary mode
	mov P, 0xff
	add P, [cFlag]
	adc A, cl
	sets [nFlag]
	seto [vFlag]
	setz [zFlag]
	setc [cFlag]
	jmp %%done

%%decimal:
	movzx rdx, A
	and dl, 0xf					; dl = A & 0xf	
	mov rdi, rdx				; rdi = (A & 0xf)
	
	mov dl, cl					; dl = data
	and dl, 0xf					; dl = data & 0xf
	add rdi, rdx 				; rdi += (data & 0xf)
	movzx rdx, byte [cFlag]
	add rdi, rdx 				; rdi = (A & 0xf) + (data & 0xf) + C	
	
	cmp rdi, 0x9				; past 9?
	jg %%adjust					; yes, add 6
	jmp %%continue
	
%%adjust:
	add rdi, 0x6
	
%%continue:
	mov rax, rdi	; rax = temp
	and al, 0xf		; rax = temp & 0x0f
	movzx rdx, A	; rdx = A
	and rdx, 0xf0	; rdx = A & 0xf0
	add rax, rdx	; rdx += (A & 0xf0)
	mov dl, cl		; dl = data
	and dl, 0xf0	; dl = data & 0xf0
	add rax, rdx	; temp = (temp & 0xf) + (A & 0xf0) + (data & 0xf0)
	
	; check for carry
	cmp rdi, 0xf
	jg %%fix
	
	jmp %%nzflags
	
%%fix:
	; apply carry from before
	add rax, 0x10
	
%%nzflags:	
	mov rdi, rax	; rdi = temp
	mov dl, A		; A
	movzx rax, cl	; data
	add rax, rdx	; A + data
	add rax, [cFlag] ; rax = A + data + C
	and rax, 0xff
	setz [zFlag]	; set Z
	
	test rdi, 0x80
	setne [nFlag]	; set N
	
	; overflow is a bit tricky.
    mov rax, rdi		; rax = sum
    movzx rdx, A    	; rdx = A
    xor rdx, rax		; A = A ^ sum
    and rdx, 0x80		; ((A ^ sum) & 0x80)

    mov bl, A
    xor bl, cl			; (A ^ data)
    and bl, 0x80		; ((A ^ data) & 0x80))
    xor bl, 0x80
    and bl, dl
    setnz [vFlag]	; set V

	and rax, 0x1f0
	cmp rax, 0x90	; check upper nybble past 9
	jg %%fixh
	jmp %%checkh
	
%%fixh:
	add rdi, 0x60	; yes, add 6
	
%%checkh:
	mov rax, rdi
	and rax, 0xff0
	cmp rax, 0x90	; check for past 9 overflow to set carry
	setg [cFlag]
	
	mov A, dil
	
%%done:
	clock %3
%endmacro


; sbx: ANDs the contents of the A and X registers (leaving the contents of A 
; intact), subtracts an immediate value, and then stores the result in X.
%macro sbx 0	
	immediate		; get a byte
	mov al, A		; save A in al
	and al, X		; and it with X
	sub al, cl		; subtract immediate value
	setnc [cFlag]
	mov X, al		; store result in X
	setnzflags X
	clock 2
%endmacro


; anc: ANDs the contents of the A register with an immediate value and then 
; moves bit 7 of A into the Carry flag.
%macro ancimm 0
	immediate		; fetch a byte
	and A, cl		; and it with A
	setnzflags A
	mov al, [nFlag]	; N flag will be the same as d7, so move it to C
	mov [cFlag], al
%endmacro


; arr: this is a rather strange instruction which also has decimal mode.  
; here is how it was described:
;	
; 	AND byte with accumulator, then rotate one bit right in accumulator 
;	and check bit 5 and 6:
;	if both bits are 1: set C, clear V.
;	if both bits are 0: clear C and V.
;	if only bit 5 is 1: set V, clear C.
;	if only bit 6 is 1: set C and V.
;   Status flags: N,V,Z,C
%macro arrimm 0
	immediate
%if BENDER_2A03	== 0
	test byte [dFlag], 0x1 
	jnz %%decimal
%endif
	; binary mode
	and cl, A
	shr cl, 1
	mov A, cl
	mov dl, byte [cFlag]
	shl dl, 7
	or cl, dl
	mov A, cl
	setnzflags A
	test A, 0x40
	setnz [cFlag]
	
	movzx rdx, A
	mov rax, rdx
	shl rdx, 8
	or rdx, rax
	
	shr dl, 6
	shr dh, 5
	xor dl, dh
	and dl, 0x1
	setnz [vFlag]
	jmp %%done

%%decimal:	
	movzx rdi, A
	movzx rdx, cl
	movzx rax, byte [cFlag]
	and rdx, rdi		; t = rdx
	mov [nFlag], al
	
	shl rax, 8
	or rax, rdx			; rotate through carry
	shr rax, 1
	test al, 0xff
	setz [zFlag]
	
	mov A, al
	xor al, dl
	test al, 0x40		; set V accordingly, (t ^ A) & 64
	setnz [vFlag]
	mov dh, dl
	and rdx, 0xf00f		; split the nybbles up
	mov al, dl
	and al, 0x1
	add dl, al
	cmp dl, 5
	jbe %%nofixl
	
	mov al, A
	add al, 0x6
	and al, 0xf
	and A, 0xf0
	or A, al

%%nofixl:
	shr dh, 4
	mov al, dh
	and al, 0x1
	add dh, al
	cmp dl, 5
	seta [cFlag]			; C = AH + (AH & 1) > 5
	jbe %%done
	
	add A, 0x60

%%done:
%endmacro


; ane*: transfers the contents of the X register to the A register and then 
; ANDs the A register with an immediate value.
%macro aneimm 0	
	immediate
	mov A, X
	and A, cl
	setnzflags A
%endmacro


; las*: This opcode ANDs the contents of a memory location with the contents of the 
; stack pointer register and stores the result in the accumulator, the X 
; register, and the stack pointer.  Affected flags: N Z.
%macro lasay 0
	absoluteybyte	
	mov dl, S
	and cl, dl
	mov A, cl
	mov X, cl
	mov S, cl
	setnzflags cl	
%endmacro


; lxa*: This opcode ORs the A register with #$EE, ANDs the result with an immediate 
; value, and then stores the result in both A and X.
%macro lxaimm 0
	immediate
	or A, 0xee
	and A, cl
	mov X, A
	setnzflags X
  	clock 2
%endmacro


; sha*: this opcode stores the result of A AND X AND the high byte of the target 
; address of the operand +1 in memory.
%macro shaiy 0 
	zpaddress
	zpread word, rcx
	mov rdx, rcx
	mov rbx, rdx
	mov bl, ch
	inc bl
	and bl, X
	and bl, A
	add dl, Y
	jnc %%done
	
	mov dh, ch
	clock 1

%%done:
	writemem rdx, bl			; PC inc already done by zpaddress
	mov ch, bl
	clock 6
%endmacro


%macro shaay 0	
	readmem word, PC
	mov rdx, rcx
	mov bl, ch
	inc bl
	and bl, X
	and bl, A
	add dl, Y
	mov ch, bl
	jnc %%done
	
	mov dh, bl
	mov ch, bl
	clock 1
	
%%done:
	writemem rdx, bl
	add PC, 2
	clock 5
%endmacro



; shs*: this opcode ANDs the contents of the A and X registers (without changing 
; the contents of either register) and transfers the result to the stack 
; pointer.  It then ANDs that result with the contents of the high byte of 
; the target address of the operand +1 and stores that final result in 
; memory.  
%macro shs 0
	readmem word, PC
	mov rdx, rcx
	inc ch
	mov cl, ch			; build T in ch, X & HI+1
	and cl, X
	and cl, A
	mov S, cl			; transfer to stack
	add dl, Y			; add Y
	jnc %%done			; check for page cross (if we got a carry)
	
	mov dh, cl			; if there's a page cross, we write T to T:LO+Y
	clock 1

%%done:
	writemem rdx, cl	; write t to the address
	add PC, 2

	clock 5
%endmacro


; shx*: 
; Set T := X AND (HI+1)
;
; If LO+Y < 256:
;     store T at [HI : LO+Y]
; otherwise:
;     store T at [T : LO+Y]
%macro shx 0	
	readmem word, PC
	mov rdx, rcx
	mov bl, ch
	
	inc bl			; build T in ch, X & HI+1
	and bl, X
	add dl, Y		; add Y
	jnc %%done		; check for page cross (if we got a carry)
	
	mov dh, bl		; if there's a page cross, we write T to T:LO+Y
	clock 1

%%done:
	writemem rdx, bl		; write t to the address
	add PC, 2
	
	clock 5
%endmacro


; shy*:
; Set T := Y AND (HI+1)
;
; If LO+Y < 256:
;     store T at [HI : LO+X]
; otherwise:
;     store T at [T : LO+X]
%macro shy 0
	readmem word, PC
	mov rdx, rcx
	inc ch			; build T in ch, X & HI+1
	mov bl, ch
	and bl, Y
	add dl, X		; add X
	jnc %%done		; check for page cross (if we got a carry)
	
	mov dh, bl		; if there's a page cross, we write T to T:LO+X
	clock 1

%%done:
	writemem rdx, bl ; write t to the address
	mov ch, bl
	add PC, 2
	clock 5
%endmacro

; * = relies on race conditions or other unpredictable behaviour.  cannot be thoroughly tested.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        instructions        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

section .code

%if 0
_UN:
	UNIMP
_next
%endif

_00:	brk 									; brk
_next

_01:	logicop or, indirectxbyte, 6			; ora ($nn, X)
_next

_02: 	jam										; jam
_next

_03:	slo readindirectx, writemem, 8			; slo ($nn, X)
_next

_04:	noop 1, 3								; nop $nn
_next

_05:	logicop or, zpbyte, 3					; ora $nn
_next

_06:	shift left, readzp, zpwrite, 5			; asl $nn
_next

_07:	slo readzp, zpwrite, 5					; slo $nn		
_next

_08:	php										; php
_next

_09:	logicop or, immediate, 2				; ora #$nn
_next

_0A: 	shift left, A							; asl
_next

_0B:	ancimm									; anc #$nn
_next

_0C:	noop 2, 4								; nop $nnnn
_next

_0D:	logicop or, absolutebyte, 4				; ora $nnnn
_next

_0E:	shift left, readabsolute, writemem, 6	; asl $nnnn
_next

_0F:	slo readabsolute, writemem, 6			; slo $nnnn
_next

_10:	branch jz, [nFlag]						; bpl $nnnn
_next

_11:	logicop or, indirectybyte, 5			; ora ($nn), Y
_next

_12:	jam										; jam
_next

_13:	slo readindirecty, writemem, 8			; slo ($nn), Y
_next

_14:	noop 1, 4								; nop $nn, X
_next

_15:	logicop or, zpxbyte, 4					; ora $nn, X
_next

_16:	shift left, readzpx, zpwrite, 6			; asl $nn, X
_next

_17:	slo readzpx, zpwrite, 6					; slo $nn, X
_next

_18:	clearflag [cFlag]						; clc
_next

_19:	logicop or, absoluteybyte, 4			; ora $nnnn, Y
_next

_1A:	noop 0, 2								; nop
_next

_1B:	slo readabsolutey, writemem, 7			; slo $nnnn, Y
_next

_1C:	noop 2, 4								; nop $nnnn, X
_next

_1D:	logicop or, absolutexbyte, 4			; ora $nnnn, X
_next

_1E:	shift left, readabsolutex, writemem, 7 	; asl $nnnn,X
_next

_1F:	slo readabsolutex, writemem, 7			; slo $nnnn, X
_next
	
_20:	jsr										; jsr $nnnn
_next

_21:	logicop and, indirectxbyte, 6			; and ($nn,X)
_next

_22: 	jam										; jam
_next

_23:	rla readindirectx, writemem, 8			; rla ($nn, X)
_next

_24:	bit zpbyte, 3							; bit $nn
_next

_25:	logicop and, zpbyte, 3					; and $nn
_next

_26:	rotate left, readzp, zpwrite, 5			; rol $nn	
_next

_27:	rla readzp, zpwrite, 5					; rla $nn
_next

_28:	plp										; plp
_next

_29:	logicop and, immediate, 2				; and #$nn
_next

_2A:	rotate left, A							; rol
_next

_2B:	ancimm									; anc #$nn
_next

_2C:	bit absolutebyte, 4						; bit $nnnn	
_next

_2D:	logicop and, absolutebyte, 4			; and $nnnn
_next

_2E:	rotate left, readabsolute, writemem, 6 	; rol $nnnn
_next

_2F:	rla readabsolute, writemem, 6			; rla $nnnn
_next

_30:	branch jnz, [nFlag]						; bmi $nnnn
_next

_31:	logicop and, indirectybyte, 5			; and ($nn), Y
_next

_32: 	jam										; jam
_next

_33:	rla readindirecty, writemem, 8			; rla ($nn), Y
_next

_34:	noop 1, 4								; nop
_next

_35:	logicop and, zpxbyte, 4					; and $nn, X
_next

_36:	rotate left, readzpx, zpwrite,6			; rol $nn, X
_next

_37:	rla readzpx, zpwrite, 6					; rla $nn, X
_next

_38:	setflag [cFlag]							; sec
_next

_39:	logicop and, absoluteybyte, 4			; and $nnnn, Y
_next

_3A:	noop 0, 2								; nop
_next

_3B:	rla readabsolutey, writemem, 7			; rla $nnnn, Y
_next

_3C:	noop 2, 4								; nop $nnnn, X
_next

_3D:	logicop and, absolutexbyte, 4			; and $nnnn, X
_next

_3E:	rotate left, readabsolutex, writemem, 7	; rol $nnnn, X
_next

_3F:	rla readabsolutex, writemem,7			; rla $nnnn, X
_next

_40:	rti										; rti
_next

_41:	logicop xor, indirectxbyte, 6			; eor ($nn,X)
_next

_42: 	jam										; jam
_next

_43:	sre readindirectx, writemem, 8			; sre ($nn, X)
_next

_44:	noop 1, 3								; nop $nn
_next

_45:	logicop xor, zpbyte, 3					; eor $nn
_next

_46:	shift right, readzp, zpwrite, 3			; lsr $nn
_next

_47:	sre readzp, zpwrite, 8					; sre ($nn, X)
_next

_48:	pha										; pha
_next

_49:	logicop xor, immediate, 2				; eor #$nn
_next

_4A:	shift right, A							; lsr
_next

_4B:	asrimm									; asr #$nn
_next

_4C:	jmpa 									; jmp $nnnn
_next

_4D:	logicop xor, absolutebyte, 4			; eor $nnnn
_next

_4E:	shift right, readabsolute, writemem, 6	; lsr $nnnn
_next

_4F:	sre readabsolute, writemem, 6			; sre $nnnn
_next

_50:	branch jz, [vFlag]						; bvc $nnnn
_next

_51:	logicop xor, indirectybyte, 5			; eor ($nn),Y
_next

_52: 	jam										; jam
_next

_53:	sre readindirecty, writemem, 8			; sre ($nn), Y
_next

_54:	noop 1, 4								; nop $nn, X
_next

_55:	logicop xor, zpxbyte, 4					; eor $nn, X
_next

_56:	shift right, readzpx, zpwrite, 6		; lsr $nn, X
_next

_57:	sre readzpx, zpwrite, 6					; sre $nn, X
_next

_58:	clearflag [iFlag]						; cli
_next

_59:	logicop xor, absoluteybyte, 4			; eor $nnnn, Y
_next

_5A:	noop 0, 2								; nop
_next

_5B:	sre readabsolutey,writemem,7			; sre $nnnn, Y
_next

_5C:	noop 2,4								; nop $nnnn, X
_next

_5D:	logicop xor, absolutexbyte, 4			; eor $nnnn, X
_next

_5E:	shift right, readabsolutex, writemem, 7	; lsr $nnnn, X
_next

_5F:	sre readabsolutex, writemem, 7			; sre $nnnn, X
_next

_60:	rts										; rts
_next

_61:	addc indirectxbyte, 6					; add ($nn, X)
_next

_62: 	jam										; jam
_next

_63:	rra readindirectx, writemem, 8			; rra ($nn, X)
_next

_64:	noop 1, 3								; nop $nn
_next

_65:	addc zpbyte, 3							; adc $nn
_next

_66:	rotate right, readzp, zpwrite,5			; ror $nn
_next

_67:	rra readzp, zpwrite, 5					; rra $nn
_next

_68:	pla										; pla
_next

_69:	addc immediate, 2						; adc #$nn
_next

_6A:	rotate right, A							; ror
_next

_6B:	arrimm									; arr #$nn
_next

_6C:	jmpi									; jmp ($nnnn)
_next

_6D:	addc absolutebyte, 4					; adc $nnnn	
_next

_6E:	rotate right, readabsolute, writemem, 4	; ror $nnnn
_next

_6F:	rra readabsolute, writemem, 6			; rra $nnnn
_next

_70:	branch jnz, [vFlag]						; bvs $nnnn
_next

_71:	addc indirectybyte, 5					; adc ($nn), Y
_next

_72: 	jam										; jam
_next

_73:	rra readindirecty, writemem, 8			; rra ($nn), Y
_next

_74:	noop 1, 4								; nop $nn, X
_next

_75:	addc zpxbyte, 4;						; adc $nn, X
_next

_76:	rotate right, readzpx, zpwrite,6		; ror $nn, X
_next

_77:	rra readzpx, zpwrite, 6					; rra $nn, X
_next

_78:	setflag [iFlag]							; sei
_next

_79:	addc absoluteybyte, 4					; adc $nnnn, Y
_next

_7A:	noop 0, 2								; nop
_next

_7B:	rra readabsolutey, writemem, 7			; rra $nnnn, X
_next

_7C:	noop 2, 4								; nop $nnnn, X
_next

_7D:	addc absolutexbyte, 4					; adc $nnnn, X
_next

_7E:	rotate right, readabsolutex, writemem, 7; ror $nnnn, X
_next

_7F:	rra readabsolutex, writemem, 7			; rra $nnnn, X
_next

_80:	noop 1, 2								; nop #$nn
_next

_81:	store A, indirectxaddress, writemem,6	; sta ($nn, X)
_next

_82:	noop 1, 2								; nop #$nn
_next

_83:	sax readindirectx, writemem, 6			; sax ($nn, X)
_next

_84:	store Y, zpaddress, zpwrite, 3			; sty $nn
_next

_85:	store A, zpaddress, zpwrite,3			; sta $nn
_next

_86:	store X, zpaddress, zpwrite,3			; stx $nn
_next

_87:	sax readzp, zpwrite, 3					; sax $nn
_next

_88:	dey										; dey
_next

_89:	noop 1,2								; nop #$nn
_next

_8A:	transfer X, A							; txa	
_next

_8B:	aneimm									; ane #$nn
_next

_8C:	store Y, absoluteaddress, writemem, 4	; sty $nnnn
_next

_8D:	store A, absoluteaddress, writemem, 4	; sta $nnnn
_next

_8E:	store X, absoluteaddress, writemem, 4	; stx $nnnn
_next

_8F:	sax readabsolute, writemem, 4			; sax $nnnn
_next

_90:	branch jz, [cFlag]						; bcc $nnnn
_next

_91:	store A, indirectyaddress, writemem, 6	; sta ($nn), Y
_next

_92: 	jam										; jam
_next

_93:	shaiy									; sha ($nn), Y
_next

_94:	store Y, zpxaddress, zpwrite, 4			; sty $nn, X
_next

_95:	store A, zpxaddress, zpwrite, 4			; sta $nn, X
_next

_96:	store X, zpyaddress, zpwrite, 4			; stx $nn, Y
_next

_97:	sax readzpy, zpwrite, 4					; sax $nn, Y
_next

_98:	transfer Y, A							; tya
_next

_99:	store A, absoluteyaddress, writemem, 5	; sta $nnnn, Y
_next

_9A:	transfer X, S							; txs
_next

_9B:	shs										; shs $nnnn, Y
_next

_9C:	shy										; shy $nnnn, X
_next

_9D:	store A, absolutexaddress, writemem, 5	; sta $nnnn, X
_next

_9E:	shx										; shx $nnnn, Y
_next

_9F:	shaay									; sha $nnnn, Y
_next

_A0:	load Y, immediate, 2					; ldy #$nn
_next

_A1:	load A, indirectxbyte, 6				; lda ($nn, X)	
_next

_A2:	load X, immediate, 2					; ldx #$nn
_next

_A3:	lax readindirectx, 6					; lax ($nn, X)
_next

_A4:	load Y, zpbyte, 3						; ldy $nn
_next

_A5:	load A, zpbyte, 3						; lda $nn
_next

_A6:	load X, zpbyte, 3						; ldx $nn
_next

_A7:	lax readzp, 3							; lax $nn
_next

_A8:	transfer A, Y							; tay
_next

_A9:	load A, immediate, 2					; lda #$nn
_next

_AA:	transfer A, X							; tax
_next

_AB:	lxaimm									; lxa #$nn
_next

_AC:	load Y, absolutebyte,4					; ldy $nnnn
_next

_AD:	load A, absolutebyte,4					; lda $nnnn
_next

_AE:	load X, absolutebyte, 4					; ldx $nnnn
_next

_AF:	lax readabsolute, 4						; lax $nnnn
_next

_B0:	branch jnz, [cFlag]						; bcs $nnnn
_next

_B1:	load A, indirectybyte, 5				; lda ($nn), Y
_next

_B2: 	jam										; jam
_next

_B3:	lax readindirecty, 5					; lax ($nn), Y
_next

_B4:	load Y, zpxbyte, 4						; ldy $nn, X
_next

_B5:	load A, zpxbyte, 4						; lda $nn, X
_next

_B6:	load X, zpybyte, 4						; ldx $nn, Y
_next

_B7:	lax readzpy, 4							; lax ($nn), Y
_next

_B8:	clearflag [vFlag]						; clv
_next

_B9:	load A, absoluteybyte, 4				; lda $nnnn, Y
_next

_BA:	transfer S, X							; tsx
_next

_BB:	lasay									; las $nnnn, Y
_next

_BC:	load Y, absolutexbyte, 4				; ldy $nnnn, X
_next

_BD:	load A, absolutexbyte, 4				; lda $nnnn, X
_next

_BE:	load X, absoluteybyte, 4				; ldx $nnnn, Y
_next

_BF:	lax readabsolutey, 4					; lax $nnnn, Y
_next

_C0:	compare Y, immediate, 2					; cpy #$nn
_next

_C1:	compare A, indirectxbyte, 6				; cmp ($nn, X)
_next

_C2:	noop 1, 2								; nop #$nn
_next

_C3:	dcp readindirectx, writemem, 8			; dcp ($nn, X)
_next

_C4:	compare Y, zpbyte, 3					; cpy $nn
_next

_C5:	compare A, zpbyte, 3					; cmp $nn
_next

_C6:	decm readzp, zpwrite, 5					; dec $nn
_next

_C7:	dcp readzp, zpwrite, 6					; dcp $nn
_next

_C8:	iny										; iny
_next

_C9:	compare A, immediate, 2					; cmp #$nn
_next

_CA:	dex										; dex
_next

_CB:	sbx 									; sbx #$nn	
_next

_CC:	compare Y, absolutebyte, 4				; cpy $nnnn
_next

_CD:	compare A, absolutebyte, 4				; cmp $nnnn
_next

_CE:	decm readabsolute, writemem, 6			; dec $nnnn
_next

_CF:	dcp readabsolute, writemem, 6			; dcp $nnnn
_next

_D0:	branch jz, [zFlag]						; bne $nnnn
_next

_D1:	compare A, indirectybyte, 5				; cmp ($nn), Y
_next

_D3:	dcp readindirecty, writemem, 8			; dcp ($nn), Y
_next

_D2: 	jam										; jam
_next

_D4:	noop 1, 4								; nop $nn, X
_next

_D5:	compare A, zpxbyte, 4					; cmp $nn, X
_next

_D6:	decm readzpx, zpwrite, 6				; dec $nn, X
_next

_D7:	dcp readzpx, zpwrite, 6					; dcp $nn, X
_next

_D8:	clearflag [dFlag]						; cld
_next

_D9:	compare A, absoluteybyte, 4				; cmp $nnnn, Y
_next

_DA:	noop 0, 2								; nop
_next

_DB:	dcp readabsolutey, writemem, 7			; dcp $nnnn, Y
_next

_DC:	noop 2, 4								; nop $nnnn, X
_next

_DD:	compare A, absolutexbyte, 4				; cmp $nnnn,X
_next

_DE:	decm readabsolutex, writemem, 7			; dec $nnnn, X
_next

_DF:	dcp readabsolutex, writemem, 7			; dcp $nnnn, X
_next

_E0:	compare X, immediate, 2					; cpx #$nn
_next

_E1:	sbc indirectxbyte, 6					; sbc ($nn, X)
_next

_E2:	noop 1, 2								; nop #$nn
_next

_E3:	isc readindirectx, writemem, 8			; isc ($nn, X)
_next

_E4:	compare X, zpbyte, 3					; cpx $nn
_next

_E5:	sbc zpbyte, 3							; sbc $nn
_next

_E6:	incm readzp, zpwrite, 5					; inc $nn
_next

_E7:	isc readzp, zpwrite, 5					; isc $nn
_next

_E8:	inx										; inx
_next

_E9:	sbc immediate, 2						; sbc #$nn
_next

_EA:	noop 0, 2						; nop
_next

_EB:	sbc immediate, 2						; usbc #$nn
_next

_EC:	compare X, absolutebyte, 4				; cpx $nnnn
_next

_ED:	sbc absolutebyte, 4						; sbc $nnnn
_next

_EE:	incm readabsolute, writemem, 6			; inc $nnnn
_next

_EF:	isc readabsolute, writemem, 6			; isc $nnnn
_next

_F0:	branch jnz, [zFlag]						; beq $nnnn
_next

_F1:	sbc indirectybyte, 5					; sbc ($nn), Y
_next

_F2:	jam										; jam
_next

_F3:	isc readindirecty, writemem, 8			; isc ($nn), Y
_next

_F4:	noop 1, 4								; nop $nn, X
_next

_F5:	sbc zpxbyte, 4							; sbc $nn, X
_next

_F6:	incm readzpx, zpwrite, 6				; inc $nn,X
_next

_F7:	isc readzpx, zpwrite, 6					; isc $nn, X
_next

_F8:	setflag [dFlag]							; sed
_next

_F9:	sbc absoluteybyte, 4					; sbc $nnnn, Y
_next

_FA:	noop 0, 2								; nop
_next

_FB:	isc readabsolutey, writemem, 7			; isc $nnnn, Y
_next

_FC:	noop 2, 4								; nop $nnnn, X
_next

_FD:	sbc absolutexbyte, 4					; sbc $nnnn, X
_next

_FE:	incm readabsolutex, writemem, 7			; inc $nnnn,X
_next

_FF:	isc readabsolutex, writemem, 7			; isc $nnnn, X
_next
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;        functions        ;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;

align 8
; clear any pending interrupt(s)	
bender_clearirq:
	push rbp
	mov rbp, rsp
	mov [bender_needs_interrupt], byte 0x0
	pop rbp
	ret


align 8
; more or less a read from memory, this routine allows
; you to DMA a byte from memory.  it is useful on the NES 
; directly, and perhaps on other systems for other purposes	
bender_dma:	
	push rbp
	mov rbp, rsp
	save
	call [bender_read_hook]			; tail-call the read hook
	restore
	pop rbp
	ret


align 8
; some systems, such as the NES require the ability to waste cycles.
; when the NES does an OAM DMA transfer, 513 cpu cycles are "stolen"
; from otherwise cpu time.
bender_eatcycles:
	push rbp
	mov rbp, rsp
	add [bender_cycles_to_eat], rdi
	pop rbp
	ret
	
	
align 8
; this will return the number of cycles elapsed since the last call.
; if you need to reset the clock, pass anything other than 0 in, or
; else it will continue counting.
bender_elapsedcycles:
	push rbp
	mov rbp, rsp
	; check to see if we need to reset the count
	or rdi, rdi
	jnz .reset
	
	; nope, just return total cycles
	mov rax, [bender_total_cycles]
	pop rbp
	ret
	
.reset:
	; reset the count
	mov rax, [bender_total_cycles]
	mov qword [bender_total_cycles], 0
	pop rbp
	ret


align 8
; initialise the cpu.  clear out registers, assign i/o hooks, and return.
; good idea to soon call reset() soon afterward.
bender_init:
	push rbp
	mov rbp, rsp
	
	xor rax, rax
	mov [rPC], rax			; clear 6502 registers
	mov [rA], al
	mov [rX], al
	mov [rY], al
	mov [rP], al
	mov [rS], al
	
	mov [bender_needs_interrupt], al	; and interrupt & jam
	mov [bender_is_jammed], al
	
	; setup hooks
	mov [bender_read_hook], rdi
	mov [bender_write_hook], rsi
	mov [bender_jam_hook], rdx
	
	; the rest is taken care of when reset is called
	pop rbp
	ret
	

align 8
; software interrupt
bender_irq:
	push rbp
	mov rbp, rsp
	push rbx
	; we don't need to worry about r12-r15 because save/restore
	; takes care of that for us
	
	; make sure we aren't jammed
	cmp byte [bender_is_jammed], 0x0
	je .irq
	ret
	
.irq:
	restore
	
	test byte [iFlag], 0x1					; check for interupt
	jnz .pending	

	mov rdx, PC								; otherwise push pch & pcl to stack
	mov bl, dh
	pushstack bl
	pushstack dl
	mov [bFlag], byte 0x0					; make sure B is not set
	packflags
	pushstack P								; push flags to stack
	mov [iFlag], byte 0x1
	readmem word, IRQ_VECTOR
	mov PC, rcx
	add [bender_cycles_to_eat], byte INTERRUPT_CYCLES
	jmp .done

.pending:
	mov [bender_needs_interrupt], byte 0x1
	
.done:
	save
	pop rbx
	pop rbp
	ret
	

align 8
; non-maskable interrupt.  this happens on the NES, for example, at 60 times/sec.	
bender_nmi:
	push rbp
	mov rbp, rsp
	push rbx
	; we don't need to worry about r12-r15 because save/restore
	; takes care of that for us
	
	; if we're jammed, we don't need to take this interrupt.  
	; we drop it, and move along
	cmp [bender_is_jammed], byte 0x1
	jne .done
	
	restore
	mov rdx, PC		; get PC
	mov bl, dh
	pushstack bl	; push high-byte to stack
	pushstack dl	; push low-byte to stack
	
	mov [bFlag], byte 0x0		; don't want B set
	packflags
	pushstack P					; flags go on the stack as well

	readmem word, NMI_VECTOR	; read from nmi vector
	mov PC, rcx					; stash it in PC and go

	add [bender_cycles_to_eat], byte INTERRUPT_CYCLES
	save
		
.done:
	pop rbx
	pop rbp
	ret
	
	
align 8
; cut out of execution early
; essentially, any remaining cycles are dumped and we should in theory return 
; from execution after the current fetch finshes
bender_abort:
	push rbp
	mov rbp, rsp
	mov qword [bender_total_cycles], 0
	pop rbp
	ret


align 8
; reset the cpu, and various related cpu data.  most importantly, fetch the
; word at the reset vector and return.
bender_reset:
	push rbp
	mov rbp, rsp
	xor rax, rax							; clear out variables
	mov [bender_is_jammed], al
	mov [bender_needs_interrupt], al
	mov [bender_total_cycles], rax
	mov [bender_remaining_cycles], rax
	mov [bender_cycles_to_eat], rax
	mov byte [rS], 0xfd	; initialise stack
	mov byte [rP], 0x34 ; set B, I and R
	;mov [bender_cycles_to_eat], byte RESET_CYCLES ; weird things.
	restore
	
%if NSF_PLAYER == 0
	readmem word, RESET_VECTOR
	mov [rPC], rcx
%endif
	pop rbp
	ret


align 8
; run the cpu for (rdi) cycles, then return
bender_run:
	push rbp
	mov rbp, rsp
	push rbx
	; we don't need to worry about r12-r15 because save/restore
	; takes care of that for us
	
%if HALT_ON_JAM == 0
	xor rax, rax
	cmp byte [bender_is_jammed], al	
	jne _jam_exit
%endif	
	mov [bender_remaining_cycles], rdi
	mov rdi, [bender_total_cycles]
	push rdi ; stash cycle count here
	
	restore
	cmp qword [bender_remaining_cycles], 0
	jle _run_end
	
	mov rcx, [bender_cycles_to_eat]
	or rcx, rcx
	jz .check
	
	mov rdx, rcx
	cmp [bender_remaining_cycles], rcx
	jg .keepgoing
	mov rdx, [bender_remaining_cycles]
	
.keepgoing:
	sub [bender_remaining_cycles], rdx
	add [bender_total_cycles], rdx
	sub [bender_cycles_to_eat], rdx
	
.check:
	cmp qword [bender_remaining_cycles], 0
	jle .next
	
	cmp [bender_needs_interrupt], byte 0x0
	je .next
	
	test [iFlag], byte 0x1
	jnz .next
	
	; do the irq
	mov [bender_needs_interrupt], byte 0x0
	mov rdx, PC
	mov bl, dh
	pushstack bl
	pushstack dl
	mov [bFlag], byte 0x0
	packflags
	pushstack P
	mov [iFlag], byte 0x1
	readmem word, IRQ_VECTOR
	mov PC, rcx
	add qword [bender_cycles_to_eat], INTERRUPT_CYCLES
	
.next:
	cmp qword [bender_remaining_cycles], 0	; any cycles left
	jle _run_end							; nope, we're out of here		
	
	readmem byte, PC						; next instruction sits at PC
	inc PC									; PC++
	lea rdi, [instruction_set+rcx*8]		; load address of next instruction
	jmp [rdi]								; go!
	
_run_end:
	save
 	
 	pop rax	; pickup cycle count from before into rax for return
 	sub rax, [bender_total_cycles]	; subtract from stashed
 	neg rax	; negate to get proper cycle count

_jam_exit:	
	pop rbx
	pop rbp
	
	ret


align 8
; execute one instruction and return
; naturally (from bender_run) returns the number of cycles executed	
bender_step:
	push rbp
	mov rbp, rsp
	mov rdi, 1
	call bender_run		; go
	pop rbp
	ret

