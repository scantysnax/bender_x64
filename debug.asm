;   _                    _           
;  | |__   ___ _ __   __| | ___ _ __ 
;  | '_ \ / _ \ '_ \ / _` |/ _ \ '__|
;  | |_) |  __/ | | | (_| |  __/ |   
;  |_.__/ \___|_| |_|\__,_|\___|_|
;
; bender -  a 6502/2A03 emulator written in x86 assembly
;			with plenty of help from others
; 			eli dayan 2012-2013
bits 64

section .text

%include "util.inc"

globl debug_init
globl debug_cleanup
globl debug_disassemble
globl debug_dump_registers

globl fp_disasm

extrn printf
extrn puts
extrn fopen
extrn fclose
extrn fprintf
extrn fputs
extrn fflush

extrn disasm

extrn rPC
extrn rA
extrn rX
extrn rY
extrn rP
extrn rS


section .data

align 8
fp_disasm	dq 0
txt_init 	db ":: debug_init()",13,10,0
txt_cleanup	db ":: debug_cleanup()",13,10,0
fn_disasm	db "./bender.disasm",0
str_mode	db "wt",0

align 8
txt_a 	db "A:    %02x",13,10,0
txt_x	db "X:    %02x",13,10,0
txt_y	db "Y:    %02x",13,10,0
txt_s	db "S:    %02x",13,10,0
txt_p	db "P:    %02x",13,10,0
txt_pc	db "PC:   %04x",13,10,0
txt_fmt db "%s\n",0


section .code


debug_init:
	mov rdi, txt_init
	call puts
	
	mov rdi, fn_disasm
	mov rsi, str_mode
	call fopen
	mov [fp_disasm], rax

	ret


debug_cleanup:
	mov rdi, txt_cleanup
	call puts
	
	mov rdi, [fp_disasm]
	call fflush
	
	mov rdi, [fp_disasm]
	call fclose

	ret


debug_disassemble:	
	push rbp
	mov rbp, rsp
	
	mov rdi, [rPC]
	mov rsi, [rP]
	mov rdx, [rA]
	mov rcx, [rX]
	mov r8, [rY]
	mov r9, [rS]
	
	multipush rbp, rbx, r12, r13, r14, r15
	call disasm
	multipop rbp, rbx, r12, r13, r14, r15
	
	mov rsi, [fp_disasm]
	mov rdi, rax
	call fputs
	
	pop rbp
	ret


debug_dump_registers:	
	push rbp
	mov rbp, rsp
	
	multipush rbp, rbx, r12, r13, r14, r15
	; PC
	mov rdi, txt_pc
	mov rsi, [rPC]
	and rsi, 0xffff
	xor rax, rax
	call printf

	; A
	mov rdi, txt_a
	movzx rsi, byte [rA]
	xor rax, rax
	call printf
	
	; X
	mov rdi, txt_x
	movzx rsi, byte [rX]
	xor rax, rax
	call printf
	
	; Y
	mov rdi, txt_y
	movzx rsi, byte [rY]
	xor rax, rax
	call printf
	
	; S
	mov rdi, txt_s
	movzx rsi, byte [rS]
	xor rax, rax
	call printf
	
	; P
	mov rdi, txt_p
	movzx rsi, byte [rP]
	xor rax, rax
	call printf

	multipop rbp, rbx, r12, r13, r14, r15
	
	pop rbp
	ret

