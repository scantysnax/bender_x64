;   _                    _           
;  | |__   ___ _ __   __| | ___ _ __ 
;  | '_ \ / _ \ '_ \ / _` |/ _ \ '__|
;  | |_) |  __/ | | | (_| |  __/ |   
;  |_.__/ \___|_| |_|\__,_|\___|_|
;
; bender -  a 6502/2A03 emulator written in x64 assembly
;			with plenty of help from others
; 			eli dayan 2013-2014


; on win32, symbols are prefixed by an underscore.  i don't know.  also thanks
; to the NASM manual for the next few macros.
%ifdef WIN32
	%macro globl 1 
		global  _%1 
		%define %1 _%1 
	%endmacro

	%macro extrn 1 
		extern  _%1 
		%define %1 _%1 
	%endmacro
%else ; no leading underscores
	%define globl global
	%define extrn extern
%endif ; WIN32


; these are neat.  again, from the NASM manual.  pushes a list of registers
; onto the stack, or pops them off without having to reverse the order.
%macro  multipush 1-* 
	%rep  %0 
		push %1 
		%rotate 1 
	%endrep 
%endmacro

%macro  multipop 1-* 
	%rep %0 
  		%rotate -1 
        pop %1
	%endrep 
%endmacro

