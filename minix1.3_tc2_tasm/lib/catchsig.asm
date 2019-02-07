;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Catchsig.asm
;
;	Library Routine.
;
;	Note: Using setjmp.obj and setjmp.h that came with Turbo C
;       since Turbo C can use register variables SI and DI which
;       are not saved with Minix's setjmp.
;
;
; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989

INCLUDE model.inc

GLOBAL _begsig:NEAR, _vectab:WORD, _M:WORD

MTYPE = 2			; M+mtype = &M.m_type

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Data Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _DATA

	dummy: DW  0

ENDS _DATA

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT,DS:DGROUP

_begsig:
	push ax			; after interrupt, save all regs
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	push ds
	push es
	mov bx,sp
	mov bx,[bx+18]		; bx = signal number
	mov ax,bx		; ax = signal number
	dec bx			; vectab[0] is for sig 1
	add bx,bx		; pointers are two bytes on 8088
	mov bx,[_vectab+bx]	; bx = address of routine to call
	push [WORD PTR _M+MTYPE]; push status of last system call
	push ax			; func called with signal number as arg
	call bx
back:
	pop ax			; get signal number off stack
	pop [WORD PTR _M+MTYPE]	; restore status of previous system call
	pop es			; signal handling finished
	pop ds
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop [WORD PTR dummy]
	iret

ENDS _TEXT

	END
