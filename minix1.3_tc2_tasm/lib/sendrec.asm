;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Sendrec.asm
;
;	Library Routine
;
;
; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989

INCLUDE model.inc

GLOBAL _send:NEAR,_receive:NEAR,_sendrec:NEAR

; See ../h/com.h for C definitions
SEND1 = 1
RECEIVE = 2
BOTH = 3
SYSVEC = 32

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP



;*========================================================================*
;                           send and receive                              *
;*========================================================================*
; send(), receive(), sendrec() all save bp, but destroy ax, bx, and cx.

_send:
	mov cx,SEND1		; send(dest, ptr)
	jmp L0

_receive:
	mov cx,RECEIVE		; receive(src, ptr)
	jmp L0

_sendrec:
	mov cx,BOTH		; sendrec(srcdest, ptr)
	jmp L0

L0:
	push bp			; save bp
	mov bp,sp		; can't index off sp
	mov ax,[bp+4]		; ax = dest-src
	mov bx,[bp+6]		; bx = message pointer
	int SYSVEC		; trap to the kernel
	pop bp			; restore bp
	ret			; return

ENDS _TEXT

	END
