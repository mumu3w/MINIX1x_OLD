;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Portio.asm
;	
;	Library Routine
;
;
; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989

INCLUDE model.h

GLOBAL _port_out:NEAR, _port_in:NEAR, _peek:NEAR

; These routines are used by the kernel to write and read I/O ports.
; They can also be called by user programs

; port_out(port, value) writes 'value' on the I/O port 'port'.

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP


_port_out:
	push bx			; save bx
	mov bx,sp		; index off bx
	push ax			; save ax
	push dx			; save dx
	mov dx,[bx+4]		; dx = port
	mov ax,[bx+6]		; ax = value
	out dx,al		; output 1 byte
	pop dx			; restore dx
	pop ax			; restore ax
	pop bx			; restore bx
	ret			; return to caller


; port_in(port, &value) reads from port 'port' and puts the result in 'value'.
_port_in:
	push bx			; save bx
	mov bx,sp		; index off bx
	push ax			; save ax
	push dx			; save dx
	mov dx,[bx+4]		; dx = port
	in al,dx		; input 1 byte
	xor ah,ah		; clear ah
	mov bx,[bx+6]		; fetch address where byte is to go
	mov [bx],ax		; return byte to caller in param
	pop dx			; restore dx
	pop ax			; restore ax
	pop bx			; restore bx
	ret			; return to caller

; value = peek(segment, offset)
_peek:
	push bp			; save bp
	mov bp,sp		; we need to access parameters
	push es			; save es
	mov es,[bp+4]		; load es with segment value
	mov bx,[bp+6]		; load bx with offset from segment
	mov al,[es:bx]		; al = byte
	xor ah,ah		; ax = byte
	pop es			; restore es
	pop bp			; restore bp
	ret			; return to caller
ENDS _TEXT

	END
