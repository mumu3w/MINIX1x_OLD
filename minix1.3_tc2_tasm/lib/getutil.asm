;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Getutil.asm
;
;	Library Routine
;
;
; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989

INCLUDE model.inc

GLOBAL _get_base:NEAR, _get_size:NEAR, _get_tot_mem:NEAR, endbss:BYTE


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP


;*========================================================================*
;                           utilities                                     *
;*========================================================================*

; return click at which prog starts
_get_base: 
	mov ax,ds
	ret

; return prog size in bytes (text+data+bss)
_get_size:
	mov ax,offset DGROUP:endbss 	; compiler label at end of bss
	ret

; Find out how much memory the machine has, including vectors, kernel MM, etc.
_get_tot_mem:

	cli
	push es
	push di
	mov ax,16384		; start search at 256K (16384 clicks)
	sub di,di
L1:
	mov es,ax
	mov [WORD PTR es:di],0A5A4h  	; write random bit pattern to memory
	xor bx,bx
	mov bx,[ WORD PTR es:di] ; read back pattern just written
	cmp bx,0A5A4h		; compare with expected value
	jne L2			; if different, no memory present
	add ax,4096		; advance counter by 64K
	cmp ax,0A000h		; stop seaching at 640K
	jne L1
L2:
	pop di
	pop es
	sti
	ret

ENDS _TEXT

	END
