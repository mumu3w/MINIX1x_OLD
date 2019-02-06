Title $MAIN    C-86 run-time start-off for Minix
page,132



PUBLIC	_end, _brksize, _edata, $main, _environ, _kamikaze
EXTRN   _main:NEAR, _exit:NEAR

INCLUDE ..\lib\prologue.h

STACKSIZE EQU 2560	; default stack is 5 Kb (2K words)

PUBLIC	__acrtused
__acrtused	EQU	9876H	; used by MSC to pull in startup routine



_TEXT	SEGMENT
	ASSUME	CS:_TEXT,DS:DGROUP

; This is the C run-time start-off routine.  It's job is to take the
; arguments as put on the stack by EXEC, and to parse them and set them up the
; way main expects them.
;

$main:
	mov	ax,DGROUP	; force relocation of data & bss
	mov	bx,sp		; set stackframe pointer (ds=ss)
	mov	cx,[bx]		; get argc
	add	bx,2		; point at next parameter
	mov	ax,cx
	inc	ax		; calculate envp
	shl	ax,1
	add	ax,bx
	mov	_environ,ax	; save envp
	push	ax		; stack envp
	push	bx		; stack argv
	push	cx		; stack argc

	call	_main		; call main (arc,argv,envp)

	add	sp,6
	push	ax		; stack program-termination status
	call	_exit		; this will never return

	; DEBUG from here on

_kamikaze: 	int 3
		ret

_TEXT	ENDS


_DATA	SEGMENT
_brksize	DW	offset dgroup:_END+2  	; dynamic changeable end of bss
_environ	DW	0			; save environment pointer here
_DATA 	ENDS

_DATAT	SEGMENT				; DATAT holds nothing. The label just
_edata	label byte			; tells us where the initialized data
_DATAT	ENDS				; (.data) ends.

_DATAV	SEGMENT				; DATAV holds nothing. The label in
_end	label byte			; the segment just tells us where
_DATAV	ENDS				; the data+bss ends.

_STACK	SEGMENT	BYTE STACK 'STACK'
	DW	STACKSIZE dup(?)	; add stack segment to bss
_STACK	ENDS


	END	$main	; program entry-point (could be anywhere)
