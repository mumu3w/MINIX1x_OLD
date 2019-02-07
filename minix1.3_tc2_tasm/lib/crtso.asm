;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;		Crtso.asm
;
; This is the C run-time start-off routine.  It's job is to take the
; arguments as put on the stack by EXEC, and to parse them and set 
; them up the way _main expects them.
;
; This must be the first file linked when linking object files to
; be used with Minix.  It establishes the order of the segments in
; addition to the other setup info that is done.
;
; It is not used for the Minix kernel, mm, fs, init, or fsck.
; For mm, fs, and init head.asm is used.
; For kernel, mpx88.asm is used.
; For fsck, fsck1.asm is used.


; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989


INCLUDE model.inc

GLOBAL $main:NEAR,_main:NEAR, _exit:NEAR
GLOBAL _environ:WORD, _brksize:WORD, endbss:BYTE, begbss:BYTE

STACKSIZE = 2560		; default stack

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Data Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _DATA


_brksize	DW  offset DGROUP:endbss   ; dynamic changable end of bss
_environ	DW	0 		   ; save envir ptr here

ENDS _DATA

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Bss and Bssend Segments
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _BSS

begbss:   

ENDS	_BSS

SEGMENT _BSSEND		; This segment only used for this label

endbss:

ENDS 	_BSSEND

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Stack Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SEGMENT _STACK
	DW  STACKSIZE dup(?)
ENDS	_STACK



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP


$main:
IFDEF _SID
	mov	ax,DGROUP	; For Separate I&D, put here
ENDIF
	mov	bx,sp		; set stackframe pointer ( ds=ss)
	mov	cx,[bx]		; get argc
	add	bx,2
	mov	ax,cx
	inc	ax		; calculate envp
	shl	ax,1
	add	ax,bx
	mov	[_environ],ax	; save envp in environ
	push	ax		; push environ
	push	bx		; push argv
	push	cx		; push argc
	call	_main		; call main (argc, argv, envp)
	add	sp,6
	push	ax		; push exit status
	call	_exit		; never returns
	ret

ENDS _TEXT


IFNDEF _SID           
SEGMENT _TEXTEND
ASSUME CS:_TEXT, DS:DGROUP

; This segment is only here to have this code at the
; end of the code segment for combined I & D.  It is never executed. 
; It forces the	linker to generate a relocation item, which dos2out 
; uses to determine the end of the text.  The way dos2out determines
; this is different for combined I & D and separate I & D.
;
	mov 	ax,DGROUP 

ENDS _TEXTEND

ENDIF

			END 

