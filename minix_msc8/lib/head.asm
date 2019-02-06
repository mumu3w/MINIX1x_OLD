title Head  -  Start-off for init, mm and fs (C86-compiler)
page ,132

PUBLIC 		__acrtused 		; magic symbol for MSC (indicates startup present)
__acrtused	EQU	9876H

EXTRN _main:NEAR
PUBLIC $main, _data_org, _brksize, _sp_limit, _end
EXTRN  _stackpt:word

INCLUDE	..\lib\prologue.h


_TEXT	SEGMENT
	assume	cs:_TEXT,ds:dgroup

$main:	jmp	short L0
	ORG 10h			; kernel uses this area	as stack for inital IRET
     L0:mov	sp,dgroup:_stackpt
	call	_main
     L1:jmp L1			; this will never be executed
	mov	ax,DGROUP	; force	relocation for dos2out (never executed)

_TEXT	ENDS


_DATAB	SEGMENT			; fs needs to know where build stuffed table
_data_org DW 0DADAh		; 0xDADA is magic number for build
	 DW 7 dup(0)		; first 8 words of MM, FS, INIT are for stack
_brksize	 DW	offset dgroup:_END  ; first free memory
_sp_limit DW	0
_DATAB	ENDS

_DATAV	SEGMENT				; DATAV	holds nothing. The label in
_END	label	byte		; the segment just tells us where
_DATAV	ENDS				; the data+bss ends.

@STACK	SEGMENT	BYTE STACK 'STACK'
@STACK	ENDS				; Add stack to satisfy DOS-linker

	END	$main			; end of assembly & entry-point
