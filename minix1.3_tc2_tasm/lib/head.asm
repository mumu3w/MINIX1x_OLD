;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;		Head.asm
;
; This is the first file to be linked when making mm, fs, and init
;
; Modified for Turbo Assembler by Deborah Mullen
; Date: Jan 29, 1989

INCLUDE model.inc

GLOBAL $main:NEAR, _main:NEAR, _exit:NEAR
GLOBAL _stackpt:WORD, _data_org:WORD, _brksize:WORD, splimit:WORD
GLOBAL endbss:BYTE, begbss:BYTE

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Data Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _DATA

_data_org:
	    DW 0DADAh		; fs needs to know where build stuffed table
            DW 7 dup(0)		; first 8 words of MM, FS, INIT are for stack
				; 0xDADA is magic number for build
_brksize:    DW offset DGROUP:endbss
splimit:     DW 0


ENDS _DATA


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Bss and Bssend Segments
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _BSS

begbss:

ENDS _BSS

SEGMENT _BSSEND

endbss:

ENDS _BSSEND


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP


$main:

	jmp SHORT L0
	ORG 10h                 ; kernel uses this area as stack for initial
L0:

IFDEF _SID
	mov ax, DGROUP		; Just for dos2out
ENDIF

	mov sp,[DGROUP:_stackpt]
	call _main
L1:	jmp short L1   		; this will never be executed

_exit:	jmp SHORT _exit		; this will never be executed either

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
