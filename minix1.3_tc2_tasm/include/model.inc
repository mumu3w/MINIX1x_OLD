;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                  Model.h
;
;     Include file for all assembly language files to
;     to used for Minix. 
;
;     For the default mode, the segments are those as defined for
;     Turbo C with the tiny model.  If separate I & D is desired,
;     define _SID when assembling. 
;
;     For Combined I & D, notice the extra segemnt, _TEXTEND.
;     This is because the reloction code MUST be the last code
;     of the linked model.  For Separate I & D, it does not matter
;     where the relocation code is.
;
;     For both models, the segment _BSSEND is used to define a label,
;     "endbss" and that is all.	 The label "begbss" is the first label
;     in the _BSS segment in the startup code.
;
;
;     Deborah Mullen
;     Feb. 26, 1989
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.SEQ			; Must have seuential segment ordering
IDEAL			; Ideal assembly mode
NOLOCALS		; Disables local symbols
%NOINCL			; Disables listing of include files
%PAGESIZE 60,132 	; Set listing paramenters

IFDEF _SID
	DISPLAY "Separate I & D"

SEGMENT	_TEXT		BYTE	PUBLIC	'CODE'
ENDS	_TEXT

SEGMENT	_DATA		PARA	PUBLIC	'DATA'
ENDS	_DATA

SEGMENT	_DATAEND		PARA	PUBLIC	'DATA'
ENDS	_DATAEND

SEGMENT	_BSS		WORD	PUBLIC	'BSS'
ENDS	_BSS

SEGMENT	_BSSEND		BYTE	PUBLIC	'BSS'
ENDS	_BSSEND

SEGMENT	_STACK		PARA	STACK 	'STACK'
ENDS	_STACK

GROUP	DGROUP	_DATA, _DATAEND, _BSS, _BSSEND, _STACK

ELSE
	DISPLAY "Combined I & D"

SEGMENT	_TEXT		WORD	PUBLIC	'CODE'
ENDS	_TEXT

SEGMENT	_TEXTEND	BYTE	PUBLIC	'CODE'
ENDS	_TEXTEND

SEGMENT	_DATA		WORD	PUBLIC	'DATA'
ENDS	_DATA

SEGMENT	_DATAEND		PARA	PUBLIC	'DATA'
ENDS	_DATAEND

SEGMENT	_BSS		WORD	PUBLIC	'BSS'
ENDS	_BSS

SEGMENT	_BSSEND		BYTE	PUBLIC	'BSS'
ENDS	_BSSEND

SEGMENT	_STACK		PARA	STACK 	'STACK'
ENDS	_STACK

GROUP	DGROUP	_TEXT, _TEXTEND, _DATA, _DATAEND, _BSS, _BSSEND, _STACK

ENDIF
