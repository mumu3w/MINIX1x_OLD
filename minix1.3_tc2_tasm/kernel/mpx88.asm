;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Mpx88.asm
;
;   This is the startup code for the Kernel module of Minix OS
;
;
;   Modified for Turbo TASM by Deborah Mullen
;   Feb. 11, 1989
;

INCLUDE model.inc

;
; This file is part of the lowest layer of the MINIX kernel.  All processing
; switching and message handling is done here and in file "proc.c".  This file
; is entered on every transition to the kernel, both for sending/receiving
; messages and for all interrupts.  In all cases, the trap or interrupt
; routine first calls save() to store the machine state in the proc table.
; Then the stack is switched to k_stack.  Finally, the real trap or interrupt
; handler (in C) is called.  When it returns, the interrupt routine jumps to
; restart, to run the process or task whose number is in 'cur_proc'.
;
; The external entry points into this file are:
;   s_call:	process or task wants to send or receive a message
;   tty_int:	interrupt routine for each key depression and release
;   rs232_int:	interrupt routine for each rs232 interrupt on port 1
;   secondary:	interrupt routine for each rs232 interrupt on port 2
;   lpr_int:	interrupt routine for each line printer interrupt
;   disk_int:	disk interrupt routine
;   wini_int:	winchester interrupt routine
;   clock_int:	clock interrupt routine (HZ times per second)
;   eth_int:	ethernet interrupt routine
;   int00-int15:handlers for unused interrupt vectors < 16
;   trp:	all traps with vector >= 16 are vectored here
;   restart:	start running a task or process

; These symbols MUST agree with the values in ../h/com.h to avoid disaster.
K_STACK_BYTES	=  512
WINI		=   -6
FLOPPY		=   -5
CLOCK		=   -3
IDLE		= -999
DISKINT		=    1
CLOCK_TICK	=    2
NR_CONS		=    1

; The 	following procedures are defined in this file and called from outside it.
GLOBAL _tty_int:NEAR, _rs232_int:NEAR, _lpr_int:NEAR, _clock_int:NEAR
GLOBAL _disk_int:NEAR, _wini_int:NEAR
GLOBAL _eth_int:NEAR, _s_call:NEAR, _trp:NEAR, _restart:NEAR
GLOBAL _secondary_int:NEAR
GLOBAL _int00:NEAR, _int01:NEAR, _int02:NEAR, _int03:NEAR, _int04:NEAR
GLOBAL _int05:NEAR, _int06:NEAR, _int07:NEAR
GLOBAL _int08:NEAR, _int09:NEAR, _int10:NEAR, _int11:NEAR, _int12:NEAR
GLOBAL _int13:NEAR, _int14:NEAR, _int15:NEAR

; The following external procedures are called in this file.
GLOBAL _main:NEAR, _sys_call:NEAR, _interrup:NEAR, _keyboard:NEAR
GLOBAL _panic:NEAR, _unexpected_int:NEAR, _trap:NEAR
GLOBAL _pr_char:NEAR, _rs232:NEAR, _dp8390_int:NEAR

; Variables, data structures and miscellaneous.
GLOBAL _cur_proc:WORD, _proc_ptr:WORD, _scan_code:WORD
GLOBAL _int_mess:WORD, _k_stack:WORD, splimit:WORD
GLOBAL _sizes:WORD, begbss:BYTE
GLOBAL _brksize:WORD, endbss:BYTE

; The following constants are offsets into the proc table.
esreg = 14
dsreg = 16
csreg = 18
ssreg = 20
XP    = 22   ; SP is reserved in TASM, changed to XP
PC    = 24
PSW   = 28
SPLIM = 50
OFF   = 18
ROFF  = 12


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Data Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _DATA

_sizes:  DW 526Fh		; this must be the first data entry (magic #)
	 DW 7 dup(0)		; build table uses prev word and this space
bx_save: DW 0		; storage for bx
ds_save: DW 0		; storage for ds
ret_save:DW 0		; storage for return address
lds_low: DD 0		; storage used for restoring bx	and ds
_brksize:DW offset DGROUP:endbss

ttyomess: DB "RS232 interrupt"

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


;*===================================================================*
;* 			MINIX					     *
;*===================================================================*
MINIX:				; this is the entry point for the MINIX kernel.
	jmp SHORT mx0  		; skip over the next few bytes
	DW 0
        DW 0    		; build puts DS at kernel text address 4
mx0:
	cli                     ; disable interrupts
	mov ax,cs		; set up segment registers
	mov ds,ax		; set up ds
	mov ax,[cs:4]		; build has loaded this word with ds value
	mov ds,ax		; ds now contains proper value
	mov ss,ax		; ss now contains proper value
	mov [_scan_code],bx	; save scan code from bootstrap
  	mov sp, OFFSET _k_stack	; set sp to point to the top of the
	add sp,K_STACK_BYTES	; 	kernel stack

	call _main		; start the main program of MINIX
mx1:	jmp SHORT mx1 		; this should never be executed

IFDEF _SID
	mov ax, DGROUP		; force relocation
ENDIF

;*===========================================================================*
;*				idle					     *
;*===========================================================================*
idle:				; executed when there is no work 
	sti			; enable interrupts
L3:  	wait			; just idle while waiting for interrupt
	jmp L3			; loop until interrupt


;*===================================================================*
;*			restart					     *
;*===================================================================*
_restart:			; This routine sets up and runs a proc or task.
	cmp [_cur_proc],IDLE	; restart user; if cur_proc = IDLE, go idle
	je idle			; no user is runnable, jump to idle routine
	cli			; disable interrupts
	mov sp,[_proc_ptr]	; return to user, fetch regs from proc table
	pop ax			; start restoring registers
	pop bx			; restore bx
	pop cx			; restore cx
	pop dx			; restore dx
	pop si			; restore si
	pop di			; restore di
	mov [WORD PTR lds_low],bx 	; lds_low contains bx
	mov bx,sp		; bx points to saved bp register
	mov bp,[bx + SPLIM-ROFF]; splimit = p_splimit
	mov [splimit],bp 	; ditto
	mov bp,[bx + dsreg-ROFF]; bp = ds
	mov [WORD PTR lds_low+2],bp	; lds_low+2 contains ds
	pop bp			; restore bp
	pop es			; restore es
	mov sp,[bx + XP-ROFF]	; restore sp
	mov ss,[bx + ssreg-ROFF]; restore ss using the value of ds
	push [bx + PSW-ROFF]    ; push psw
	push [bx + csreg-ROFF]	; push cs
	push [bx + PC-ROFF]	; push pc
	lds bx,[DWORD PTR lds_low]		; restore ds and bx in one fell swoop
	iret			; return to user or task

;*===========================================================================*
;*				s_call					     *
;*===========================================================================*
_s_call:			; System calls are vectored here.
	call save		; save the machine state
	mov bp,[_proc_ptr]	; use bp to access sys call parameters
	push [bp+2]		; push(pointer to user message) (was bx)
	push [bp]		; push(src/dest) (was ax)
	push [_cur_proc]	; push caller
	push [bp+4]		; push(SEND/RECEIVE/BOTH) (was cx)
	call _sys_call		; sys_call(function, caller, src_dest, m_ptr)
	jmp _restart		; jump to code to restart proc/task running


;*===========================================================================*
;*				tty_int					     *
;*===========================================================================*
_tty_int:			; Interrupt routine for terminal input.
	call save		; save the machine state
	call _keyboard		; process a keyboard interrupt
	jmp _restart		; continue execution


;*============================================================================
;*				rs232_int				     *
;*============================================================================
_rs232_int:			; Interrupt routine for rs232 I/O.
	call save		; save the machine state
	mov ax,NR_CONS		; which unit caused the interrupt
	push ax			; pass it as a parameter
	call _rs232		; process a rs232 interrupt
	jmp _restart		; continue execution


;*============================================================================
;*				secondary_int				     *
;*============================================================================
_secondary_int:			; Interrupt routine for rs232 port 2
	call save		; save the machine state
	mov ax,NR_CONS + 1	; which unit caused the interrupt
	push ax			; pass it as a parameter
	call _rs232		; process a rs232 interrupt
	jmp _restart		; continue execution


;*===========================================================================*
;*				lpr_int					     *
;*===========================================================================*
_lpr_int:			; Interrupt routine for terminal input.
	call save		; save the machine state
	call _pr_char		; process a line printer interrupt
	jmp _restart		; continue execution


;*===========================================================================*
;*				disk_int				     *
;*===========================================================================*
_disk_int:			; Interrupt routine for the floppy disk.
	call save		; save the machine state
	mov [_int_mess+2],DISKINT; build message for disk task
	mov ax,OFFSET _int_mess	; prepare to call interrupt(FLOPPY, &intmess)
	push ax			; push second parameter
	mov ax,FLOPPY		; prepare to push first parameter
	push ax			; push first parameter
	call _interrup		; this is the call
	jmp _restart		; continue execution


;*===========================================================================*
;*				wini_int				     *
;*===========================================================================*
_wini_int:			; Interrupt routine for the winchester disk.
	call save		; save the machine state
	mov [_int_mess+2],DISKINT; build message for winchester task
	mov ax,OFFSET _int_mess	; prepare to call interrupt(WINI, &intmess)
	push ax			; push second parameter
	mov ax,WINI		; prepare to push first parameter
	push ax			; push first parameter
	call _interrup		; this is the call
	jmp _restart		; continue execution


;*===========================================================================*
;*				clock_int				     *
;*===========================================================================*
_clock_int:			; Interrupt routine for the clock.
	call save		; save the machine state
	mov [_int_mess+2],CLOCK_TICK	; build message for clock task
	mov ax,OFFSET _int_mess	; prepare to call interrupt(CLOCK, &intmess)
	push ax			; push second parameter
	mov ax,CLOCK		; prepare to push first parameter
	push ax			; push first parameter
	call _interrup		; this is the call
	jmp _restart		; continue execution


;*===========================================================================*
;*				eth_int					     *
;*===========================================================================*
_eth_int:			; Interrupt routine for ethernet input
	call save		; save the machine state
	call _dp8390_int	; call the handler
	jmp _restart		; continue execution


;*===========================================================================*
;*				int00-15				     *
;*===========================================================================*

vec_mess:			; this is where unexpected interrupts come.
	push ax			; push the vector number
	call _unexpected_int	; go panic
	jmp _restart		; never executed


_int00:	call save		; interrupt through vector 0
	mov ax,0		; save vector number in ax
	jmp vec_mess		; print message
	
_int01:	call save		; interrupt through vector 1
	mov ax,1		; save vector number in ax
	jmp vec_mess		; print message
	
_int02:	call save		; interrupt through vector 1
	mov ax,2		; save vector number in ax
	jmp vec_mess		; print message
	
_int03:	call save		; interrupt through vector 3
	mov ax,3		; save vector number in ax
	jmp vec_mess		; print message
	
_int04:	call save		; interrupt through vector 4
	mov ax,4		; save vector number in ax
	jmp vec_mess		; print message
	
_int05:	call save		; interrupt through vector 5
	mov ax,5		; save vector number in ax
	jmp vec_mess		; print message
	
_int06:	call save		; interrupt through vector 6
	mov ax,6		; save vector number in ax
	jmp vec_mess		; print message
	
_int07:	call save		; interrupt through vector 7
	mov ax,7		; save vector number in ax
	jmp vec_mess		; print message
	
_int08:	call save		; interrupt through vector 8
	mov ax,8		; save vector number in ax
	jmp vec_mess		; print message
	
_int09:	call save		; interrupt through vector 9
	mov ax,9		; save vector number in ax
	jmp vec_mess		; print message
	
_int10:	call save		; interrupt through vector 10
	mov ax,10		; save vector number in ax
	jmp vec_mess		; print message
	
_int11:	call save		; interrupt through vector 11
	mov ax,11		; save vector number in ax
	jmp vec_mess		; print message
	
_int12:	call save		; interrupt through vector 12
	mov ax,12		; save vector number in ax
	jmp vec_mess		; print message
	
_int13:	call save		; interrupt through vector 13
	mov ax,13		; save vector number in ax
	jmp vec_mess		; print message
	
_int14:	call save		; interrupt through vector 14
	mov ax,14		; save vector number in ax
	jmp vec_mess		; print message
	
_int15:	call save		; interrupt through vector 15
	mov ax,15		; save vector number in ax
	jmp vec_mess		; print message
	
;*===========================================================================*
;*				trp					     *
;*===========================================================================*
_trp:				; this is where unexpected traps come.
	call save		; save the machine state
	call _trap		; print a message
	jmp _restart		; this error is not fatal


;*===========================================================================*
;*				save					     *
;*===========================================================================*
save:				; save the machine state in the proc table.  
	cld			; set direction flag to a known value
	push ds			; stack: psw/cs/pc/ret addr/ds
	push cs			; prepare to restore ds
	pop ds			; ds has now been set to cs
	mov ds,[cs:4]		; word 4 in kernel text space contains ds value
	pop [WORD PTR ds_save]		; stack: psw/cs/pc/ret addr
	pop [WORD PTR ret_save]		; stack: psw/cs/pc
	mov [WORD PTR bx_save],bx 	; save bx for later ; we need a free register
	mov bx,[_proc_ptr]	; start save set up; make bx point to save area
	add bx,OFF		; bx points to place to store cs
	pop [bx + PC-OFF]	; store pc in proc table 
	pop [bx + csreg-OFF]	; store cs in proc table
	pop [bx + PSW-OFF]	; store psw 
	mov [bx + ssreg-OFF],ss	; store ss
	mov [bx + XP-OFF],sp	; sp as it was prior to interrupt
	mov sp,bx		; now use sp to point into proc table/task save
	mov bx,ds		; about to set ss
	mov ss,bx		; set ss
	push [WORD PTR ds_save]		; start saving all the registers, sp first
	push es			; save es between sp and bp
	mov es,bx		; es now references kernel memory too
	push bp			; save bp
	push di			; save di
	push si			; save si
	push dx			; save dx
	push cx			; save cx
	push [WORD PTR bx_save]		; save original bx
	push ax			; all registers now saved
	mov sp, OFFSET _k_stack	; temporary stack for interrupts
	add sp,K_STACK_BYTES	; set sp to top of temporary stack
	mov [splimit], OFFSET _k_stack; limit for temporary stack
	add [splimit],8		; splimit checks for stack overflow
	cld
	mov ax,[WORD PTR ret_save]	; ax = address to return to
	jmp (ax)		; return to caller; Note: sp points to saved ax


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
