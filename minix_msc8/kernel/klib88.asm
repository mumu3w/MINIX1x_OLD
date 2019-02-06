title klib88
page,132

; This file contains a number of assembly code utility routines	needed by the
; kernel.  They	are:
;
;   phys_copy:	copies data from anywhere to anywhere in memory
;   cp_mess:	copies messages	from source to destination
;   port_out:	outputs	data on	an I/O port
;   port_in:	inputs data from an I/O	port
;   lock:	disable	interrupts
;   unlock:	enable interrupts
;   restore:	restore	interrupts [enable/disabled] as	they were before lock[]
;   build_sig:	build 4	word structure pushed onto stack for signals
;   csv:	procedure prolog to save the registers
;   cret:	procedure epilog to restore the	registers
;   get_chrome:	returns	0 is display is	monochrome, 1 if it is color
;   vid_copy:	copy data to video ram [on color display during	retrace	only]
;   get_byte:	reads a	byte from a user program and returns it	as value

; The following	procedures are defined in this file and	called from outside it.
PUBLIC _phys_copy, _cp_mess, _port_out, _port_in, _lock, _unlock, _restore
PUBLIC _build_sig, _get_chrome, _vid_copy,	_get_byte
PUBLIC _reboot, _wreboot

	ifdef GENERIC_FDISK
PUBLIC _hdisk_params, _diskio, _win_init, _wn_low_safety, _wn_low_xor
	endif

; The following	external procedure is called in	this file.
EXTRN _panic:NEAR

; Variables and	data structures
EXTRN _color:WORD, _cur_proc:WORD, _proc_ptr:WORD,	_splimit:WORD
EXTRN _vid_mask:WORD
PUBLIC _vec_table

INCLUDE	..\lib\prologue.h

;
; Macro to call the Hard Disk BIOS.  This is a macro because int 13 won't
; work with Minix on my BIOS... don't know why... this lets you 
; change it easily.
;
hdcall	macro
	ifdef	ERSATZ_GENERIC
	pushf
	call	dword ptr [disk_rom]
	else
	int	13H
	endif
	endm

_TEXT	SEGMENT
	assume	cs:_text,ds:dgroup

;===========================================================================
;				phys_copy
;===========================================================================
; This routine copies a	block of physical memory.  It is called	by:
;    phys_copy(	(long) source, (long) destination, (long) bytecount)

_phys_copy:
	pushf			; save flags
	cli			; disable interrupts
	push bp			; save the registers
	push ax			; save ax
	push bx			; save bx
	push cx			; save cx
	push dx			; save dx
	push si			; save si
	push di			; save di
	push ds			; save ds
	push es			; save es
	mov bp,sp		; set bp to point to saved es

  L0:	mov ax,28[bp]		; ax = high-order word of 32-bit destination
	mov di,26[bp]		; di = low-order word of 32-bit	destination
	mov cx,4		; start	extracting click number	from dest
  L1:	rcr ax,1		; click	number is destination address /	16
	rcr di,1		; it is	used in	segment	register for copy
	loop L1			; 4 bits of high-order word are	used
	mov es,di		; es = destination click

	mov ax,24[bp]		; ax = high-order word of 32-bit source
	mov si,22[bp]		; si = low-order word of 32-bit	source
	mov cx,4		; start	extracting click number	from source
  L2:	rcr ax,1		; click	number is source address / 16
	rcr si,1		; it is	used in	segment	register for copy
	loop L2			; 4 bits of high-order word are	used
	mov ds,si		; ds = source click

	mov di,26[bp]		; di = low-order word of dest address
	and di,000Fh		; di = offset from paragraph  in es
	mov si,22[bp]		; si = low-order word of source	address
	and si,000Fh		; si = offset from paragraph  in ds

	mov dx,32[bp]		; dx = high-order word of byte count
	mov cx,30[bp]		; cx = low-order word of byte count

	test cx,8000h		; if bytes >= 32768, only do 32768
	jnz L3			; per iteration
	test dx,0FFFFh		; check	high-order 17 bits to see if bytes
	jnz L3			; if bytes >= 32768 then go to L3
	jmp short L4		; if bytes < 32768 then	go to L4
  L3:	mov cx,8000h		; 0x8000 is unsigned 32768
  L4:	mov ax,cx		; save actual count used in ax;	needed later

	test cx,0001h		; should we copy a byte	or a word at a time?
	jz L5			; jump if even
	rep movsb		; copy 1 byte at a time
	jmp short L6		; check	for more bytes

  L5:	shr cx,1		; word copy
	rep movsw		; copy 1 word at a time

  L6:	mov dx,32[bp]		; decr count, incr src & dst, iterate if needed
	mov cx,30[bp]		; dx ||	cx is 32-bit byte count
	xor bx,bx		; bx ||	ax is 32-bit actual count used
	sub cx,ax		; compute bytes	- actual count
	sbb dx,bx		; dx ;;	cx is  bytes not yet processed
	or cx,cx		; see if it is 0
	jnz L7			; if more bytes	then go	to L7
	or dx,dx		; keep testing
	jnz L7			; if loop done,	fall through

	pop es			; restore all the saved	registers
	pop ds			; restore ds
	pop di			; restore di
	pop si			; restore si
	pop dx			; restore dx
	pop cx			; restore cx
	pop bx			; restore bx
	pop ax			; restore ax
	pop bp			; restore bp
	popf			; restore flags
	ret			; return to caller

L7:	mov 32[bp],dx		; store	decremented byte count back in mem
	mov 30[bp],cx		; as a long
	add 26[bp],ax		; increment destination
	adc 28[bp],bx		; carry	from low-order word
	add 22[bp],ax		; increment source
	adc 24[bp],bx		; carry	from low-order word
	jmp L0			; start	next iteration


;===========================================================================
;				cp_mess
;===========================================================================
; This routine is makes	a fast copy of a message from anywhere in the address
; space	to anywhere else.  It also copies the source address provided as a
; parameter to the call	into the first word of the destination message.
; It is	called by:
;    cp_mess[src, src_clicks, src_offset, dst_clicks, dst_offset]
; where	all 5 parameters are shorts [16-bits].
;
; Note that the	message	size, 'Msize' is in WORDS [not bytes] and must be set
; correctly.  Changing the definition of message the type file and not changing
; it here will lead to total disaster.
; This routine destroys	ax.  It	preserves the other registers.

Msize =	12			; size of a message in 16-bit words
_cp_mess:
	push bp			; save bp
	push es			; save es
	push ds			; save ds
	mov bp,sp		; index	off bp because machine can't use sp
	pushf			; save flags
	cli			; disable interrupts
	push cx			; save cx
	push si			; save si
	push di			; save di

	mov ax,8[bp]		; ax = process number of sender
	mov di,16[bp]		; di = offset of destination buffer
	mov es,14[bp]		; es = clicks of destination
	mov si,12[bp]		; si = offset of source	message
	mov ds,10[bp]		; ds = clicks of source	message
	mov es:[di],ax		; copy sender's	process	number to dest message
	add si,2		; don't	copy first word
	add di,2		; don't	copy first word
	mov cx,Msize-1		; remember, first word doesn't count
	rep movsw		; iterate cx times to copy the message

	pop di			; restore di
	pop si			; restore si
	pop cx			; restore cs
	popf			; restore flags
	pop ds			; restore ds
	pop es			; restore es
	pop bp			; restore bp
	ret			; that's all folks!


;===========================================================================
;				port_out
;===========================================================================
; port_out(port, value)	writes 'value' on the I/O port 'port'.

_port_out:
	push bx			; save bx
	mov bx,sp		; index	off bx
	push ax			; save ax
	push dx			; save dx
	mov dx,4[bx]		; dx = port
	mov ax,6[bx]		; ax = value
	out dx,al		; output 1 byte
	pop dx			; restore dx
	pop ax			; restore ax
	pop bx			; restore bx
	ret			; return to caller


;===========================================================================
;				port_in
;===========================================================================
; port_in(port,	&value)	reads from port	'port' and puts	the result in 'value'.
_port_in:
	push bx			; save bx
	mov bx,sp		; index	off bx
	push ax			; save ax
	push dx			; save dx
	mov dx,4[bx]		; dx = port
	in  al,dx		; input	1 byte
	xor ah,ah		; clear	ah
	mov bx,6[bx]		; fetch	address	where byte is to go
	mov [bx],ax		; return byte to caller	in param
	pop dx			; restore dx
	pop ax			; restore ax
	pop bx			; restore bx
	ret			; return to caller


;===========================================================================
;				lock
;===========================================================================
; Disable CPU interrupts.
_lock:
	pushf			; save flags on	stack
	cli			; disable interrupts
	pop dgroup:lockvar		; save flags for possible restoration later
	ret			; return to caller


;===========================================================================
;				unlock
;===========================================================================
; Enable CPU interrupts.
_unlock:
	sti			; enable interrupts
	ret			; return to caller


;===========================================================================
;				restore
;===========================================================================
; Restore enable/disable bit to	the value it had before	last lock.
_restore:
	push dgroup:lockvar		; push flags as	they were before previous lock
	popf			; restore flags
	ret			; return to caller


;===========================================================================
;				build_sig
;===========================================================================
; Build	a structure that is pushed onto	the stack for signals.	It contains
; pc, psw, etc., and is	machine	dependent. The format is the same as generated
; by hardware interrupts, except that after the	"interrupt", the signal	number
; is also pushed.  The signal processing routine within	the user space first
; pops the signal number, to see which function	to call.  Then it calls	the
; function.  Finally, when the function	returns	to the low-level signal
; handling routine, control is passed back to where it was prior to the	signal
; by executing a return-from-interrupt instruction, hence the need for using
; the hardware generated interrupt format on the stack.	 The call is:
;     build_sig(sig_stuff, rp, sig)

; Offsets within proc table
PC    =	24
csreg =	18
PSW   =	28

_build_sig:
	push bp			; save bp
	mov bp,sp		; set bp to sp for accessing params
	push bx			; save bx
	push si			; save si
	mov bx,4[bp]		; bx points to sig_stuff
	mov si,6[bp]		; si points to proc table entry
	mov ax,8[bp]		; ax = signal number
	mov [bx],ax		; put signal number in sig_stuff
	mov ax,PC[si]		; ax = signalled process' PC
	mov 2[bx],ax		; put pc in sig_stuff
	mov ax,csreg[si]	; ax = signalled process' cs
	mov 4[bx],ax		; put cs in sig_stuff
	mov ax,PSW[si]		; ax = signalled process' PSW
	mov 6[bx],ax		; put psw in sig_stuff
	pop si			; restore si
	pop bx			; restore bx
	pop bp			; restore bp
	ret			; return to caller


	ifdef	NEEDCSV
	public csv, cret
;===========================================================================
;		csv & cret  (compiler generated	symbols)
;===========================================================================
; This version of csv replaces the standard one.  It checks for	stack overflow
; within the kernel in a simpler way than is usually done. cret	is standard.
csv	proc	near
	pop bx			; bx = return address
	push bp			; stack	old frame pointer
	mov bp,sp		; set new frame	pointer	to sp
	push di			; save di
	push si			; save si
	sub sp,ax		; ax =	bytes of local variables
	cmp sp,dgroup:_splimit	; has kernel stack grown too large
	jbe csv1		; if sp	is too low, panic
	jmp [bx]		; normal return: copy bx to program counter

csv1:
	mov dgroup:_splimit,0		; prevent call to panic	from aborting in csv
	mov bx,dgroup:_proc_ptr		; update rp->p_splimit
	mov WORD PTR 50[bx],0		; rp->sp_limit = 0
	push dgroup:_cur_proc		; task number
	mov ax,offset dgroup:stkoverrun; stack	overran	the kernel stack area
	push ax				; push first parameter
	call _panic		; call is: panic(stkoverrun, cur_proc)
	jmp csv1		; this should not be necessary

csv	endp


cret	proc	near

	lea	sp,-4[bp]	; set sp to point to saved si
	pop	si		; restore saved	si
	pop	di		; restore saved	di
	pop	bp		; restore bp
	ret			; end of procedure

cret	endp

	endif	; needcsv

;===========================================================================
;				get_chrome
;===========================================================================
; This routine calls the BIOS to find out if the display is monochrome or
; color.  The drivers are different, as	are the	video ram addresses, so	we
; need to know.
_get_chrome:
	int 11h			; call the BIOS	to get equipment type
	and al,30h		; isolate color/mono field
	cmp al,30h		; 0x30 is monochrome
	je getchr1		; if monochrome	then go	to getchr1
	mov ax,1		; color	= 1
	ret			; color	return
getchr1:xor ax,ax		; mono = 0
	ret			; monochrome return


;===========================================================================
;				vid_copy
;===========================================================================
; This routine takes a string of [character, attribute]	pairs and writes them
; onto the screen.  For	a color	display, the writing only takes	places during
; the vertical retrace interval, to avoid displaying garbage on	the screen.
; The call is:
;     vid_copy(buffer, videobase, offset, words)
; where
;     'buffer'	  is a pointer to the (character, attribute) pairs
;     'videobase' is 0xB800 for	color and 0xB000 for monochrome	displays
;     'offset'	  tells	where within video ram to copy the data
;     'words'	  tells	how many words to copy
; if buffer is zero, the fill character	(BLANK)	is used

BLANK =	0700h

_vid_copy:
	push bp			; we need bp to	access the parameters
	mov bp,sp		; set bp to sp for indexing
	push si			; save the registers
	push di			; save di
	push bx			; save bx
	push cx			; save cx
	push dx			; save dx
	push es			; save es
vid0:	mov si,4[bp]		; si = pointer to data to be copied
	mov di,8[bp]		; di = offset within video ram
	and di,dgroup:_vid_mask	; only 4K or 16K counts
	mov cx,10[bp]		; cx = word count for copy loop
	mov dx,03DAh		; prepare to see if color display is retracing

	mov bx,di		; see if copy will run off end of video ram
	add bx,cx		; compute where copy ends
	add bx,cx		; bx = last character copied + 1
	sub bx,dgroup:_vid_mask	; bx = # characters beyond end of video ram
	sub bx,1		; note: dec bx doesn't set flags properly
	jle vid1		; jump if no overrun
	sar bx,1		; bx = # words that don't fit in video ram
	sub cx,bx		; reduce count by overrun
	mov dgroup:tmp,cx		; save actual count used for later

vid1:	test dgroup:_color,1	; skip vertical	retrace	test if	display	is mono
	jz vid4			; if monochrome	then go	to vid.2

;  vid2:in			; with a color display,	you can	only copy to
;	test al,010q		; the video ram	during vertical	retrace, so
;	jnz vid2		; wait for start of retrace period.  Bit 3 of
vid3:   in al,dx		; 0x3DA	is set during retrace.	First wait
	test al,010q		; until	it is off (no retrace),	then wait
	jz vid3			; until	it comes on (start of retrace)

vid4:   pushf			; copying may now start; save the flags
	cli			; interrupts just get in the way: disable them
	mov es,6[bp]		; load es now: int routines may	ruin it

	cmp si,0		; si = 0 means blank the screen
	je vid7			; jmp for blanking
	; lock nop		; this is a trick for the IBM PC-simulator only
				; 'lock' indicates a video ram access
	rep movsw		; this is the copy loop

vid5:   popf			; restore flags
	cmp bx,0		; if bx < 0, then no overrun and we are done
	jle vid6		; jump if everything fit
	mov 10[bp],bx		; set up residual count
	mov word ptr 8[bp],0		; JER added size: start copying at base of video ram
	cmp word ptr 4[bp],0		; JER added size: NIL_PTR means store blanks
	je vid0			; go do it
	mov si,dgroup:tmp		; si = count of words copied
	add si,si		; si = count of bytes copied
	add 4[bp],si		; increment buffer pointer
	jmp vid0		; go copy some more

vid6:				; JER - not in original source (vid6 label) - hope it's right
	pop es			; restore registers
	pop dx			; restore dx
	pop cx			; restore cx
	pop bx			; restore bx
	pop di			; restore di
	pop si			; restore si
	pop bp			; restore bp
	ret			; return to caller

vid7:   mov ax,BLANK		; ax = blanking	character
	rep stosw		; blank	the screen
	jmp vid5		; done

;===========================================================================
;				get_byte
;===========================================================================
; This routine is used to fetch	a byte from anywhere in	memory.
; The call is:
;     c	= get_byte(seg,	off)
; where
;     'seg' is the value to put	in es
;     'off' is the offset from the es value
_get_byte:
	push bp			; save bp
	mov bp,sp		; we need to access parameters
	push es			; save es
	mov es,4[bp]		; load es with segment value
	mov bx,6[bp]		; load bx with offset from segment
	mov al,es:[bx]		; al = byte
	xor ah,ah		; ax = byte
	pop es			; restore es
	pop bp			; restore bp
	ret			; return to caller


;===========================================================================
;				reboot & dump
;===========================================================================
; This code reboots the	PC

rb_vect		dw	00000H	; hardware restart address
		dw	0FFFFH

; The following zeros out BIOS RAM (ROS on an IBM :-)) to clear any
; warmstart flags that may be present in a given BIOS.

zros	proc	near

	mov	cx,0100H	; 200H bytes / sizeof(word)
	mov	ax,0		; segment 0 & value to store
	mov	es,ax		; set es to low memory
	mov	di,0400H	; clear addresses 400H-5FFH
	cld			; autoincrement
	rep stosw		; zero out memory
	ret

zros	endp

; Restore the interrupt	vectors	in low core.
_resvec	proc	near

	cld
	mov cx,2*65
	mov si,offset dgroup:_vec_table
	xor di,di
	mov es,di
    rep	movsw
	ret

_resvec	endp

_reboot:
	cli			; disable interrupts
	mov al,20h
	out 20h,al		; re-enable interrupt controller
;;;;;	call _resvec		; restore the vectors in low core
;;;;;	int 19h			; reboot the PC
	call zros		; zero out low memory to force cold restart
	jmp dword ptr rb_vect

_wreboot:
	cli			; disable interrupts
	mov al,20h		; re-enable interrupt controller
	out 20h,al
	call _resvec		; restore the vectors in low core to call BIOS
	xor ax,ax		; wait for character before continuing
	int 16h			; get char
	call zros		; zero out low memory to force cold restart
;;;;;	int 19h			; reboot the PC
	jmp dword ptr rb_vect


	ifdef	GENERIC_FDISK


;===========================================================================
;				diskio - JER (copied from fsck1)
;===========================================================================

; diskio(RW, cyl, sector, head, #sectors, drv, sb, ib, trksiz)
;	 4    6     8      10      12     14   16  18    20
; Do not issue a BIOS call that crosses a track boundary
_diskio:
	push	bp
	mov	bp,sp
	push	si
	push	di
	mov	tmp1,0		; tmp1 = # sectors actually transferred
	mov	di,12[bp]	; di = # sectors to transfer
	mov	tmp2,di		; di = # sectors to transfer
d0:
	mov	ax,6[bp]	; cylinder
	mov	cl,ah		; cl = hi-order bits of cylinder
	ror	cl,1		; BIOS expects hi bits in a funny place
	ror	cl,1		; 
	mov	ch,al		; cx = sector # in BIOS format
	mov	dh,10[bp]	; dh = head
	and	cl,0C0H		; mask off any garbage bits
	or	cl,8[bp]	; cl = sector # in low 6 bits
	inc	cl		; BIOS counts sectors starting at 1
	mov	dl,14[bp]	; dl = drive code (0-3 or 0x80 - 0x81)
	or	dl,80H		; force "hard disk" bit on
	push	es		; set es with sb of buffer
	mov	bx,16[bp]
	mov	es,bx
	mov	bx,18[bp]	; bx = ib of buffer
	mov	ah,4[bp]	; ah = READING or WRITING
	add	ah,2		; BIOS codes are 2 and 3, not 0 and 1
	mov	al,12[bp]	; al = # sectors to transfer
	mov	tmp,ax		; save, al is # sectors to read/write
	hdcall			; call the hard disk BIOS
	pop	es		; restore es saved when setting sb above
;	cmp	ah,0		; ah!=0 means BIOS detected error (no, cy does)
;	jne	d2		; exit with error
	jc	d2fail		; if carry set, error occurred
	mov	ax,tmp		; fetch count of sectors transferred
	xor	ah,ah		; count is in ax
	add	tmp1,ax		; tmp1 accumulates sectors transferred
	mov	si,tmp1		; are we done yet?
	cmp	si,tmp2	
	je	d2ok		; jump if done
	inc	word ptr 8[bp]	; next time around, start 1 sector higher
	add	18[bp],200h	; move up in buffer by 512 bytes (ib 4 bits)
	jmp	d0
d2ok:
	xor	ah,ah		; indicate "read OK" to driver
d2fail:	
	xor	al,al		; move 1-byte BIOS error code into integer
	xchg	ah,al		;    return value for C caller
	pop	di
	pop	si
	pop	bp
	ret

;===========================================================================
;	hdisk_params - JER - get hard disk params (semi) legally
;===========================================================================
; hdisk_params(drive, pparams)
_hdisk_params	proc	near
	push	bp
	mov	bp,sp
	mov	dl,4[bp]	; drive number
	or	dl,80H		; indicate fixed disk
	mov	ah,08H		; request parameters
	push	es
	hdcall			; call the hard disk BIOS
	pop	es
	mov	bx,6[bp]	; near pointer to parameter block
	mov	2[bx],dh	; maximum head number
	mov	byte ptr 3[bx],0 ; 256 head disk?
	inc	word ptr 2[bx] 	; convert to number of heads
	mov	6[bx],cl	; number of sectors
	and	byte ptr 6[bx],3fH ; mask off cyl # bits
	mov	byte ptr 7[bx],0
	rol	cl,1		; get cyl # high order bits
	rol	cl,1
	and	cl,03H
	xchg	cl,ch		; put in proper order
	mov	0[bx],cx	; store in parameter block
	xor	dh,dh
	mov	4[bx],dx	; number of drives
	pop	bp		; done
	ret
_hdisk_params	endp

_win_init	proc	near
	push	bp
	mov	bp,sp
	mov	ah,00H		; BIOS reset
	mov	dl,80H
	push	es
	hdcall			; call the hard disk BIOS
	pop	es
	jc	wi0		; return error if carry set
	mov	ah,11H		; drive recalibrate
	mov	dl,80H
	push	es
	hdcall			; call hard disk BIOS
	pop	es
	jc	wi0		; return error if carry set
	xor	ax,ax		; return "OK"
wi0:	pop	bp
	ret
_win_init	endp


	endif	; GENERIC_FDISK


_TEXT	ENDS



_DATA	SEGMENT
lockvar	   DW	 0		; place	to store flags for lock()/restore()
tmp        DW    0		; count of bytes already copied
tmp1	   DW	 0
tmp2	   DW	 0

	ifdef ERSATZ_GENERIC
;
; The following vector is used to invoke the hard disk BIOS on the hard
; disk controller.  It should be possible to do this via an int 13, but
; so far I haven't gotten it to work... there's some code down in paragraph 70
; of DOS that seems to be causing this strange problem... I have to figure
; this out in detail, it doesn't make sense yet.  ...Now I think I have it...
;
disk_rom   DW	 00897H
	   DW	 0C800H
	endif

;
; The following are used by gn_wini to do an integrity check on the
; partition offsets -- they're here so that an errant program which
; destroys the "wini" table is less likely to destroy these too, although
; there's also a check on the integrity of these constants themselves
; against a constant in the instruction space of the winchester task.
;
_wn_low_safety	dd	0H
_wn_low_xor	dd	0H

;
; Here is where the original interrupt vectors get stored
;
_vec_table   DW	 130 dup(0)	; storage for interrupt	vectors
stkoverrun DB	 "Kernel stack overrun,	task = ",0
_DATA	ENDS

	END	; end of assembly
