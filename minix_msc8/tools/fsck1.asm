title  fsck1  -  additional stuff for fsck
page,132

INCLUDE ..\lib\prologue.h      


; Define STANDALONE if you need fsck to be part of Minix.
STANDALONE EQU  1
XTSECTORS  EQU  17
XTCYLS     EQU  68
IFDEF STANDALONE
  PUBLIC $main, _end, _edata, _exit, _kerstack, _prt
  EXTRN  _main:near
 ;EXTRN  _cinit:near, _debug:near
ENDIF
PUBLIC _reset_diskette, _diskio, _getc, _putc, _dmaoverrun
EXTRN _cylsiz:word, _tracksiz:word, _drive:byte

PUBLIC __acrtused
__acrtused	EQU	9876H

;-------------------------------------------+
; DATA - JER - moved here from before stack |
;-------------------------------------------+

_DATA	SEGMENT
magic	DW	0ABCDH
tmp	DW 0
tmp1	DW 0
tmp2	DW 0
fsckmsg DB "arrived at fsck1",0
_DATA	ENDS


IFDEF STANDALONE
;---------------------------------------+
;  Set up memory lay-out for standalone |
;---------------------------------------+
;
STACKSIZE EQU 8192

_DATA	SEGMENT
_brksa	DW	?
_DATA	ENDS

LOWCORE SEGMENT AT 0            ; tell where BIOS-data etc is
	ORG 120
dskbase label word
LOWCORE ENDS

MINIX	SEGMENT AT 60h          ; This is where Minix is loaded
_kernel  label byte		; absolute address 0000:1536d = 0060:0000h
MINIX	ENDS

_DATAT  SEGMENT			; DATAT holds nothing. The label tells us
_edata   label byte              ; where .data ends.
_DATAT  ENDS

_DATAU   SEGMENT		; allocate the stack in .bss
_kerstack DB STACKSIZE dup(?)
_DATAU   ENDS

_DATAV  SEGMENT			; DATAV holds nothing. The label tells us
_end    label byte              ; where .data+.bss ends (first free memory)
_DATAV  ENDS


;---------------------------------------+
;               Code                    |
;---------------------------------------+
;
_TEXT   SEGMENT
        assume  cs:_text,ds:dgroup

IFDEF   STANDALONE
$main:
	mov	dx,bx		; bootblok puts # sectors/track in bx
	xor	ax,ax
        mov     bx,offset dgroup:_edata          ; prepare to clear bss
        mov     cx,offset dgroup:_end
	sub	cx,bx
	sar	cx,1
st1: mov	[bx],ax		; clear bss
	add	bx,2
	loop	st1

	mov	dgroup:_tracksiz,dx	; dx (was bx) is # sectors/track
	add	dx,dx
	mov	dgroup:_cylsiz,dx	; # sectors/cylinder
	mov	sp,offset dgroup:_kerstack+STACKSIZE
	

;	call	cinit
;	mov	ax,offset fsckmsg
;	push	ax
;	call	debug
;	pop	ax

	call	_main

	mov	bx,ax		; put scan code for '=' in bx

	ifdef	debug
	mov	ax,60H
	mov	es,ax
	mov	ax,word ptr es:4
	call 	prtax
	wto	'= DS word at kernel entry point'
	endif

	cli
	mov	dx,60h
	mov	ds,dx
	mov	es,dx
	mov	ss,dx


        jmp     far ptr _kernel		; direct jmp to kernel & start Minix
        mov     ax,DGROUP               ; Force DOS-relocation (sep I&D)

_exit:	mov	bx,dgroup:_tracksiz
	jmp	$main


_prt:
	push	bp
	mov	bp,sp
	push	bx
	mov	bx,[bp+4]
	call	print
	pop	bx
	pop	bp
	ret
	
print:                          ; print string (bx), ax destroyed
        mov     al,[bx]         ; al contains char to be printed
        test    al,al           ; null char?
        jne     prt1            ; no
        ret                     ; else return
prt1:   mov     ah,14           ; 14 = print char
        inc     bx              ; increment string pointer
        push    bx              ; save bx
        mov     bl,1            ; foreground color
	xor	bh,bh		; page 0
        int     10h             ; call BIOS VIDEO_IO
        pop     bx              ; restore bx
        jmp     print           ; next character

;
;	code for debug messages
;
	ifdef	debug
;
; prtnum - print low-order 4 bits of al register in hex
;

prtnum	proc	near
	push	ds
	push	cs
	pop	ds
	push	bx
	mov	bx,offset xltab
	xlatb
	mov	ah,14
	mov	bh,10
	int	10H
	pop	bx
	pop	ds
	ret

xltab	db	'0123456789ABCDEF '
prtnum	endp

;
; prtax - print ax register's contents in hex
;

prtax	proc	near
	push	cx
	push	ax
	mov	al,ah
	mov	cl,4
	shr	al,cl
	call	prtnum
	pop	ax
	push	ax
	mov	al,ah
	and	al,0fH
	call	prtnum
	pop	ax
	push	ax
	mov	cl,4
	shr	al,cl
	call	prtnum
	pop	ax
	push	ax
	and	al,0fH
	call	prtnum
	mov	al,10H		; special value to print a space
	call	prtnum
	pop	ax
	pop	cx
	ret

prtax	endp

	endif
    
ENDIF   ; /* STANDALONE */


_putc:
        xor     ax,ax
        call    csv
        mov     al,4[bp]        ; al contains char to be printed
        mov     ah,14           ; 14 = print char
        mov     bl,1            ; foreground color
        push    bp              ; not preserved
        int     10h             ; call BIOS video-io
        pop     bp
        jmp     cret

_getc:
        xor     ah,ah
        int     16h
        ret

_reset_diskette:                       ; reset_diskette
        xor     ax,ax
        call    csv
        push    es              ; not preserved
        int     13h             ; call BIOS DISKETTE_IO
        pop     es
        jmp     cret

; handle diskio(RW, sector_number, buffer, sector_count) call
; Do not issue a BIOS call that crosses a track boundary
_diskio:
	xor	ax,ax
	call	csv
	mov	tmp1,0		; tmp1 = # sectors actually transferred
	mov	di,10[bp]	; di = # sectors to transfer
	mov	tmp2,di		; di = # sectors to transfer
d0:	mov	ax,6[bp]	; ax = sector number to start at
	xor	dx,dx		; dx:ax is dividend
	div	dgroup:_cylsiz	; ax = cylinder, dx = sector within cylinder
	mov	cl,ah		; cl = hi-order bits of cylinder
	ror	cl,1		; BIOS expects hi bits in a funny place
	ror	cl,1		; ditto
	mov	ch,al		; cx = sector # in BIOS format
	mov	ax,dx		; ax = sector offset within cylinder
	xor	dx,dx		; dx:ax is dividend
	div	dgroup:_tracksiz	; ax = head, dx = sector
	mov	dh,al		; dh = head
	or	cl,dl		; cl = 2 high-order cyl bits || sector
	inc	cl		; BIOS counts sectors starting at 1
	mov	dl,dgroup:_drive	; dl = drive code (0-3 or 0x80 - 0x81)
	mov	bx,8[bp]	; bx = address of buffer
	mov	al,cl		; al = sector #
	add	al,10[bp]	; compute last sector
	dec	al		; al = last sector to transfer
	cmp	al,byte ptr dgroup:_tracksiz ; see if last sector is on next track
	jle	d1		; jump if last sector is on this track
	mov	word ptr 10[bp],1    ; transfer 1 sector at a time
d1:	mov	ah,4[bp]	; ah = READING or WRITING
	add	ah,2		; BIOS codes are 2 and 3, not 0 and 1
	mov	al,10[bp]	; al = # sectors to transfer
	mov	tmp,ax		; al is # sectors to read/write
	push	es		; BIOS ruins es
	int	13h		; issue BIOS call
	pop	es		; restore es
	cmp	ah,0		; ah != 0 means BIOS detected error
	jne	d2		; exit with error
	mov	ax,tmp		; fetch count of sectors transferred
	xor	ah,ah		; count is in ax
	add	tmp1,ax		; tmp1 accumulates sectors transferred
	mov	si,tmp1		; are we done yet?
	cmp	si,tmp2		; ditto
	je	d2		; jump if done
	inc	word ptr 6[bp]	; next time around, start 1 sector higher
	add	8[bp],200h	; move up in buffer by 512 bytes
	jmp	d0
d2:	jmp	cret


_dmaoverrun:                       ; test if &buffer causes a DMA overrun
        push    bp
        mov     bp,sp
        push    bx
        push    cx

        mov     ax,ds
        mov     cl,4
        shl     ax,cl
        mov     bx,[bp+4]
        add     ax,bx   ; ax has absolute addres modulo 64K
        add     ax,1023 ; add transfer size - 1
        mov     ax,0
        jnc     ok
        inc     ax      ; indicate error
ok:
        pop     cx
        pop     bx
        pop     bp
        ret

csv:
        pop     bx
        push    bp
        mov     bp,sp
        push    di
        push    si
        sub     sp,ax
        jmp     bx

cret:
        lea     sp,-4[bp]
        pop     si
        pop     di
        pop     bp
        ret

_TEXT   ENDS

@STACK  SEGMENT BYTE STACK 'STACK' ; Satisfy DOS-linker
@STACK  ENDS			   ; (dummy stack-segment)

ENDIF

        END     ; end of assembly-file

