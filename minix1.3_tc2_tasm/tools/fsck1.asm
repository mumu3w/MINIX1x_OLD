title  fsck1  -  additional stuff for fsck
page,132

TURBO equ 1
XDEBUG equ 0

INCLUDE model.inc


; Define STANDALONE if you need fsck to be part of Minix.
STANDALONE EQU  1
XTSECTORS  EQU  17
XTCYLS     EQU  68

GLOBAL $main:NEAR, _end:WORD, _exit:NEAR
GLOBAL _reset_diskette:NEAR, _diskio:NEAR, _getc:NEAR, _putc:NEAR, _dmaoverrun:NEAR
GLOBAL _main:NEAR,_cylsiz:word, _tracksiz:word, _drive:word


;---------------------------------------+
;               Code                    |
;---------------------------------------+
;
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP

$main:
	mov	dx,bx		; bootblok puts # sectors/track in bx
	xor	ax,ax
  mov bx,OFFSET DGROUP:edata          ; prepare to clear bss
  mov cx,OFFSET DGROUP:_end
	sub	cx,bx
	sar	cx,1
st1:mov	[bx],ax		; clear bss
	add	bx,2
	loop	st1

	mov	[DGROUP:_tracksiz],dx	; dx (was bx) is # sectors/track
	add	dx,dx
	mov	[DGROUP:_cylsiz],dx	; # sectors/cylinder
	mov	sp,OFFSET DGROUP:kerstack+STACKSIZE
	call	_main
	mov	bx,ax		; put scan code for '=' in bx
	cli
	mov	dx,60h
	mov	ds,dx
	mov	es,dx
	mov	ss,dx
        jmp     far ptr kernel		; direct jmp to kernel & start Minix
        IFDEF  _SID
        mov     ax,DGROUP               ; Force DOS-relocation (sep I&D)
        ENDIF

_exit:	mov	bx,[DGROUP:_tracksiz]
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


_putc:
        push    bp
        mov     bp,sp
        xor     ax,ax

        mov     ax,[bp+4]        ; al contains char to be printed
        mov     ah,14           ; 14 = print char
        mov     bl,1            ; foreground color
        push    bp              ; not preserved
        int     10h             ; call BIOS video-io
        pop     bp
        pop     bp
        ret

_getc:
        xor     ah,ah
        int     16h
        ret

_reset_diskette:                       ; reset_diskette
        xor     ax,ax
        push    es              ; not preserved
        int     13h             ; call BIOS DISKETTE_IO
        pop     es
        ret

; handle diskio(RW, sector_number, buffer, sector_count) call
; Do not issue a BIOS call that crosses a track boundary
_diskio:
	xor	ax,ax
	push	bp
	mov	bp,sp
	push	si
	push	di
	mov	[tmp1],0		; tmp1 = # sectors actually transferred
	mov	di,[bp+10]	; di = # sectors to transfer
	mov	[tmp2],di		; di = # sectors to transfer
d0:	mov	ax,[bp+6]	; ax = sector number to start at
	xor	dx,dx		; dx:ax is dividend
	div	[DGROUP:_cylsiz]	; ax = cylinder, dx = sector within cylinder
	mov	cl,ah		; cl = hi-order bits of cylinder
	ror	cl,1		; BIOS expects hi bits in a funny place
	ror	cl,1		; ditto
	mov	ch,al		; cx = sector # in BIOS format
	mov	ax,dx		; ax = sector offset within cylinder
	xor	dx,dx		; dx:ax is dividend
	div	[DGROUP:_tracksiz]	; ax = head, dx = sector
	mov	dh,al		; dh = head
	or	cl,dl		; cl = 2 high-order cyl bits || sector
	inc	cl		; BIOS counts sectors starting at 1

; _drive used to be declared as a byte here but as an int in fsck.c
; so now we have to use the int-declaration from fsck.c:
;
;	mov	dl,dgroup:_drive	

	mov	dl,[byte ptr DGROUP:_drive]	; dl = drive code (0-3 or 0x80 - 0x81)
	mov	bx,[bp+8]	; bx = address of buffer
	mov	al,cl		; al = sector #
	add	al,[bp+10]	; compute last sector
	dec	al		; al = last sector to transfer
	cmp	al,[byte ptr DGROUP:_tracksiz] ; see if last sector is on next track
	jle	d1		; jump if last sector is on this track
	mov	[word ptr bp+10],1    ; transfer 1 sector at a time
d1:	mov	ah,[bp+4]	; ah = READING or WRITING
	add	ah,2		; BIOS codes are 2 and 3, not 0 and 1
	mov	al,[bp+10]	; al = # sectors to transfer
	mov	[tmp],ax		; al is # sectors to read/write
	push	es		; BIOS ruins es
	int	13h		; issue BIOS call
	pop	es		; restore es
	cmp	ah,0		; ah != 0 means BIOS detected error
	jne	d2		; exit with error
	mov	ax,[tmp]		; fetch count of sectors transferred
	xor	ah,ah		; count is in ax
	add	[tmp1],ax		; tmp1 accumulates sectors transferred
	mov	si,[tmp1]		; are we done yet?
	cmp	si,[tmp2]		; ditto
	je	d2		; jump if done
	inc	[word ptr bp+6]	; next time around, start 1 sector higher
	add	[word ptr bp+8],200h	; move up in buffer by 512 bytes
	jmp	d0
d2:	
	pop	di
	pop	si
	pop	bp
	ret


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

SEGMENT _DATA		
tmp	DW 0
tmp1	DW 0
tmp2	DW 0
ENDS _DATA	

SEGMENT _DATAEND			; DATAEND holds nothing. The label tells us
edata:              ; where .data ends.
ENDS _DATAEND  

;---------------------------------------+
;  Set up memory lay-out for standalone |
;---------------------------------------+
;
STACKSIZE EQU 8192

SEGMENT LOWCORE	 AT 0            ; tell where BIOS-data etc is
	ORG 120
dskbase:
ENDS LOWCORE 

SEGMENT MINIX	 AT 60h          ; This is where Minix is loaded
kernel:		; absolute address 0000:1536d = 0060:0000h
ENDS MINIX	

SEGMENT	_BSS		; allocate the stack in .bss
kerstack DB STACKSIZE dup(?)
ENDS _BSS	

SEGMENT	_BSSEND  		; DATAV holds nothing. The label tells us
_end:              ; where .data+.bss ends (first free memory)
ENDS _BSSEND	

SEGMENT _STACK ; Satisfy DOS-linker
ENDS _STACK ; (dummy stack-segment)

        END     ; end of assembly-file

