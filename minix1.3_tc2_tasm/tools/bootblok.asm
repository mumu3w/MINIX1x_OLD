;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  	Bootblok.asm
;
;       This is a standalone file.  It it not linked to anything.
;       It must be less than 512 bytes.  Since the code is used
;       as is, there is no reason to provide code that forces
;       relocation.  Just remember to strip the 512 block header
;       from bootblok.exe.  It should only be assembled with Combined
;       I & D.  
;
;
; Modified for TURBO C from bootblok.s 
; Deborah Mullen 2/89
;

INCLUDE model.h

; When the PC is powered on, it reads the first block from the floppy
; disk into address 0x7C00 and jumps to it.  This boot block must contain
; the boot program in this file.  The boot program first copies itself to
; address 192K - 512 (to get itself out of the way).  Then it loads the 
; operating system from the boot diskette into memory, and then jumps to fsck.
; Loading is not trivial because the PC is unable to read a track into
; memory across a 64K boundary, so the positioning of everything is critical.
; The number of sectors to load is contained at address 504 of this block.
; The value is put there by the build program after it has discovered how
; big the operating system is.  When the bootblok program is finished loading,
; it jumps indirectly to the program (fsck) which address is given by the
; last two words in the boot block. 
;
; Summary of the words patched into the boot block by build:
; Word at 504: # sectors to load
; Word at 506: # DS value for fsck
; Word at 508: # PC value for fsck
; Word at 510: # CS value for fsck
;
; This version of the boot block must be assembled without separate I & D
; space.

IFDEF SID
	DISPLAY "Separate I&D -- may not be a good idea here"
ENDIF  

LOADSEG = 0060h         ; here the boot block will start loading
BIOSSEG = 07C0h         ; here the boot block itself is loaded
BOOTSEG = 2FE0h         ; here it will copy itself (192K-512b)
DSKBASE = 120            ; 120 = 4 * 0x1E = ptr to disk parameters
JMPI = 0EAh		 ; opcode for jp inter-segment ( far jmp)

final   = 504
fsck_ds = 506
fsck_pc = 508
fsck_cs = 510


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Data Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _DATA

disksec: DW 1
pcpar:	DB	0DFh, 02h, 25, 2, 9, 2Ah, 0FFh, 50h, 0F6h, 1, 3 ; for pc
atpar:	DB	0DFh, 02h, 25, 2,15, 1Bh, 0FFh, 54h, 0F6h, 1, 8 ; for at

fderr:	DB "Read err Auto reboot."
	DB 0
greet:	DB "Greetings!! Dos created Minix 1.3 (1 serial)."
	DB 0
tracksiz: DW 15	; changed to 9 for ps and pc

; Don't forget that words 504 - 510 are filled in by build.  The regular
; code had better not get that far.

ENDS _DATA

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Code Segment
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SEGMENT _TEXT
ASSUME CS:_TEXT, DS:DGROUP

; copy bootblock to bootseg
        mov     ax,BIOSSEG
        mov     ds,ax
        xor     si,si           ; ds:si - original block
        mov     ax,BOOTSEG
        mov     es,ax
        xor     di,di           ; es:di - new block
        mov     cx,256         ; #  words to move
	rep	movsw		; copy loop
    
	
; start boot procedure
	DB JMPI			; far jump
	DW OFFSET start         ; use trick and precode instructions
	DW BOOTSEG
;	jmpi	start, BOOTSEG	; set cs to BOOTSEG

start:
	mov     dx,cs
        mov     ds,dx           ; set ds to cs
        xor     ax,ax
        mov     es,ax           ; set es to 0
        mov     ss,dx           ; set ss to cs i.e., stack in high core
        mov     sp,1536        ; initialize sp to high core

; print greeting
	mov	ax,2		; reset video
	int	10h

        mov     ax,0200h	; BIOS call in put cursor in ul corner
        xor     bx,bx
        xor     dx,dx
        int     10h
        mov     bx, OFFSET greet
        call    print

; Initialize disk parameters
; Try 1.2M diskette by trying to read sector 15

	xor	ax,ax
	mov	es,ax
	mov	dx,ds
	mov	ax, OFFSET atpar
	mov	[WORD PTR es:DSKBASE],ax
	mov	[WORD PTR es:DSKBASE+2],dx

	xor	ax,ax	; reset drive
	int	13h

	xor	ax,ax
	mov	es,ax
	mov	ax,0201h	; read sector, #sector = 1
	mov	bx,0600h	; es:bx buffer
	mov	cx,000Fh	; track 0, sector 15
	mov	dx,0000h	; drive 0, head 0
	int	13h
	jnb	L1

; Error. It wasn't 1.2M. Now set up for 720K

	mov	[WORD PTR tracksiz],9
	xor	ax,ax		; ps disk parameters are in ROM F01520
	mov	es,ax
	mov	ax,1520h
	mov	[WORD PTR es:DSKBASE],ax
	mov	ax,0F000h	
	mov	[WORD PTR es:DSKBASE+2],ax

; Try 720K by trying to read track 64.
; 360K has 40 tracks, 720 has 80 tracks.

	xor	ax,ax	; diskette reset
	int	13h
	mov	[WORD PTR tracksiz],9

	xor	ax,ax
	mov	es,ax
	mov	ax,0201h	; read sector, number of sectors is 1
	mov	bx,0600h	; es:bx buffer
	mov	cx,4001h	; track 64, sector 1
	mov	dx,0000h	; drive 0, head 0
	int	13h
	jnb	L1

; Error. It wasn't 720K either. Now set up for 360K

	xor	ax,ax
	mov	es,ax
	mov	dx,ds
	mov	ax, OFFSET pcpar
	mov	[WORD PTR es:DSKBASE],ax
	mov	[WORD PTR es:DSKBASE+2],dx
	xor	ax,ax		; diskette reset
	int	13h

L1:

; Load the operating system from diskette.
load:
	call	setreg		; set up ah, cx, dx
	mov	bx,[ WORD PTR disksec]	; bx = number of next sector to read
	add	bx,2		; diskette sector 1 goes at 1536 ("sector" 3)
	shl	bx,1		; multiply sector number by 32
	shl	bx,1		; ditto
	shl	bx,1		; ditto
	shl	bx,1		; ditto
	shl	bx,1		; ditto
	mov	es,bx		; core address is es:bx (with bx = 0)
	xor	bx,bx		; see above
	add	[WORD PTR disksec],ax	; ax tells how many sectors to read
	mov	ah,2		; opcode for read
	int	13h		; call the BIOS for a read
	jb	error		; jump on diskette error
	mov	ax,[WORD PTR disksec]	; see if we are done loading
	cmp	ax,[WORD PTR final]	; ditto
	jb	load		; jump if there is more to load

; Loading done.  Finish up.
        mov     dx,03F2h      ; kill the motor
        mov     ax,000Ch
        out	dx,al
        cli

	mov	bx,[WORD PTR tracksiz]	; fsck expects # sectors/track in bx
        mov     ax,[WORD PTR fsck_ds]      ; set segment registers
        mov     ds,ax           ; when sep I&D DS != CS
        mov     es,ax           ; otherwise they are the same.
        mov     ss,ax           ; words 504 - 510 are patched by build

; now jmp to fsck

	jmp   [DWORD PTR cs:fsck_pc]	; jmp to fsck

; Given the number of the next disk block to read, disksec, compute the
; cylinder, sector, head, and number of sectors to read as follows:
; ah = # sectors to read;  cl = sector #;  ch = cyl;  dh = head; dl = 0
setreg:
	mov	si,[WORD PTR tracksiz]	; 9 (PC) or 15 (AT) sectors per track
	mov 	ax,[WORD PTR disksec]	; ax = next sector to read
	xor	dx,dx		; dx:ax = 32-bit dividend
	div	si		; divide sector # by track size
	mov	cx,ax		; cx = track #; dx = sector (0-origin)
	mov	bx,dx		; bx = sector number (0-origin)
	mov	ax,[WORD PTR disksec]	; ax = next sector to read
	add	ax,si		; ax = last sector to read + 1
	dec	ax		; ax = last sector to read
	xor	dx,dx		; dx:ax = 32-bit dividend
	div	[WORD PTR tracksiz]	; divide last sector by track size
	cmp	al,cl		; is starting track = ending track
	je	set1		; jump if whole read on 1 cylinder
	sub	si,dx		; compute lower sector count
	dec	si		; si = # sectors to read

; Check to see if this read crosses a 64K boundary (128 sectors).
; Such calls must be avoided.  The BIOS gets them wrong.
set1:  
	mov	ax,[WORD PTR disksec]	; ax = next sector to read
	add	ax,2		; disk sector 1 goes in core sector 3
	mov	dx,ax		; dx = next sector to read
	add	dx,si		; dx = one sector beyond end of read
	dec	dx		; dx = last sector to read
	shl	ax,1		; ah = which 64K bank does read start at
	shl	dx,1		; dh = which 64K bank foes read end in
	cmp	ah,dh		; ah != dh means read crosses 64K boundary
	je	set2		; jump if no boundary crossed
	shr	dl,1		; dl = excess beyond 64K boundary
	xor	dh,dh		; dx = excess beyond 64K boundary
	sub	si,dx		; adjust si
	dec	si		; si = number of sectors to read

set2:	mov	ax,si		; ax = number of sectors to read
	xor	dx,dx		; dh = head, dl = drive
	mov	dh,cl		; dh = track
	and	dh,01h	; dh = head
	mov	ch,cl		; ch = track to read
	shr	ch,1		; ch = cylinder
	mov	cl,bl		; cl = sector number (0-origin)
	inc	cl		; cl = sector number (1-origin)
	xor	dl,dl		; dl = drive number (0)
	ret			; return values in ax, cx, dx

;-------------------------------+
;    error & print routines     ;
;-------------------------------+

error:
        push    ax
        mov     bx, OFFSET fderr
        call    print           ; print msg
	xor	cx,cx
err1:	mul	bl		; delay
	loop	err1
	int	19h


print:                          ; print string (bx)
        mov	al,[bx]	        ; al contains char to be printed
        test   al,al           ; null char?
        jne     prt1            ; no
        ret                     ; else return
prt1:   mov    ah,14          ; 14 = print char
        inc     bx              ; increment string pointer
        push    bx              ; save bx
        mov    bl,1           ; foreground color
	xor	bh,bh		; page 0
        int     10h            ; call BIOS VIDEO_IO
        pop     bx              ; restore bx
        jmp     print           ; next character

ENDS _TEXT

		END
