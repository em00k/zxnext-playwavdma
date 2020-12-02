;
; TBBlue / ZX Spectrum Next project
; Copyright (c) 2015 - Fabio Belavenuto & Victor Trucco
;
; All rights reserved
;
; Redistribution and use in source and synthezised forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; Redistributions of source code must retain the above copyright notice,
; this list of conditions and the following disclaimer.
;
; Redistributions in synthesized form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
;
; Neither the name of the author nor the names of other contributors may
; be used to endorse or promote products derived from this software without
; specific prior written permission.
;
; THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; You are responsible for any legal issues arising from your use of this code.
;
; v1.5 changes david saphier 
; 
	macro nextreg_a reg
		dw $92ed
		db reg
	endm
	
	macro MUL_DE
		dw $30ed
	endm

	macro getreg reg 
		ld bc,$243B			; Register Select 
		ld a,reg			; a = slot already so add $50 for slot regs 
		out(c),a			; 
		ld bc,$253B			; reg access 
		in a,(c)
	endm 

	device zxspectrum48

    org 2000h

;port 107=datagear / port 11=MB02+

Z80DMAPORT		equ 107

SPECDRUM        equ $df

BUFFER_SIZE     equ 2048

start1			di
				push iy  
				getreg $7			; get cpu speed 
				ld (oldspeed),a		; store for later 
				getreg $54			; get cpu speed 
				ld (oldbank4),a		; store for later 
				ld a,3 
				nextreg_a $7 		; turbo
				
				ld a,h:or l:jr nz,.gl	
				ld	hl,emptyline:call print_rst16:jr finish
.gl				ld	de,filename:ld b,64
.bl				ld	a,(hl):cp ":":jr z,.dn:or a:jr z,.dn:cp 13:jr z,.dn:bit 7,a:jr nz,.dn
				ld	(de),a:inc hl:inc de:djnz .bl
.dn				xor a:ld	(de),a


				ld (stack),sp 
				
				ld sp,$3ffe
				
				
				call setdrv							; init the mmc 
				; check for param 
				ld hl,filename 
				ld a,(hl)
				inc hl 
				cp '-'
				jr nz,nocommand 
				ld a,(hl)
				cp 's'
				inc hl 
				jr z,foundscaler 
				jr skipcommand
foundscaler: 	; get custom scaler value 
				; hl scaler address in text 
				inc hl 
				call ASCIItoAcc
				; now in A 
				inc hl
				push hl 
				pop ix 				
				ld (scaler),a 
				ld (sflag),a 
				jr skipcommand
nocommand:				
				ld ix,filename						; grab the filename 								
skipcommand: 
				call reservebank					; we need a bank @ 8000, get nextzxos give us one
				call play							; now jump to main routine 
quickexit:				
				call freebank						; we are done, free the bank 
				
				ld sp,(stack)						; stick the stack back to what it was 
				ld a,(oldbank4)							
				nextreg_a $54						; put back orginal bank 
				
finish

				ld a,(oldspeed)
				nextreg_a $7						; restore speed 
				pop iy								; saved to help running from another dotcommand
				xor a 
				ei
				ret

oldspeed:	
				db 0

oldbank4:		db 0 

print_rst16		ld a,(hl):inc hl:or a:ret z:rst 16:jr print_rst16

play	
				push de:call fopen:pop de:ret c:ld a,(handle):or a:ret z
				
				ld L, 0  							; 0 = Seek from start of file
				ld BC, 0
				ld DE, 22 							; skip the 44 bytes from header
				call fseek							; mono / stereo 

				ld ix, BufferWAV
				ld bc, 4
				; read the data from SD or finish with some error
				push bc:call fread:pop de:
				ld hl,BufferWAV
				
				ld a,(hl) 				; number of channels 1=mono 2=stereo 
				ld b,a 					; save it 
				ld a,(sflag) : or a : jr nz,dontincreasescaler
				inc hl 
				inc hl 
				inc hl 
				ld a,(hl)				; is it 32000  00 7d 
										; is it 16000  80 3e
				cp $70					; is A >= $7d?
				jr nc,skipscaler2		; yes jump to skipscaler we are at 32000+
				;' Hz / 615 = scaler 
				;13 32000 
				;26 16000
				;39 11025
				
				
				ld a,13*2				; might be 16000
				ld (scaler),a 			; 
skipscaler1:	
				ld a,(hl)				; lets chack 
				cp $2b					; is A >= $2b?
				jr nc,skipscaler2		; 
				ld a,13*3				; 11025
				ld (scaler),a 

skipscaler2:

				ld a,b 					; get channels 
				cp 1
				jr nz,dontincreasescaler 
				ld a,(scaler)
				add a,a 
				ld (scaler),a 
				
dontincreasescaler:
				ld L, 0  							; 0 = Seek from start of file
				ld BC, 0
				ld DE, 44 							;skip the 44 bytes from header
				call fseek

				; clear the buffer
				ld hl, BufferWAV
				ld ( HL ),$80
				ld de, BufferWAV + 1
				ld bc, BUFFER_SIZE
				ldir

				ld ix, BufferWAV
				ld bc, BUFFER_SIZE 
				call fread
				
				; transfer the DMA "program"
				
				ld hl,DMA 
				ld b,DMAEND-DMA
				ld c,Z80DMAPORT
				otir
loop1            
				; DMA read the bytes non-stop, so we need to make sure to not overwrite the audio data
				; now we are testing if the DMA is reading from the first part 

				ld c,Z80DMAPORT
waitforbuffer1  

				in a,(c)
                bit 2,a
				
                jr z,waitforbuffer1
	
				; the DMA is not reading from the first part, we can fill with the audio data
				ld ix, BufferWAV
				ld bc, BUFFER_SIZE / 2
				
				; read the data from SD or finish with some error
				push bc:call fread:pop de:jp c,ending
				ld	a,b:cp d:jp nz,ending:ld a,c:cp e:jp nz,ending
				
				; now we are testing if the DMA is reading from the second part 
				ld c,Z80DMAPORT
				
waitforbuffer2   
				
				in a,(c)
                bit 2,a
				
                jr nz,waitforbuffer2

				; the DMA is not reading from the second part, we can fill with the audio data
				ld ix, BufferWAV + ( BUFFER_SIZE / 2 )
				ld bc, BUFFER_SIZE / 2
				
				; read the data from SD or finish with some error
				push bc:call fread:pop de:jp c,ending
				ld	a,b:cp d:jp nz,ending:ld a,c:cp e:jp nz,ending

				;check the space key to abort

				ld bc,$7ffe   
				in a,(c)
				and 1
				
				jp nz,loop1						

ending            
				ld c,Z80DMAPORT
				LD A,#83 	; DISABLE DMA
				OUT (c),a

				call fclose ; close the file
				
				ret 		; return to BASIC

			
DMA		  		defb #C3			;R6-RESET DMA
				defb #C7			;R6-RESET PORT A Timing
				defb #CA			;R6-SET PORT B Timing same as PORT A

				defb #7D 			;R0-Transfer mode, A -> B
				defw BufferWAV	;R0-Port A, Start address
				defw BUFFER_SIZE	;R0-Block length

				defb $54 			;R1-Port A address incrementing, variable timing
				defb $2				;R1-Cycle length port B

				defb $68			;R2-Port B address fixed, variable timing
				defb $22			;R2-Cycle length port B 2T with pre-escaler
scaler:
				defb 13				;R2-Port B Pre-escaler
				
				;13 = 32000
				;32*2 16000
				
				defb #CD 			;R4-Burst mode
				defw SPECDRUM		;R4-Port B, Start address
				defb #A2			;R5-Restart on end of block, RDY active LOW
				defb #BB			;Read Mask Follows
				defb #10			;Mask - only port A hi byte
					  
				defb #CF			;R6-Load

				defb #B3			;R6-Force Ready			
				defb #87			;R6-Enable DMA
DMAEND			  
				
ASCIItoAcc:

	; hl = address of 8 bit ascii string 
	; a returns 8 bit value of string 
				;ld hl,data
; check lenght 
				push hl
				ld bc,0
checkloop:
				ld a,b : cp 4 : jp z,quickexit : ld a,(hl) : inc hl : inc b : cp 32 : jr nz,checkloop
				pop hl : ld a,b : sub 1 : cp 2 : jr z,twodigi : cp 1 : jr z,onedigi
hundreds:
				ld a,(hl) : inc hl : cp 0 : ret z 
				sub $30 : ld d,a : ld e,100 : MUL_DE : ld a,e : push af 
				jr twodigiskip
twodigi:
				ld de,0 : push de 
twodigiskip:
				ld a,(hl) : inc hl : cp 0 : ret z 
				sub $30 : ld d,a : ld e,10 : MUL_DE : ld a,e 
				jr tens
onedigi:
				ld de,0 : push de 
tens:
				ld a,(hl) : inc hl : cp 0 : ret z 
				sub $30 : add a,e : pop de : add a,d 
failedacc:		ret 			
reservebank
				ld hl,$0001  	; H=banktype (ZX=0, 1=MMC); L=reason (1=allocate)
				exx
				ld c,7 			; RAM 7 required for most IDEDOS calls
				ld de,$01bd 	; IDE_BANK
				rst $8:defb $94 ; M_P3DOS
				jp nc,anybank
				ld a,e 
				ld (bank),a 
				nextreg_a $54	; we will swap out memory with reserved bank
				ret 
freebank	
				ld hl,$0003  	; H=banktype (ZX=0, 1=MMC); L=reason (1=allocate)
				ld a,(bank)
				ld e,a
				exx
				ld c,7 			; RAM 7 required for most IDEDOS calls
				ld de,$01bd 	; IDE_BANK
				rst $8:defb $94 ; M_P3DOS
				ret 
			
anybank			ld a,33			
				nextreg_a $54
				ret
			
setdrv			xor a:rst $08:db $89:xor a:ld (handle),a:ret
fopen			ld	b,$01:db 33
fcreate			ld	b,$0c:push ix:pop hl:ld a,42:rst $08:db $9a:ld (handle),a:ret
fread			push ix:pop hl:db 62
handle			db	0:or a:ret z:rst $08:db $9d:ret
fwrite			push ix:pop hl:ld a,(handle):or a:ret z:rst $08:db $9e:ret
fclose			ld	a,(handle):or a:ret z:rst $08:db $9b:ret
fseek			ld a,(handle):rst $08:db $9f:ret;

filename		ds		64,0
emptyline		db		".playwav <file> to play a WAV",13
				db		".playwav -s <nr> <file>",13,13
				db		"Where <nr> is a scale value from",13
				db		"0-255, omit the <>",13
				db		" ",13
				db		"Sample rates supported:",13,13
				db		"32000hz 8bit stereo / mono PCM",13
				db		"16000hz 8bit stereo / mono PCM",13
				db		"11025hz 8bit stereo / mono PCM",13,13
				db		"Playback rate is auto detected.",13
				db		"Other rates can be played, use",13
				db		"the -s option to adjust speed.",13,13
				db		"v1.7 12/9/2020 emk",13,00
				db 		"original code VTrucco&FBelavenuto, remixed David Saphier 2019/emook"
BufferWAV   	equ $8000
stack 			dw 0000 
bank			db 0 
sflag			db 0 
.last
	savebin "h:\dot\playwav",start1,.last-start1

;-------------------------------
