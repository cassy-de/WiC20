; ACME compiler syntax

;  V     V  i    CCCC    2222    000
;   V   V       C            2  0   0
;    V V    i   C          22   0   0 
;     V     i    CCCC    22222   000
;  cassy.de 
; 
;   ffmpeg -i sine.wav -f u8 -ac 1 sine2.wav
;	ffmpeg -i Nova\ Vivo\ 2021.mp3 -ar 15625 -ac 1 -f u8 novavivo.raw

		bank	= $A4
		row			= $a5
		fill_ptr = $9B ; $9B/$9C pointer to fill audio loop buffer
		audio_ptr = $9E ; $9E/$9F pointer to audio loop data


; zeropage usage: $9E-$9F, $A4-$AB
;*=$1001  		; BASIC Start VC20 unexpanded
*=$1201  		; BASIC Start VC20 + 8K .. 32k

		!wo eob    		; Calculate pointer to end of BASIC program
		!wo 2021   		; BASIC line number
		!by $9e    		; BASIC token for SYS 
;		!text "4128" 	; ML start address VC20 unexpanded
		!text "4640" 	; ML start address VC20 + 8K .. 20K
		!by $00 		; End of BASIC line 
eob		!by $00, $00	; The next BASIC line would start here 

;*=$1020  				; ML Program Start VC20 unexpanded
*=$1220  				; ML Program Start VC20 RAM expanded
startprg
		JSR $E55F		; CLR SCR
		LDA #$08		; black border, black background
		STA $900F		; VIC
;		LDA $9005		; VIC Char
;		AND #$F0		; save upper nibble -> screen location, unchanged
;		ORA #$0E 		; -> set char location to $1800
;		STA $9005		; save reg
		LDX #$01		; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text0
		LDY #>text0
		JSR $CB1E
		LDX #22			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text1
		LDY #>text1
		JSR $CB1E
		LDX #197	
-		LDA text3,X
		STA $1108,X 
;		LDA #$3
;		STA 
		DEX
		BNE -

;clear_audio_buffer
;		LDA #$00
;		STA fill_ptr 		; store low byte of sound data pointer
;		LDA #$20
;		STA fill_ptr+1 		; high byte of sound data pointer
;-		LDY #$00
;--		LDA #$00
;;		STA (fill_ptr),Y
;		INC fill_ptr 	; low byte	
;		BNE --
;		INC fill_ptr+1 
;		LDA fill_ptr+1 
;		CMP #$60	; $2000 - $5FFF -> OK
;		BNE -

;initirq
;		SEI 
;		LDA #<irqrt		; set new IRQ vector address
;		STA $0314 		; to JMP table , low byte
;		LDA #>irqrt
;		STA $0315 		; and high byte
;		LDA #$8A		; set new IRQ frequency
;		STA $9124		; timer 1 low byte
;		LDA #$00
;	LDA #$01
;		STA $9125		; timer 1 high byte
;		LDA #$00
;		STA audio_ptr 		; store low byte of sound data pointer
;		LDA #$20
;		STA audio_ptr+1 		; high byte of sound data pointer
;		CLI 

initwic
;	inc $1100
		LDA com_jmp+1 
		TAY 
		LDA com_jmp 
		JSR com_out			; send command to WiC
;	inc $1100
		JSR wic64_pull_strt	; init pull data
		TAX					; x: counter low byte
		INC tmp				; +1 for faster check dec/bne
;		LDA #$20			; init bank address to first fill of buffer
;		STA bank
;	inc $1100
;		LDA #$00
;		STA fill_ptr 		; store low byte of sound data pointer
;		LDA #$20
;		STA fill_ptr+1 		; high byte of sound data pointer
;-		LDY #$00
;--		JSR read_byte		; fetch audio stream data from wic
;		LSR					; upper nibble to play music
;		LSR
;		LSR
;		LSR
;		STA (fill_ptr),Y
;		INC fill_ptr 	; low byte	
;		BNE --
;		INC fill_ptr+1 
;		LDA fill_ptr+1 
;		CMP #$60	; $2000 - $5FFF -> OK
;		BNE -

;-		JSR read_byte		; fetch data from WiC
;		NOP 				; 
;		DEX
;		BNE -
;		DEY
;		CPY #$FF
;		BNE -
;		RTS



mainlp

		LDA $9003			; bit #7 is LSB of raster scan lines
-		CMP $9003			; has changed? next scan line -> 64uS -> 15635 Hz PAL
		BEQ -				; not changed? go back and wait

;	LDA $9004
;	AND #$04
;	ASL
;	STA $900E
;
		JSR read_byte		; fetch audio stream data from wic
		LSR 
		LSR 
		LSR 
		LSR 
		STA $900E
		AND #$07
		ORA #$08
		STA $900F

;		LDA $9004
;		CMP #$20
;		BPL +
;		LDA #$0D
;		STA $900F
;		JMP mainlp
;+		CMP #$88
;		BPL +
;		LDA #$08
;		STA $900F
;		JMP mainlp
;+		LDA #$0D
;		STA $900F

		DEX
		BNE mainlp
		DEC tmp
		BNE mainlp	; all bytes read?


		JMP initwic


; ----------------------------------------------------------------------

;irqrt	; all registers already saved
;		LDY #$00
;		LDA (audio_ptr),Y
;		LSR
;		LSR
;		LSR
;		LSR
;		STA $900E
;		INC audio_ptr 	; low byte	
;		BNE +
;		INC audio_ptr+1 
;		LDA audio_ptr+1 
;		CMP #$60	; $2000 - $5FFF -> OK
;		BNE +
;		LDA #$20	; reset to $2000
;		STA audio_ptr+1 
;+		JMP $EB15	; return from IRQ

; ----------------------------------------------------------------------


text0	
		!text 30," WIC20       CASSY-DE ",$0D 
		!text 31,"  STREAM PLAYER DEMO",$0 

text1	
		!text 5,"  - PROGRESSIVE MIX - ",$0 

;		!by 31,18,232,233,233,233,233,233,233,234,32,32,32,32,32,32,32,32,32,32,220,221,222,223
;		!by 31,224,156,225,159,226,227,228,158,229,230,31,231,"s","T","R","E","A","M"," ","p","L","A","Y","E","R",32
;		!by 31,235,236,236,236,236,236,236,237,32,32,32,32,32,32,32,32,32,32,32,32,32,239,146
;		!by 30,$00
;
;text2
;		!by 31,231,232,234,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219
;		!by 31,18,32,32,146,237,30
;		!text "",$0D
;		!text " ",5,0


text3
		!by 0,160,223,233,160, 32,233,160,223, 32,160,160,223, 32, 32,160, 32,233,160,160,160, 32, 32
		!by 160, 95,105,160,233,105, 32, 95,223, 32, 32, 95,223, 32,160, 32,160, 32, 32, 32, 32, 32
		!by 160, 32, 32,160,160, 32, 81, 32,160, 32, 32, 32,160, 32,160, 32, 95,160,160,160,223, 32
		!by 160, 32, 32,160,160, 32, 32, 32,160, 32, 32,233,105, 32,160, 32, 32, 32, 32, 32,160, 32
		!by 160, 32, 32,160,160, 32, 32, 32,160,160,160,105, 32, 32,160,160,160,160,160,160,105, 32
		!by  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
		!by  13,  1,  4,  9, 19, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
		!by  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
		!by  32,  4,  5, 19,  5, 18, 20, 32, 15,  6, 32, 12, 15, 19, 20, 32, 19, 15, 21, 12, 19


; ----------------------------------------------------------------------

		tmp2        = $a5
		tmp	        = $a6
		
		data_pointer = $a7 ; $a7/$a8 adress for data
		bytes_send  = $a9 ; $a9/$aa number of bytes
		
		z_timeout   = $ab ; flag for timeout (0 = no timeout)
		z_error	    = z_timeout	; (> 0 means timeout)
		
		safe_area   = $0334 	; for load-routine

		
wic64_ESP_read				; set WiC64 to 'read-mode'
wic64_exit			
		LDA $911C		; PCR 
		AND #$0F		; clear upper nibble 
		ORA #$E0		; CB2 = high, CB1 falling edge
		STA $911C		; -> ESP read mode, WiC green LED on
		LDA $911F		; ORA
		ORA #$40		; PA6 -> high
		STA $911F		; -> Data ACK to ESP inactive
		LDA $9113		; DDRA
		ORA #$40		; PA6 -> OUTPUT
		STA $9113
		LDA #$FF		; DDRB -> OUTPUT
		STA $9112
		rts	
				
com_out	
    	sta data_pointer	; set datapointer to lowbyte=A, highbyte=Y
		sty data_pointer+1
wic64_push
;    	sei						; disable IRQ
		jsr wic64_ESP_read	
		ldy #$02		
		lda (data_pointer),y	; number of bytes to send (lowbyte)
		sta bytes_send+1
		dey
		lda (data_pointer),y	; number of bytes to send (highbyte)
		tax
		beq +					; special case:		lowbyte=0
		inc bytes_send+1
+		dey						; y=0		
loop_send	lda (data_pointer),y	
		jsr write_byte			; send bytes to WiC64 in loop
		iny
		bne +
		inc data_pointer+1	
+		dex
		bne loop_send
		dec bytes_send+1
		bne loop_send				
;		cli						; enable IRQ
		rts
		
wic64_pull
    	jsr wic64_pull_strt	; init retrieving data from WiC64
		bne nonull			; check for length lobyte=0
		cmp tmp				; length highbyte=0?
		beq pull_end		; no bytes 
nonull
		tax				; x: counter lowbyte
		beq loop_read		; special case lobyte=0?
		inc tmp			; +1 for faster check with dec/bne
loop_read
		jsr read_byte		;read byte
		sta (data_pointer),y
		iny
		bne +
		inc data_pointer+1
+		dex
		bne loop_read		
		dec tmp
		bne loop_read		; all bytes?
pull_end
;		cli
		rts		
		
wic64_pull_strt	
;        sei			; init reading
		ldy #$00		; set port B to input
		sty $9112		
		LDA $911C		; PCR 
		AND #$0F		; clear upper nibble 
		ORA #$C0		; CB2 = low, CB1 falling edge
		STA $911C		; -> ESP send mode, ViC20 reading data, WiC green LED off
		jsr read_byte		; dummy byte for triggering ESP IRQ
		jsr read_byte		; data length high byte
		sta bytes_send+1
		sta tmp			; counter high byte
		jsr read_byte		; data length low byte
		sta bytes_send+0	
		rts

pa6ack					; manually generate PA6 ACK pulse -> /PC2
		LDA $911F		; read IRA
		AND #$BF		; reset PA6
		STA $911F		; -> PA6 = low
		ORA #$40		; set PA6
		STA $911F		; -> PA6 = high
		RTS 

wait_handshake	
        lda #$00		; handshake with and without timeout
		sta tmp2		; looplength for timeout: 256
-		LDA $911D		; IFR
		AND #$10		; check for CB1
		bne hs_rts 		; handshake ok -> no error, rts
		lda z_timeout	; timeout activated?
		beq -			; if not, infinite loop
		dec tmp2		; else count down
		bne -
		lda #$ff		; timeout occurred!
		bne +
hs_rts	lda #$00
+		sta z_error		; $00=OK, $ff=timeout
		RTS

write_byte	
        STA $9110       ; bits 0..7 parallel to WiC64 (userport PB 0-7)
		JSR pa6ack		; generate ACK to ESP
		JSR wait_handshake
        LDA $9110       ; read ORB to clear IFR flag CB1
		RTS 

read_byte	
        jsr wait_handshake
		lda $9110		; read byte from WiC64 and clear IFR flag CB1
		PHA				; save data
		JSR pa6ack		; generate ACK to ESP
		PLA				; restore data
		rts


;wiccmd	jsr com_out			; send command to WiC
;		jsr wic64_pull_strt	; init pull data
;		LDA bytes_send+1	; get length of string
;		TAY
;		LDA bytes_send	
;		TAX 
;-		JSR read_byte		; fetch data from WiC
;		NOP 				; 
;		DEX
;		BNE -
;		DEY
;		CPY #$FF
;		BNE -
;		RTS

; ---------------------------------------------


;portal		; load/return to portal-menue
;		ldx #(read_0334_end-read_0334_start)
;-		lda read_0334_start,x	; copy load routine in safe area
;		sta safe_area,x
;		dex
;		bpl -
;		LDX #$00
;		LDA com_jmp+1,X 
;		TAY 
;		LDA com_jmp,X 
;		jsr com_out		; send command to load portal
;		jsr wic64_pull_strt	; init pull data
;		tax			; x: counter low byte
;		inc tmp			; +1 for faster check dec/bne
;		jsr read_byte		; loadaddress low byte
;		sta data_pointer
;		jsr read_byte		; loadaddress high byte
;		sta data_pointer+1	
;		jmp safe_area		; load programm and run
;
;read_0334_start
;!pseudopc safe_area {
;loop_read_0334	
;handshake_0334	
;        lda $911d
;		nop
;		nop
;		nop
;		nop 			; short delay
;		and #$10        	; wait for NMI FLAG2
;		beq handshake_0334
;		lda $9110		; read byte from WiC64 (userport)
;		PHA				; manually generate PA6 ACK pulse -> /PC2
;		LDA $911F		; read IRA
;		AND #$BF		; reset PA6
;		STA $911F		; -> PA6 = low
;		ORA #$40		; set PA6
;		STA $911F		; -> PA6 = high
;		PLA 
;		sta (data_pointer),y
;		iny
;		bne +
;		inc data_pointer+1
;+		dex
;		bne loop_read_0334	
;		dec tmp
;		bne loop_read_0334	; all bytes read?
;		cli
;		JSR $C659		; CHRGET pointer  start of program
;		JSR $E518		; init VIC and clear screen
;		JMP $C7AE		; RUN
;}
;read_0334_end

com_jmp		!by <com_load0, >com_load0, <com_load1, >com_load1

;com_load0	!text "W",$1E,$00,$01		
;			!text "http://10.0.1.11/madis.raw",0 
com_load0	!text "W",$2B,$00,$01
			!text "http://www.chris-straessle.de/madis.raw",0 
com_load1	!text "W",$2B,$00,$01
			!text "http://www.chris-straessle.de/start.prg",0 




;*=$1800  		; include font table 2k 
;!bin "code/PXLfont88665B-RF2.3-VC20wic01c.bin"


;*=$2000  		;  
; !bin "code/sine.raw"
;!bin "code/002.raw"

;eof		!by 0,0,0


