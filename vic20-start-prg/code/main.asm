; ACME compiler syntax

;  V     V  i    CCCC    2222    000
;   V   V       C            2  0   0
;    V V    i   C          22   0   0 
;     V     i    CCCC    22222   000
;

		selected	= $a4
		row			= $a5


; zeropage usage: $a4-$ab
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
		JSR $E55F		; CLR SCR
		LDA #$08		; black border, black background
		STA $900F		; VIC
		LDA $9005		; VIC Char
		AND #$F0		; save upper nibble -> screen location, unchanged
		ORA #$0E 		; -> set char location to $1800
		STA $9005		; save reg
		LDX #$00		; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text0
		LDY #>text0
		JSR $CB1E
		LDX #20			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text1
		LDY #>text1
		JSR $CB1E

		LDA #$00		; clear
		STA selected	;  menu selection 
menu_0	LDA #$00		; clear
		STA row			;  init current row 

menu_a	LDX #4			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load0
		LDY #>menu_load0
		JSR printm3

		LDX #6			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load1
		LDY #>menu_load1
		JSR printm3

		LDX #8			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load2
		LDY #>menu_load2
		JSR printm3

		LDX #10			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load3
		LDY #>menu_load3
		JSR printm3

		LDX #12			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load4
		LDY #>menu_load4
		JSR printm3

		LDX #14			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load5
		LDY #>menu_load5
		JSR printm3

		LDX #16			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load6
		LDY #>menu_load6
		JSR printm3

		LDX #18			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR printm1
		LDA #<menu_load7
		LDY #>menu_load7
		JSR printm3

		LDX #21			; X+1 rows down
		LDY #$03		; y+1 cols right
		JSR printm1
		LDA selected
		ASL
		TAX
		LDA desc_jmp,X
		LDY desc_jmp+1,X
		JSR printm3

;	ldx #$00
;-	txa
;	sta $1100,X
;	INX
;	bne -

		JSR sound

getkey	LDA $CB 			; get key, $40 no key pressed
		CMP #$40
		BEQ getkey
		CMP #$1F 			; Cursor up / down
		BEQ getcrsr
		CMP #$0F 			; Return
		BEQ getret
		jmp getkey


getcrsr	
		LDA $028E 
		BEQ getcri
		LDX selected
		DEX
		BPL +
		LDX #$07
+		STX selected
		JMP menu_0 
getcri	LDX selected
		INX
		CPX #$08
		BNE +
		LDX #$00
+		STX selected
		JMP menu_0 

getret	JSR sound3	
		JMP portal			; CR / Enter -> load PRG via portal

printm1	JSR $E50C		; cursor pos X Y
		LDX row 
		CPX selected
		BNE printm2
		LDA #<text_ha		; highlight off
		LDY #>text_ha
		JSR $CB1E
		CLC 
		BCC +
printm2	LDA #$20
		JSR $FFD2 
+		RTS 

printm3	JSR $CB1E
		LDX row 
		CPX selected
		BNE +
		LDA #<text_he		; highlight off
		LDY #>text_he
		JSR $CB1E
+		LDA #$20
		JSR $FFD2 
		INC row 
		RTS 

sound	LDA $900E			; VIC volume
		ORA #$07			; medium
		STA $900E
		LDX #$FF 
sound2	STX $900C			; VIC sound
		LDY #$EE   
-		DEY 
		BNE -
		DEX
		CPX #$7E
		BNE sound2
		RTS 

sound3	LDX #225 
		STX $900C			; VIC sound
		LDY #$22   
-		JSR $EF96			; delay 1ms
		DEY 
		BNE -
		LDX #235 
		STX $900C			; VIC sound
		LDY #$22   
-		JSR $EF96			; delay 1ms
		DEY 
		BNE -
		LDX #240 
		STX $900C			; VIC sound
		LDY #$22   
-		JSR $EF96			; delay 1ms
		DEY 
		BNE -
		LDA $900E			; VIC volume
		AND #$F0			; off
		STA $900E
		RTS

text0	
		!by 31,18,232,233,233,233,233,233,233,234,32,32,32,32,32,32,32,32,32,32,220,221,222,223
		!by 31,224,156,225,159,226,227,228,158,229,230,31,231,32,32,32,"h","A","U","P","T","M","E","N","U","E",32
		!by 31,235,236,236,236,236,236,236,237,32,32,32,32,32,32,32,32,32,32,32,32,32,239,146
		!by 30,$00

text1
		!by 31,231,232,234,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219
		!by 31,18,32,32,146,237,0

text_ha	!by 158,18,240
		!by $00

text_he	!by 241,146
		!by 30,$00


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
    	sei						; disable IRQ
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
		cli						; enable IRQ
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
pull_end	cli
		rts		
		
wic64_pull_strt	
        sei			; init reading
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

portal		; load/return to portal-menue
		ldx #(read_0334_end-read_0334_start)
-		lda read_0334_start,x	; copy load routine in safe area
		sta safe_area,x
		dex
		bpl -
		ASL selected
		LDX selected
		LDA com_jmp+1,X 
		TAY 
		LDA com_jmp,X 
		jsr com_out		; send command to load portal
		jsr wic64_pull_strt	; init pull data
		tax			; x: counter low byte
		inc tmp			; +1 for faster check dec/bne
		jsr read_byte		; loadaddress low byte
		sta data_pointer
		jsr read_byte		; loadaddress high byte
		sta data_pointer+1	
		jmp safe_area		; load programm and run

read_0334_start
!pseudopc safe_area {
loop_read_0334	
handshake_0334	
        lda $911d
		nop
		nop
		nop
		nop 			; short delay
		and #$10        	; wait for NMI FLAG2
		beq handshake_0334
		lda $9110		; read byte from WiC64 (userport)
		PHA				; manually generate PA6 ACK pulse -> /PC2
		LDA $911F		; read IRA
		AND #$BF		; reset PA6
		STA $911F		; -> PA6 = low
		ORA #$40		; set PA6
		STA $911F		; -> PA6 = high
		PLA 
		sta (data_pointer),y
		iny
		bne +
		inc data_pointer+1
+		dex
		bne loop_read_0334	
		dec tmp
		bne loop_read_0334	; all bytes read?
		cli
		JSR $C659		; CHRGET pointer  start of program
		JSR $E518		; init VIC and clear screen
		JMP $C7AE		; RUN
}
read_0334_end


*=$1800  		; include font table 2k 
!bin "code/PXLfont88665B-RF2.3-VC20wic01c.bin"


*=$2000  		; include jump tables and menu data 

;com_start	!text "W",$20,$00,$01
;		!text "http://x.wic64.net/start.prg",0 ; url of portal

com_jmp		!by <com_load0, >com_load0, <com_load1, >com_load1
			!by <com_load2, >com_load2, <com_load3, >com_load3
			!by <com_load4, >com_load4, <com_load5, >com_load5
			!by <com_load6, >com_load6, <com_load7, >com_load7

desc_jmp	!by <desc_load0, >desc_load0, <desc_load1, >desc_load1
			!by <desc_load2, >desc_load2, <desc_load3, >desc_load3
			!by <desc_load4, >desc_load4, <desc_load5, >desc_load5
			!by <desc_load6, >desc_load6, <desc_load7, >desc_load7

menu_load0	!text "gAMES      16K",0
desc_load0	!text "sPIELE 16K+    ram",18,31,32,32,146,30,"      eRWEITERUNG",0
com_load0	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/games16k/start.prg",0 

menu_load1	!text "gAMES      32K",0
desc_load1	!text "sPIELE MIT        ",18,31,32,32,146,30,"   ram vOLLAUSBAU",0
com_load1	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/games32k/start.prg",0 

menu_load2	!text "dEMOS      8K",0
desc_load2	!text "dEMO-pROGRAMME    ",18,31,32,32,146,30,"   revision ETC. ",0
com_load2	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/demos08k/start.prg",0 

menu_load3	!text "aUDIO      16K",0
desc_load3	!text "sid-eMULATION     ",18,31,32,32,146,30," aUDIO victRACKER",0
com_load3	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/audio16k/start.prg",0 

menu_load4	!text "gRAPHICS   8K",0
desc_load4	!text "gRAFIK-dEMOS      ",18,31,32,32,146,30," pIXEL-aRT hIrES ",0
com_load4	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/graph08k/start.prg",0 

menu_load5	!text "tOOLS FUER vc20 8k",0
desc_load5	!text "cLOCKS, cONFIG ...",18,31,32,32,146,30," tEST-bEREICH    ",0
com_load5	!text "W",$3A,$00,$01
			!text "http://www.chris-straessle.de/files/tools08k/start.prg",0 

menu_load6	!text "res - cHAT",0
desc_load6	!text "cHAT rOOM wIc64   ",18,31,32,32,146,30," NOCH NICHT ready",0
com_load6	!text "W",$2B,$00,$01
			!text "http://www.chris-straessle.de/start.prg",0 

menu_load7	!text "res - mESSAGES",0
desc_load7	!text "nACHRICHTEN       ",18,31,32,32,146,30," NOCH NICHT ready",0
com_load7	!text "W",$2B,$00,$01
			!text "http://www.chris-straessle.de/start.prg",0 

