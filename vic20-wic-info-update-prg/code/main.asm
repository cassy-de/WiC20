; ACME compiler syntax

;  V     V  i    CCCC    2222    000
;   V   V       C            2  0   0
;    V V    i   C          22   0   0 
;     V     i    CCCC    22222   000
;  cassy.de 

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
startprg
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
		LDX #5			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text1
		LDY #>text1
		JSR $CB1E
		LDX #20			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text2
		LDY #>text2
		JSR $CB1E

		LDX #5			; X+1 rows down
		LDY #8			; y+1 cols right
		JSR $E50C		; cursor pos X Y
		JSR getfwv

		LDX #6			; X+1 rows down
		LDY #1			; y+1 cols right
		JSR $E50C		; cursor pos X Y
		JSR getfwd

		LDX #21			; X+1 rows down
		LDY #6			; y+1 cols right
		JSR $E50C		; cursor pos X Y
		JSR getip

		LDX #22			; X+1 rows down
		LDY #6			; y+1 cols right
		JSR $E50C		; cursor pos X Y
		JSR getssid

		LDX #08			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		JSR getavail	; fw update available ?
		CMP #2			; DEV ?
		BNE +
		LDA #<text5
		LDY #>text5
		JSR $CB1E
		CLC 
		BCC ++
+		CMP #1			; STD ?
		BNE +
		LDA #<text4
		LDY #>text4
		JSR $CB1E
		CLC 
		BCC ++
+		LDA #<text3		; no update available, OK
		LDY #>text3
		JSR $CB1E
++
		LDX #11			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text6
		LDY #>text6
		JSR $CB1E


mainlp

getkey	LDA $CB 			; get key, $40 no key pressed
		CMP #$40
		BEQ mainlp
		CMP #60 			; "0"?	-> go back to portal
		BEQ keypor
		CMP #$0 			; "1"?	-> STD update
		BEQ keyone
		CMP #56 			; "2"?	-> DEV update
		BEQ keytwo
		CMP #4 				; "9"?	-> SPCL DEV update
		BEQ keynine
		JMP mainlp

keypor	LDA #$09			; index to go back to main menu of WiC20-portal
		STA selected
		JMP portal

keyone	JSR clrscr1
		JSR getupdstd
		JSR wait30
		JMP startprg

keytwo	JSR clrscr1
		JSR getupddev
		JSR wait30
		JMP startprg

keynine	JSR clrscr1
		JSR getupdspc
		JSR wait30
		JMP startprg

clrscr1
		LDX #11			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
		LDA #<text7
		LDY #>text7
		JSR $CB1E
		RTS

wait30
		LDA #110
		STA row
		LDX #14			; X+1 rows down
		LDY #$00		; y+1 cols right
		JSR $E50C		; cursor pos X Y
-		LDA #42			; print DOT
		JSR $FFD2		
		JSR delay2
		DEC row
		BNE -
		RTS

delay2
		LDY #$FF   
-		JSR $EF96			; delay 1ms
		DEY 
		BNE -
		RTS

text0	
		!by 31,18,232,233,233,233,233,233,233,234,32,32,32,32,32,32,32,32,32,32,220,221,222,223
		!by 31,224,156,225,159,226,227,228,158,229,230,31,231,"i","N","F","O"," ","&"," ","u","P","D","A","T","E",32
		!by 31,235,236,236,236,236,236,236,237,32,32,32,32,32,32,32,32,32,32,32,32,32,239,146
		!by 30,$00

text1
		!by 30
		!text "wIc fw: ",$0D,$0D
		!text " ",$0D,$0D
		!by 0

text2
		!by 31,231,232,234,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219,219
		!by 31,18,32,32,146,237,30
		!text "ip",$0D
		!text " ssid",5,0

text3
		!by 18,30
		!text " fIRMWARE IST AKTUELL ",0

text4
		!by 18,28
		!text " uPDATE std VERFUEGBAR",0

text5
		!by 18,28
		!text " uPDATE dev VERFUEGBAR",0

text6
		!by 146,158
		!text " 1 - sTANDARD uPDATE  ",$0D,$0D
		!text " 2 - dEVELOPER uPDATE ",$0D,$0D
		!text " 0 - zURUECK          ",0
text7
		!by 146,158
		!text " bITTE wARTEN ....    ",$0D
		!text "    CA. 30 sEKUNDEN  ",$0D
		!text "                      ",$0D,$0D
		!text "                      ",0


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

getfwv
		LDA com_jmp+1 
		TAY 
		LDA com_jmp 
		JSR wicprnt
		RTS

getfwd
		LDA com_jmp+3
		TAY 
		LDA com_jmp+2 
		JSR wicprnt
		RTS

getip
		LDA com_jmp+5 
		TAY 
		LDA com_jmp+4 
		JSR wicprnt
		RTS

getssid
		LDA com_jmp+7 
		TAY 
		LDA com_jmp+6 
		JSR wicprnt
		RTS

getavail
		LDA com_jmp+9 
		TAY 
		LDA com_jmp+8 
		JSR wiccmd
		RTS

getupdstd
		LDA com_jmp+11 
		TAY 
		LDA com_jmp+10 
		JSR com_out
		RTS

getupddev
		LDA com_jmp+13 
		TAY 
		LDA com_jmp+12 
		JSR com_out
		RTS

getupdspc
		LDA com_jmp+15 
		TAY 
		LDA com_jmp+14 
		JSR com_out
		RTS

wicprnt	jsr com_out			; send command to WiC
		jsr wic64_pull_strt	; init pull data
		LDA bytes_send+1	; get length of string
		TAY
		LDA bytes_send	
		TAX 
-		JSR read_byte		; fetch data from WiC
		JSR $FFD2
		DEX
		BNE -
		DEY
		CPY #$FF
		BNE -
		RTS

wiccmd	jsr com_out			; send command to WiC
		jsr wic64_pull_strt	; init pull data
		LDA bytes_send+1	; get length of string
		TAY
		LDA bytes_send	
		TAX 
-		JSR read_byte		; fetch data from WiC
		NOP 				; 
		DEX
		BNE -
		DEY
		CPY #$FF
		BNE -
		RTS

; ---------------------------------------------


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

com_jmp		!by <com_load0, >com_load0, <com_load1, >com_load1
			!by <com_load2, >com_load2, <com_load3, >com_load3
			!by <com_load4, >com_load4, <com_load5, >com_load5
			!by <com_load6, >com_load6, <com_load7, >com_load7
			!by <com_load8, >com_load8, <com_loadm, >com_loadm

com_load0	!text "W",$04,$00,$00		; get FW version from WiC64
com_load1	!text "W",$04,$00,$07		; get FW date from WiC64
com_load2	!text "W",$04,$00,$06		; get IP from WiC64
com_load3	!text "W",$04,$00,$10		; get SSID from WiC64
com_load4	!text "W",$04,$00,$18		; update available? 0=NO, 1=STD, 2=DEV
com_load5	!text "W",$04,$00,$03		; perform FW update STANDARD
com_load6	!text "W",$04,$00,$04		; perform FW update DEVELOPER
com_load7	!text "W",$04,$00,$05		; perform FW update DEVELOPER SPECIAL
com_load8	!text "W",$04,$00,$00		; get FW version from WiC64
com_loadm	!text "W",$22,$00,$01
			!text "https://straessle.eu/start.prg",0 

