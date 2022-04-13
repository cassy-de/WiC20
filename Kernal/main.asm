; VC20 / VIC20 version of WiC64 Kernal patch 
; version 1.0 cassy-de 
; Compiler: Visual Studio Code / ACME 
;  
; WiC64 Test Kernal for Loading directly .prg from
; http server
;
; replaces "!" at start of LOAD "!xxx" command with
; the default server (http://www.wic64.de/prg/)
;
; Changes F-Keys for loading WiC64 NET Start prorgam
; via F1 and 
; PHP directory demo via F3
;
; F3/F7 = RUN:
; F2 = SYS49152
; F4 = SYS64738 (RESET)
; F6 = PRINT PEEK(
; F8 = NEW
;
; The whole code is very big crap ! it's only a
; tech demo and not a real kernal for normal use !
;
; Lots of segment errors when compiling - but works anyway
; 
; This part of WiC64 was written 2020-2021 by KiWi
;
; Compiler used: C64 Studio by Georg Rottensteiner
;
; https://www.georg-rottensteiner.de/de/c64.html
;
;          WiC64 Hardware & Software - Copyright (c) 2021
;
;               Thomas "GMP" Müller <gmp@wic64.de>
;             Sven Oliver "KiWi" Arke <kiwi@wic64.de>
;          Hardy "Lazy Jones" Ullendahl <lazyjones@wic64.de>
;             Henning "YPS" Harperath <yps@wic64.de>
;
;    
;         All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are
;met:
;
;1. Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;
;2. Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in the
;   documentation and/or other materials provided with the
;   distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



; !to "wic20kernal.bin",plain

Pointerlow = $A4
Pointerhigh = $A5
KeyNumber = $A6


*=$e000
!bin "code/kernal.901486-07.bin"       ; PAL
;!bin "code/kernal.901486-06.bin"        ; NTSC


*=$edf3     ; new colors, light blue bckgnd, white border
!by 233

*=$E40B     ; change pointer to new init screen
    LDA #$E8 
    LDY #$F7 

*=$F7E7
    RTS     ; former "tape header", now new init screen
!by 147, 28, 18, 32, 32, 32, 32, 32, 32, 13
!by 158, 18, 32, 32, 32, 32, 32, 146, 32, 32, 159, 161, 187, 161, 190, 18, 172, 185, 146, 32, 18, 5, "W", "I", "C", 32, "2", "0", 13
!by 30, 18, 32, 32, 32, 32, 146, 32, 32, 32, 159, 18, 188, 188, 146, 161, 161, 18, 188, 184, 146, 32, 18, 5, "K", "E", "R", "N", "A", "L", 13
!by 159, 18, 32, 32, 32, 13
!by 156, 18, 32, 32, 146, 31, 32, 32, 0
eop1

*=$e602                     ; Keyboard abfrage patchen
    jsr FKeys
    
* = $eaf4     ; Recorder Taste abfragen killen

      nop
      nop

* = $eaf8     ; Tape Motor $C0 killen

      nop
      nop

* = $efa3

sendloadheader:
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
        JSR send_string   ; http://irgendwas an den ESP Senden
        RTS

load:
        sei
        lda #$00  ; Datenrichtung Port B Eingang
        sta $9112
        lda $911C
        and #$0F      ; CB2 auf LOW = ESP im Sendemodus
        ora #$C0      ; -> 110x xxxx 
        sta $911C
        jsr read_byte   ;; Dummy Byte - um IRQ im ESP anzuschubsen
        jsr read_byte
        sta $fa
        jsr read_byte
        sta $fb         ; Länge der Datenübertragung Byte 1 und 2
    
    
loaderrorcheck:
        lda $fa
        cmp #$00
        bne noloaderror
        lda $fb
        cmp #$02
        bne noloaderror

        jsr read_byte
        jsr $ffd2
        jsr read_byte
        jsr $ffd2
        lda #" "
        jsr $ffd2
        sec
        lda #$04
        cli
        rts

noloaderror:
    
    
setloadadress:    
        jsr read_byte
        sta $fc
        jsr read_byte
        sta $fd
        lda $b9                   ; Sekundäradresse holen
        cmp #$00
        bne LoadtoOriginal
    
        lda $c3                   ; Kernal übergibt Ladeadresse über $c3/c4
        sta $c1
        sta $fc
        lda $c4
        sta $c2
        sta $fd
    

LoadtoOriginal:
        lda $fc
        sta $c3
        sta $c1                   ; Load Adresse Start C1 bzw. Ende C3

        lda $fd
        sta $c4
        sta $c2 
    
startload:
        ldx $fb             ; low byte 
xloop:    
        ldy #$00
goread:
        jsr read_byte
        sta ($fc),y
        iny
        bne ycont
        inc $fd
ycont:    
        dex
        bne goread
        dec $fa
        lda $fa
        cmp #$ff
        bne goread
    
        sty $c3
        lda $fd
        sta $c4


cleanup:      ; ESP in Lesemodus schalten    
        lda $911C     ; Register 12 - PCR 
        and #$0F      ; CB1  IRQ falling edge
        ora #$E0      ; CB2 auf HIGH = ESP im Empfangsmodus
        sta $911C
        lda #$FF  ; Datenrichtung Port B Ausgang
        sta $9112

        lda #$00
        clc
        cli
        rts
    
save:


datastart = $c3
memend = $ae
length = $c1

    lda #$01
    sta datastart
    lda #$08
    sta datastart+1

    sec
    lda memend
    sbc datastart       ; Pointer Memory End load save
    sbc #1
    sta length

    lda memend+1
    sbc datastart+1
    sta length+1



    ldy #00                 ; Filenamen senden
write_filename_header:
    lda ($BB),y
;    jsr write_byte          ; Header senden
    iny
    cpy $B7
    bne write_filename_header



ende:
    clc
    rts


LoadHTTP:

send_string:                ; httpcommand: !text "W",$00,$00,$01
    lda #"W"
    jsr write_byte
    lda $b7
    clc
    adc #$04
    jsr write_byte
    lda #$00
    jsr write_byte
    lda #$01
    jsr write_byte

    ldy #00                 ; Filenamen senden
send_filenameheader:
    lda ($BB),y
    jsr charconvert
    jsr write_byte
    iny
    cpy $B7
    bne send_filenameheader
    rts
    
    
write_byte:
    sta $9110   ; Bit 0..7: Userport Daten PB 0-7 schreiben
    jsr pa6ack ; manually ACK to ESP
dowrite:
    lda $911D   ; Interrupt Flag Register 6522
    nop
    nop
    nop
    nop
    and #$10        ; Warten auf CB1 = Byte wurde gelesen vom ESP
    beq dowrite
    lda $9110     ; read ORB to reset IFR bit #$10  
    lda $911D   ; Interrupt Flag Register 6522
    ora #$10    ; set bit #5 to reset soft IRQ
    sta $911D   ; Interrupt Flag Register 6522
    rts

read_byte:
   
doread:
    lda $911D   ; Interrupt Flag Register 6522
    nop
    nop
    nop
    nop
    and #$10        ; Warten auf CB1 = Byte wurde vom ESP bereitgestellt
    beq doread

    lda $911D   ; Interrupt Flag Register 6522
    ora #$10    ; set bit #5 to reset soft IRQ
    sta $911D   ; Interrupt Flag Register 6522
    
    lda $9110 
    pha
    jsr pa6ack ; manually ACK to ESP
    pla
    rts

pa6ack					; manually generate PA6 ACK pulse -> /PC2
		LDA $911F		; read IRA
		AND #$BF		; reset PA6
		STA $911F		; -> PA6 = low
		ORA #$40		; set PA6
		STA $911F		; -> PA6 = high
		RTS 

charconvert:
    cmp #$20
    bne con0
    lda #$2e
con0:    
    cmp #$c0    
    bcs con2
    cmp #$40    
    bcs con1
    rts
con1:
    clc
    adc #$20
    rts
con2:
    sec
    sbc #$80
    rts

FKeys:
    jsr $e5cf               ; Originale Keyboard Abfrage ausführen
CheckFKeys:
    cmp #$8d
    bcs NoFKey              ; Größer als F8
    cmp #$85
    bcc NoFKey              ; Kleiner als F1
    sec
    sbc #$84
    sta KeyNumber           ; F1=1 / F8 = 8
    
    lda #<FTable            ; Zeiger auf F-Tasten Tabelle setzen
    sta Pointerlow
    lda #>FTable
    sta Pointerhigh
Search:
     jsr IncPointer
     dec KeyNumber
     lda KeyNumber
     cmp #0                 ; FX = Xte Null in F-Tasten Tabelle suchen (F1=erste Null - F8=8te Null)
     beq Found              ; Richtige xte 0 gefunden - Jetzt den Text ausgeben
SearchNextKey:
     ldy #$00
     lda (Pointerlow),y
     cmp #$00
     beq Search
     jsr IncPointer
     jmp SearchNextKey

Found:
      ldy #$00
      lda (Pointerlow),y
      cmp #0                ; Bei der Ausgabe auf 0 gestoßen
      beq NoFKey            ; Ausgabe beenden
      cmp #$0d              ; Auf ENTER gestoßen - Enter als letzte Taste dem Tastaturpuffer übermitteln
      beq NoFKey
      jsr $E742             ; CHROUT - Ein Zeichen ausgeben
      jsr IncPointer
      jmp Found
    
NoFKey:
    rts

    
IncPointer:                 ;Zeiger der F-Tasten Tabelle hoch zählen
     inc Pointerlow
     lda Pointerlow
     cmp #$00
     bne IncEnd
     inc Pointerhigh
IncEnd:
     rts
     
* = $F4C7                   ; used to be RS232, now function key table
FTable:                     ; Tastaturcodes von https://www.c64-wiki.de/wiki/C64-Tastaturtabelle
!byte 0
F1:
!text " ",147,"L",207,34,"HTTPS://STRAESSLE.EU/START.PRG",34,$0d
!byte 0
F3:
!text "RUN:",$0d
!byte 0
F5:
!text " ",147,"L",207,34,"!DIR.PRG",34,$0d
!byte 0
F7:
!text "RUN:",$0d
!byte 0
F2:
!text "SYS 49152"
!byte 0
F4:
!text "SYS 64822",$0d
!byte 0
F6:
!text "PRINT PEEK ("
!byte 0
F8:
!text "NEW:",$0d
!byte 0


* = $f5d9     ; jsr $f817 Tape play key check removed

      ldy #$01
      lda $c3
      sta ($b2),y
      iny
      lda $c4
      sta ($b2),y
      jsr sendloadheader
      jmp $f604

* = $f63d     ; jsr $ - load data from tape
      jsr load

* = $f6fd     ; jsr $ Tape record key check removed

      jsr $F728       ; 'SAVING' (Name) ausgeben
      jsr save
      clc
      rts

;*=$fd6c                     ; Fast Reset Patch , no RAM Check
;
;    jsr $fd02               ; CBM80 Cartige eingesteckt ?
;    beq cbm80
;
;    ldx #$00
;    ldy #$a0                ; $a000 Ende des Speichers für Basic
;    jmp exit
;   
;cbm80:
;    ldx #$00
;    ldy #$80                ; $8000 Ende des Speichers für Basic
;   
;exit:
;    jmp $fd8c
    


    
