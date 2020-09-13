;==============================================================
 	    				     ; String loading routine
;==============================================================

SndLCDStrProc:
		sbi   EECR, EERE		 ; EEPROM Lesen
		in    LCD_temp, EEDR	 ; gelesenen wert in LCD_temp
		tst   LCD_temp		 	 ; wenn 0 dann fertig
		breq  SndLCDStrProc_done
							 	 ; ansonsten
		rcall LCD_send		 	 ; Zeichen senden
		in    ZL, EEARL          ; und Adresszeiger erhöhen
		;in    ZH, EEARH
		inc   ZL 
		out   EEARL, ZL
		;out   EEARH, ZH
		
		rjmp  SndLCDStrProc 	 ; und das ganze wiederholen bis eine 0 kommt
				
	SndLCDStrProc_done:	     	 ; damit ist die routine fertig
        ret


;==============================================================


;==============================================================
 	    				     ; LCD Subroutines start here
;==============================================================
.equ LCD_RS = 5
.equ LCD_EN = 6
.equ LCD_PORT = PORTB
.equ LCD_PORT2 = PORTD


wait1sec:
		ldi cnt4,$64
	wait1sec_:
		rcall wait_5ms
		dec cnt4
		brne wait1sec_

        ret

Wait_5ms:                    			;>5ms Pause
        ldi  cnt1, $42
	WGLOOP0:   
		ldi  cnt2, $C9
	WGLOOP1:   
		dec  cnt2
        brne WGLOOP1
        dec  cnt1
        brne WGLOOP0
        
		ret 

Wait_50us:                              ;50us Pause
        ldi  cnt1, $A0 					; anstelle $84
	Wait_50us_:
		dec  cnt1
        brne Wait_50us_
        
		ret                          	;wieder zurück



LCD_toggle_enable:
        sbi   LCD_PORT2, LCD_EN    		; Enable an
		nop					 			; mind. 220ns warten
		nop
		nop
		nop
		nop
		nop
		nop
		nop
        cbi   LCD_PORT2, LCD_EN    		; Enable aus
		
		ret

lcd_send: 
		push  ZL
		push  ZH
        mov   ZL, LCD_temp              ; "Sicherungskopie" für
                                        ; die Übertragung des 2.Nibbles
        swap  LCD_temp                  ; Vertauschen
        andi  LCD_temp , 0b00001111     ; oberes Nibble auf Null setzen
        ;sbr   LCD_temp , 1<<4           ; entspricht 0b00010000
		sbi LCD_PORT2, LCD_RS
			in ZH, LCD_PORT
			andi ZH, 0b11110000
			or LCD_temp, ZH 
        out   LCD_PORT, LCD_temp        ; ausgeben
        rcall LCD_toggle_enable         ; Enable-Routine aufrufen
                                        ; 2. Nibble, kein swap da es schon
                                        ; an der richtigen stelle ist
        andi  ZL, 0b00001111            ; obere Hälfte auf Null setzen 
        sbi LCD_PORT2, LCD_RS
		;sbr   ZL, 1<<4                  ; entspricht 0b00010000
			or ZL, ZH 
        out   LCD_PORT, ZL                 ; ausgeben
        rcall LCD_toggle_enable         ; Enable-Routine aufrufen
        rcall Wait_50us                 ; Delay-Routine aufrufen
        	
		pop ZH
		pop ZL
		ret                             ; zurück zum Hauptprogramm

 ;sendet einen Befehl an das LCD
lcd_command:                            ; wie lcd_data, nur ohne RS zu setzen
        push  ZL
		push  ZH

        cbi LCD_PORT2, LCD_RS
        mov   ZL, LCD_temp 
        swap  LCD_temp 
        andi  LCD_temp , 0b00001111
			in ZH, LCD_PORT
			andi ZH, 0b11110000
			or LCD_temp, ZH 
        out   LCD_PORT, LCD_temp 
        rcall LCD_toggle_enable
        andi  ZL, 0b00001111
			or ZL, ZH 
        out   LCD_PORT, ZL
        rcall LCD_toggle_enable
        rcall Wait_50us

		pop   ZH
		pop   ZL
		ret

 

LCD_init:
        ldi	  cnt3,$A0
	powerupwait:
        rcall Wait_5ms				; powerup wait 15msec after vdd>4.5V	
        dec	  cnt3
        brne  powerupwait
	
		ldi   LCD_temp, 0b00000011	; initialisieren
		out   LCD_PORT, LCD_temp	  
		rcall LCD_toggle_enable		; 3x senden
		rcall Wait_5ms				; wait 5msec
		rcall LCD_toggle_enable
		rcall Wait_5ms				; wait 5msec
		rcall LCD_toggle_enable
		rcall Wait_5ms				; wait 5msec

        ldi   LCD_temp, 0b00000010    ; 4bit-Modus einstellen
        out   LCD_PORT, LCD_temp
        rcall LCD_toggle_enable
        rcall Wait_5ms
        ldi   LCD_temp, 0b00101000    ; set function  : 2 lines
        rcall lcd_command
        ldi   LCD_temp, 0b00001100    ; display on/off: display on, cursor off, blinking off
        rcall lcd_command
        ldi   LCD_temp, 0b00000100    ; entry mode set: cursor move decrease, display not shifted 
        rcall lcd_command
        
		ret

LCD_clear:
        ldi   LCD_temp, 0b00000001   ;Display löschen
        rcall lcd_command
        rcall Wait_5ms
        ret


LCD_line1:
		ldi   LCD_temp, 0b10000000  ; cursor nach 00H: Home
		rcall LCD_command 
		ret

LCD_line2:
		ldi   LCD_temp, 0b11000000  ; cursor nach 40H: zweite Zeile
		rcall LCD_command 
		ret

SndLCDEEPHexVal_code: 				; EE addr muss initialisiert sein !
		sbi   EECR, EERE			; lesen
		in    LCD_temp, EEDR		; gelesenen wert in LCD_temp
		swap  LCD_temp				; obere 4 bit
		andi  LCD_temp,0b00001111

		ldi   xl, '0'
		add   LCD_temp, xl
		cpi   LCD_temp, ':'     	; zusätzliches Offset nach 'A'
		ldi   xl, 'A'-':'
		brlt  pc + 2				; wenn 9..0 dann cool
		add   LCD_temp, xl 			; ansonsten nochmal offset dazu eben

		rcall lcd_send				; raus damit 

		in    LCD_temp, EEDR		; gelesenen wert in LCD_temp
		andi  LCD_temp,0b00001111

		ldi   xl, '0'
		add   LCD_temp, xl
		cpi   LCD_temp, ':'     	; zusätzliches Offset nach 'A'
		ldi   xl, 'A'-':'
		brlt  pc + 2				; wenn 9..0 dann cool
		add   LCD_temp, xl 			; ansonsten nochmal offset dazu eben

		rcall lcd_send				; raus damit 		
		
		ret




;==============================================================
