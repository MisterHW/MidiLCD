
;==============================================================
;
;	LCD DISPLAY HANDLING ROUTINES FOR HD47780-COMPATIBLE
;	DISPLAYS with 16x2 CHARACTERS
;
;==============================================================
;
;	8 display midi-controlled interface firmware
;	Complies with Crumhorn Labs : Hauptwerk 2.0 
;	LCD message system format
;
;	by Helge Wurst 2006-2008
;  
;==============================================================





;==============================================================
 	    				     ; String loading routine
;==============================================================

SndLCDStrProc:					 ; (EEARL and EEARH must be set before calling, no parameters passed.)
		sbi   EECR, EERE		 ; read EEPROM 
		in    LCD_temp, EEDR	 ; store the value read to LCD_temp
		tst   LCD_temp		 	 ;  = #0 ? : finished. (zero-terminated string)
		breq  SndLCDStrProc_done
							 	 ; else
		rcall LCD_send		 	 ; send char to display
		in    ZL, EEARL          ; increment address pointer (no handling for addresses > 255!)
		inc   ZL 
		out   EEARL, ZL

		rjmp  SndLCDStrProc 	 ; repeat until #0 terminator found
				
	SndLCDStrProc_done:	     	 ; done.
        ret


;==============================================================







;==============================================================
 	    				     ; LCD Subroutines
;==============================================================


; definitions (modify when changing the hardware layout)

.equ LCD_RS_PORT 	= PORTA 	; (RS all) display register select (IO mapping: XXXXXXXO)
.equ LCD_RS 		= 0			; RS pin
.equ LCD_DATA_PORT  = PORTB		; 4 bit parallel interface to HD47780 controller (IO mapping: XXXXOOOO)
.equ LCD_EN_PORT 	= PORTC 	; (EN) enable (IO mapping: OOOOOOOO) : display 0..7 selected by LCD_EN_MSK
.equ LCD_BACKLIGHT  = PORTD		; (IO mapping: OOOOOOOO)




; miscallaneous delay loops

wait1sec:
		ldi cnt4,$B8
	wait1sec_:
		rcall wait_5ms
		dec cnt4
		brne wait1sec_

        ret



Wait_5ms:           			
        ldi  cnt1, $42
	WGLOOP0:   
		ldi  cnt2, $C9
	WGLOOP1:   
		dec  cnt2
        brne WGLOOP1
        dec  cnt1
        brne WGLOOP0
        
		ret 



Wait_50us:                              
        ldi  cnt1, $83 					
	Wait_50us_:
		dec  cnt1
        brne Wait_50us_
        
		ret                          	




Wait_20us:                              
        ldi  cnt1, $34 					
	Wait_20us_:
		dec  cnt1
        brne Wait_20us_
        
		ret                          	




Wait_10us:                             
        ldi  cnt1, $18 				
	Wait_10us_:
		dec  cnt1
        brne Wait_10us_
        nop
		
		ret                          	




LCD_toggle_enable:							; see LCD Display datasheet for details
		
        out  	LCD_EN_PORT, LCD_EN_MSK   	; Enable on
		  nop					 			; wait at least 220ns 
		  nop								; 125ns @8Mhz
		  nop
		push 	LCD_EN_MSK
        clr 	LCD_EN_MSK  				; Enable off
		out 	LCD_EN_PORT, LCD_EN_MSK
		pop 	LCD_EN_MSK

		ret



; send a character to selected displays
lcd_send: 
										; let LCD_temp be 76543210. Transfer as : xxxx7654 | toggle EN | xxxx3210 | toggle EN
		push  ZL						
        mov   ZL, LCD_temp              ; backup LCD_temp
                                        
        swap  LCD_temp                  ; swap low and high nibble
        andi  LCD_temp , 0b00001111     ; clear new high nibble

		sbi LCD_RS_PORT, LCD_RS			; set RS=1
        out   LCD_DATA_PORT, LCD_temp   ; transmit Hi(LCD_temp)
        rcall LCD_toggle_enable         
                                        
                                        ; now for the lowe nibble...
        andi  ZL, 0b00001111            ; clear high nibble

        sbi LCD_RS_PORT, LCD_RS			
        out   LCD_DATA_PORT, ZL 		; transmit ZL = Lo(LCD_temp)
        rcall LCD_toggle_enable 

        rcall Wait_50us                 ; call delay routine (wait until display has processed the input)
        	
		pop ZL
		ret                             



; send a command to selected displays
lcd_command:                            
        push  ZL
        mov   ZL, LCD_temp 

        swap  LCD_temp 
        andi  LCD_temp , 0b00001111

        cbi LCD_RS_PORT, LCD_RS			; set RS=0 (differs from lcd_send)
        out   LCD_DATA_PORT, LCD_temp 	; transmit Hi(LCD_temp)
        rcall LCD_toggle_enable


        andi  ZL, 0b00001111
		
										; RS=0 remains as set above.
        out   LCD_DATA_PORT, ZL			; transmit ZL = Lo(LCD_temp)
        rcall LCD_toggle_enable			

        rcall Wait_50us					; call delay routine (wait until display has processed the input)

		pop   ZL
		ret

 

LCD_init:								; initialize display(s) on start-up
        ldi	  cnt3,$05
	powerupwait:
        rcall Wait_5ms					; power-up: wait 15msec after vdd>4.5V	
        dec	  cnt3
        brne  powerupwait
	
		ldi   LCD_temp, 0b00000011		; initialize
		out   LCD_DATA_PORT, LCD_temp	  
		rcall LCD_toggle_enable			; send 3 times
		rcall Wait_5ms					; wait 5msec
		rcall LCD_toggle_enable
		rcall Wait_5ms					; wait 5msec
		rcall LCD_toggle_enable
		rcall Wait_5ms					; wait 5msec

        ldi   LCD_temp, 0b00000010    	; set 4bit parallel interface mode
        out   LCD_DATA_PORT, LCD_temp
        rcall LCD_toggle_enable
        rcall Wait_5ms
        ldi   LCD_temp, 0b00101000    	; set function  : 2 lines (for 16x2, 20x2, 40x2 LCD displays)
        rcall lcd_command
        ldi   LCD_temp, 0b00001100    	; display on/off: display on, cursor off, blinking off
        rcall lcd_command
        ldi   LCD_temp, 0b00000100    	; entry mode set: cursor move decrease, display not shifted 
        rcall lcd_command
        
		ret



LCD_clear:
        ldi   LCD_temp, 0b00000001   	; the clear display command...
        rcall lcd_command				; transmit it
        rcall Wait_5ms
        ret



LCD_line1:
		ldi   LCD_temp, 0b10000000  	; cursor to 00H: Home
		rcall LCD_command 	
		ret



LCD_line2:
		ldi   LCD_temp, 0b11000000  	; cursor to 40H: zweite Zeile
		rcall LCD_command 
		ret



SndLCDEEPHexVal_code: 				; EEARL/H address must be initialized 
		sbi   EECR, EERE			; toggle to read from EEPROM
		in    LCD_temp, EEDR		; store read data from EEPROM data register to LCD_temp
		swap  LCD_temp				; upper 4 bits first
		andi  LCD_temp,0b00001111

		ldi   xl, '0'				; convert 0 to '0'=#48 (+48)
		add   LCD_temp, xl
		cpi   LCD_temp, ':'     	; additional offset for chars beyond '9' needed?
		ldi   xl, 'A'-':'			; ... '8'=#56 , '9'=#57, 'A'=#65 , 'B'=#66 ...
		brlt  pc + 2				; LCD_temp within '0'..'9' ? (equivalent to LCD_temp < ':'?)
		add   LCD_temp, xl 			; add offset if needed

		rcall lcd_send				; send hex character

		in    LCD_temp, EEDR		; store read data from EEDR to LCD_temp again
		add   LCD_temp, ADDR_OFFSET 
		andi  LCD_temp,0b00001111	; now select lower 4 bits


		ldi   xl, '0'				; and process then as above
		add   LCD_temp, xl
		cpi   LCD_temp, ':'     	
		ldi   xl, 'A'-':'
		brlt  pc + 2				
		add   LCD_temp, xl 			

		rcall lcd_send				; send hex character 		
		
		ret





subfade:  							; LCD 0..7 backlight PWM fade-in/fade-out subroutine from r17(initial) to r16(final) states 
									; R16, R17 need to be preset. No parameters passed.        			
        ldi  cnt1, $FF
	SFLOOP0:   
		ldi  cnt2, $FF
		out LCD_BACKLIGHT, r16
	SFLOOP1:   
		cp cnt2, cnt1
		skne
  		out LCD_BACKLIGHT, r17
		
		push cnt1
		rcall wait_10us
		pop cnt1

		dec  cnt2
        brne SFLOOP1
        dec  cnt1
        brne SFLOOP0
        
		out PORTD, r16
		ret 


;==============================================================
