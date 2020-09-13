
;==============================================================
;
;	DEVICE INITIALISATION CODE
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
;			devide initialisation
;==============================================================



; 01 stack pointer initialization 

		InitStrackPtr

; 02 Port I/O mapping configuration 

        SetPortIOStat PORTA, 0b00000001 ; RA0      : LCD RS (all)		
        SetPortIOStat PORTB, 0b00001111 ; RB0..RB3 : LCD DATA (all)
		SetPortIOStat PORTC, 0b11111111	; RC0..RC7 : LCD EN0..EN7
		SetPortIOStat PORTD, 0b11111111 ; RD0..RD7 : LCD backlight 0..7
		SetPortIOStat PORTE, 0b00000000 ; RE0 	   : Midi IN

; 03 LCD initialization 

		clr r16
		out portD, r16

		ldi LCD_EN_MSK, 0b11111111 ; select all displays
                rcall LCD_init

		movew xl, skip_intro
		cpi xl, 'N'
		brne init03_skip

        rcall LCD_clear
        SndLCDEEPStr msg01_1
        rcall LCD_line2
		SndLCDEEPStr msg01_2

        clr	  r17		; turn on all backlights		
		clr   r16
		com	  r16
		rcall subfade

		out portD, r16

        rcall LCD_clear
        SndLCDEEPStr msg02_1
        rcall LCD_line2
        SndLCDEEPStr msg02_2
        rcall wait1sec

		movew xl, dark_stdby
		cpi xl, 'N'
		breq init03_skip

        clr	  r16		;  turn off all backlights 
		clr   r17
		com	  r17
		rcall subfade	
		
	init03_skip:

		ser r16
		movew xl, dark_stdby
		cpi xl, 'Y'
		skeq
		out portD, r16	

 
; 04	show standby messanges on all displays 

        rcall LCD_clear
        SndLCDEEPStr msg03_1
        rcall LCD_line2
        SndLCDEEPStr msg03_2

		ldi LCD_EN_MSK, (1<<0)
		ldi ADDR_OFFSET, 0
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<1)
		ldi ADDR_OFFSET, 1
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<2)
		ldi ADDR_OFFSET, 2
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<3)
		ldi ADDR_OFFSET, 3
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<4)
		ldi ADDR_OFFSET, 4
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<5)
		ldi ADDR_OFFSET, 5
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<6)
		ldi ADDR_OFFSET, 6
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, (1<<7)
		ldi ADDR_OFFSET, 7
        SndLCDEEPDecVal disp_id

		ldi LCD_EN_MSK, 0b11111111
		ldi lcd_temp, ' '
		rcall lcd_send
		ldi ADDR_OFFSET, 0
        SndLCDEEPDecVal disp_id + 1 ; byte 2 is the same for all displays ;)


; 05 clear all slot status bytes

		ldi xl,0 
		ldi xh,0
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh
		inc xl
		
		SelectSlot xl
		rmovwf r1, r0, xh



; 06 setup Int2 Interrupt on falling edge

	;INT2 on PE0				; first disable
		clr xl
		out GICR, xl
	;falling edge				;sbi EMCUCR, ISC2; extended mcu control register : interrupt sense control for INT2: 0 (falling edge triggered ) (p.78)
		out EMCUCR, xl			; EMCUCR, ISC2 = 0
	; clear occurence
		out GIFR, xl
	;INT2 on PE0				;sbi GICR, INT2	; activate INT2
		ldi xl, 1<< INT2
		out GICR, xl
	;enable interrupts
		sei	


; device initialisation done.

;==============================================================
