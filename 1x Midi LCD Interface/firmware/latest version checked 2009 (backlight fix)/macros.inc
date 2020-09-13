;==============================================================
 	    				     ; Macro section starts here
;==============================================================

.macro skeq			; skip if equal, like the skip if bit ... commands, no params
 		breq pc + 2		
.endmacro

.macro skne			; skip if not equal, like the skip if bit ... commands, no params
 		brne pc + 2		
.endmacro

; EVERY REGISTER EXCEPT ZL AND ZH!
.macro MOVFW ; reg, var      ; syntax: movfw <register> <variable>
        ldi ZL, low(@1)    	 ; load Z register low 
        ldi ZH, high(@1)     ; load Z register high 
        ld  @0,Z             ; load var1 into register 1 
.endmacro


.macro MOVWF ; var, reg      ; syntax: movwf <variable> <register>
        ldi ZL, low(@0)      ; Load Z register low 
        ldi ZH, high(@0)     ; Load Z register high 
        st  Z, @1		     ; store data back to ram 
.endmacro

.macro MOVWFEX ; var, reg, offset ; syntax: movwfex <variable> <register> <offset>
        ldi ZL, low(@0)      ; Load Z register low 
        ldi ZH, high(@0)     ; Load Z register high 
        add ZL, @2			 ; add offset
		brcc pc + 2 		 ; übetrag ? wenn nicht nächste zeile überspringen
		inc ZH      		 ; ggf Übertrag addieren
		st  Z, @1		     ; store data back to ram 
.endmacro

.macro MOVFWEX ; var, reg, offset ; syntax: movfwex <variable> <register> <offset>
        ldi ZL, low(@1)    	 ; load Z register low 
        ldi ZH, high(@1)     ; load Z register high 
        add ZL, @2			 ; add offset
		brcc pc + 2 		 ; übetrag ? wenn nicht nächste zeile überspringen
		inc ZH      		 ; ggf Übertrag addieren
        ld  @0,Z             ; load var1 into register 1 
.endmacro

.macro SetPortIOStat 	 	 ; Port, cfg ; set BiDi-Modes, Port is PortA..PortD . 
		ldi ZL, @1			 ; load the BiDi-mode Bits in a register, 1 is output 0 is input
		out @0 - 1, ZL		 ; DDRA = PortA - 1 ; DDRB = PortB - 1 		
.endmacro

.macro  InitStrackPtr
        ldi ZL, LOW(RAMEND)  ; LOW-Byte der obersten RAM-Adresse
        out SPL, ZL
        ;ldi ZH, HIGH(RAMEND) ; HIGH-Byte der obersten RAM-Adresse
        ;out SPH, ZH
.endmacro 


.macro SndLCDEEPStr			 ; Syntax SndLCDStr <eepromaddr>
        ;ldi   r16, HIGH(@0)    ; Adresse laden
        ;out   EEARH, r16 
        ldi   r16, LOW(@0)
        out   EEARL, r16
		rcall SndLCDStrProc  ; subprozedur starten
.endmacro 



.macro SndLCDRAMStr ;<ramaddr>, <count>
		push  xl
		clr   xl

	SndLCDRamStr_loop:
		cpi   xl, @1
		skne 
		rjmp  SndLCDRamStr_done ; do this if equal
	
		movfwex lcd_temp, @0, xl
		rcall lcd_send
		inc   xl

		rjmp  SndLCDRamStr_loop

	SndLCDRamStr_done:
		pop   xl
.endmacro

.macro SndLCDEEPHexVal 			; syntax:SndLCDEEPHexVal <dw address>
        ;ldi   ZL, HIGH(@0)      ; Adresse laden
        ;out   EEARH, ZL 
        ldi   ZL, LOW(@0)
        out   EEARL, ZL
		rcall SndLCDEEPHexVal_code
.endmacro

.macro MOVEW 					; move eeprom data to register; syntax: MOVEW reg, eepaddr
        ;ldi   ZL, HIGH(@1)      ; Adresse laden
        ;out   EEARH, ZL 
        ldi   ZL, LOW(@1)
        out   EEARL, ZL
		sbi   EECR, EERE		; lesen
		in    @0, EEDR			; gelesenen wert in Register
.endmacro

.macro ParseRamStr 				; syntax: ParseRamStr <ramaddr>, <count>
		push XL 				; counter regitser
		push XH 				; buffer reg.
		
		ldi XL, @1
	ParseRamStr_lbl01:
		dec XL			

		MOVFWEx XH, @0, XL 		; load from ram, relative
		
		cpi XH, $E4 ; ä
		  skne
		  ldi XH, $E1
		cpi XH, $C4 ; Ä
		  skne
		  ldi XH, $E1
		cpi XH, $F6 ; ö
		  skne
		  ldi XH, $EF
		cpi XH, $D6 ; Ö
		  skne
		  ldi XH, $EF
		cpi XH, $FC ; ü
		  skne
		  ldi XH, $F5
		cpi XH, $DC ; Ü
		  skne
		  ldi XH, $F5

		MOVWFEx @0, XH, XL		; store to ram relative
	
		cpi xl, $00
		brne ParseRamStr_lbl01	; repeat for each

		pop XH					; done :)
		pop XL
.endmacro
		

;==============================================================