
;==============================================================
;
;	ARITHMETIC, DATA AND STRING HANDLING MACROS
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



; PLEASE NOTE:
;
; EVERY REGISTER EXCEPT ZL AND ZH CAN BE USED IN MAIN CODE!
; ZL AND ZH MAY BE USED (AND ALTERED!) IN THE FOLLOWING MACROS
; BACKUP TO STACK BEFORE USING THESE MACROS, IF NEEDED.





;==============================================================
;		arithmetic basic functions and extended branch op.
;==============================================================


; set port directions
.macro SetPortIOStat 	 	 ; Port, cfg ; set BiDi-Modes, Port is PortA..PortD . 
		ldi ZL, @1			 ; load the BiDi-mode Bits in a register, 1 is output 0 is input
		out @0 - 1, ZL		 ; DDRA = PortA - 1 ; DDRB = PortB - 1 		
.endmacro



; does what it says
.macro  InitStrackPtr	
        ldi ZL, LOW(RAMEND)  ; LOW-Byte der obersten RAM-Adresse
        out SPL, ZL
        ldi ZH, HIGH(RAMEND) ; HIGH-Byte der obersten RAM-Adresse
        out SPH, ZH
.endmacro 




.macro skeq					; skip next line of code if equal, 
							; like the skip if bit ... commands, no params (needs cp/cpi/tst result in SREG)
 		breq pc + 2		
.endmacro




.macro skne					; skip next line of code if not equal, 
							; like the skip if bit ... commands, no params (needs cp/cpi/tst result in SREG)
 		brne pc + 2		
.endmacro




.macro incw 				; syntax <incw Rdl> - low byte of 2-byte register vector
	adiw @0 ,1 				; (@0 +1 : @0) = (@0 +1 : @0) + 1;
.endmacro




; increment a 16 bit variable (increment with carry)
.macro inc16 				;<Rd hi byte>, <Rd lo byte> --- slower; for registers  X, Y, Z use incw
	inc @1
	tst @1					
	skne
	inc @0
.endmacro




.macro cb 	; clear bit in I/O reg. Syntax : cb <Port>, <Reg.=0..7 (variable)>
	mov ZL, @1
	andi ZL, 0b00000111
	ldi ZH, (1<<0)
	inc ZL
  sb_shift_bitmask:
	dec ZL
	skeq
	lsl ZH
	cpi ZL, 0
	brne sb_shift_bitmask
	com ZH

	in ZL, @0
	and ZL, ZH
	out @0, ZL
.endmacro




.macro sb 	; set bit in I/O reg. Syntax : sb <Port>, <Reg.=0..7 (variable)>
	mov ZL, @1
	andi ZL, 0b00000111
	ldi ZH, (1<<0)
	inc ZL
  sb_shift_bitmask:
	dec ZL
	skeq
	lsl ZH
	cpi ZL, 0
	brne sb_shift_bitmask

	in ZL, @0
	or ZL, ZH
	out @0, ZL
.endmacro




.macro sbl 	; set bit in local ram reg. Syntax : sbl <Port>, <Reg.=0..7 (variable)>
	mov ZL, @1
	andi ZL, 0b00000111
	ldi ZH, (1<<0)
	inc ZL
  sb_shift_bitmask:
	dec ZL
	skeq
	lsl ZH
	cpi ZL, 0
	brne sb_shift_bitmask

	or @0, ZH
.endmacro






;==============================================================
 	    				     ; SRAM / EEPROM R/W operations
;==============================================================


.macro sbx ; set bit external ( sram ) syntax  sbx <register> <bit(s)>
	  in  ZL, @0
	  sbr ZL, @1; = ori ZL, @1
	  out @0, ZL
.endmacro




.macro cbx ; clear bit external ( sram ) syntax  sbx <register> <bit(s)>
	  in  ZL, @0
	  cbr  ZL, @1 ; = andi ZL, $FF- @1
	  out @0, ZL
.endmacro




; load from constant sram location
.macro MOVFW ; reg, var      ; syntax: movfw <register> <variable>
        ldi ZL, low(@1)    	 ; load Z register low 
        ldi ZH, high(@1)     ; load Z register high 
        ld  @0,Z             ; load var1 into register 1 
.endmacro




; load from (@1:@2) sram address location
.macro RMOVFW ;       		 ; syntax: rmovfw <Rd dest>, <Rd hi addr>, <Rd lo addr>
        mov ZH,  @1    	
        mov ZL,  @2   
        ld  @0,Z             
.endmacro




; load from constant sram location with variable offset
.macro MOVFWEX ; var, reg, offset ; syntax: movfwex <variable> <register> <Rd offset>
        ldi ZL, low(@1)    	 ; load Z register low 
        ldi ZH, high(@1)     ; load Z register high 
        add ZL, @2			 ; add offset
		brcc pc + 2 		 ; übetrag ? wenn nicht nächste zeile überspringen
		inc ZH      		 ; ggf Übertrag addieren
        ld  @0,Z             ; load var1 into register 1 
.endmacro




; store to constant sram location
.macro MOVWF ; var, reg      ; syntax: movwf <variable> <register>
        ldi ZL, low(@0)      ; Load Z register low 
        ldi ZH, high(@0)     ; Load Z register high 
        st  Z, @1		     ; store data back to ram 
.endmacro





; store to (@0:@1) sram address location
.macro RMOVWF       ; syntax:  rmovwf <Rd hi addr>, <Rd lo addr>, <Rd dest>
        mov ZH,  @0    	
        mov ZL,  @1 
        st  Z, @2		     
.endmacro





;  store to constant sram location with variable offset
.macro MOVWFEX ; var, reg, offset ; syntax: movwfex <variable> <register> <offset>
        ldi ZL, low(@0)      ; Load Z register low 
        ldi ZH, high(@0)     ; Load Z register high 
        add ZL, @2			 ; add offset
		brcc pc + 2 		 ; übetrag ? wenn nicht nächste zeile überspringen
		inc ZH      		 ; ggf Übertrag addieren
		st  Z, @1		     ; store data back to ram 
.endmacro





; load from constant EEPROM location
.macro MOVEW 					; move eeprom data to register; syntax: MOVEW reg, eepaddr
        ldi   ZL, HIGH(@1)      ; Adresse laden
        out   EEARH, ZL 
        ldi   ZL, LOW(@1)
        out   EEARL, ZL
		sbi   EECR, EERE		; lesen
		in    @0, EEDR			; gelesenen wert in Register
.endmacro





; load from @1:@2 eeprom address
.macro RMOVEW 					; RMOVEW <Rd dest>, <Rd hi addr>, <Rd low addr>
        out   EEARH, @1 
        out   EEARL, @2
		sbi   EECR, EERE		; lesen
		in    @0, EEDR			; gelesenen wert in Register
.endmacro




; load from constant EEPROM location with dynamic offset
.macro MOVEWEX 					; move eeprom data to register; syntax: MOVEW reg, eepaddr, offset
        ldi   ZH, HIGH(@1)      ; Adresse laden
        ldi   ZL, LOW(@1)
        add   ZL, @2			; add offset
		brcc  pc + 2 		 	; übetrag ? wenn nicht nächste zeile überspringen
		inc   ZH      		 	; ggf Übertrag addieren
        out   EEARH, ZH 
        out   EEARL, ZL

		sbi   EECR, EERE		; lesen
		in    @0, EEDR			; gelesenen wert in Register
.endmacro





; want some more ?  Copy entire memory blocks from EEPROM to RAM
.macro CopyEW 			; Syntax : CopyEW <RAMaddr>, <EEPaddr>, <size>
  push xl
  push yl
  
  tst @2 				; 0?
  breq CopyEW_exit	 
  
  eep_ram_copyloop:
  dec xl  
    movewex yl, @1, xl
	movwfex @0, yl, xl
  tst xl
  brne eep_ram_copyloop

  CopyEW_exit:
  pop yl
  pop xl
.endmacro





; save register content to constant EEPROM location (be careful not to use this in loops, 100,000 write cycles max.)
; use this routine to save persistent data to EEPROM
.macro MOVWE 					; move reg. data to eeprom ; syntax: MOVEW <eepaddr>, <reg>
		in   ZL, SREG
		push ZL
		cli						; no interrupt or write will fail
        ldi  ZL, HIGH(@0)      	; Adresse laden
        out  EEARH, ZL 
        ldi  ZL, LOW(@0)
        out  EEARL, ZL
		out  EEDR, @1			; Daten laden
		sbi	 EECR, EEMWE		; Bit 2 - EEMWE: EEPROM Master Write Enable
		sbi	 EECR, EEWE			; EEPROM Write enable ( start programming within 4 cycles)
		nop
		nop
		nop
		nop
		nop
	MOVWE_wait_for_write:
		sbic EECR, EEWE			; wait until write action is finished. (skip if EECR, EEWE cleared) ~ 1.5ms
		rjmp MOVWE_wait_for_write
		nop
		nop
		pop  ZL
		out  SREG, ZL
.endmacro





;==============================================================
;		 data conversion routines
;==============================================================

; convert some special chars for correct appearance on HD44780-compatible controllers
; (charset problems, see datasheet)

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





; convert a byte value to sting, output in a 3-byte ram variable
.macro ByteToStr ;  syntax ByteToStr <variable>,<register>
	  mov XH, @1
	  ldi YH, '0'

	  ; 100x (multiples of 100)
	  clr XL
	conv_100_:
	  cpi XH, 100 
	  brlo conv_100_done
	  inc XL
	  subi XH, 100
	  rjmp conv_100_
	conv_100_done:
	  add XL, YH
	  movwf @0, XL

	  ;  10x (multiples of 10)
	  clr XL
	conv_10_:
	  cpi XH, 10
	  brlo conv_10_done
	  inc XL
	  subi XH, 10
	  rjmp conv_10_
	conv_10_done:
	  add XL, YH
	  movwf @0+1, XL

	  ;   1x 
	  add XH, YH
	  movwf @0+2, XH

.endmacro






;==============================================================
;			LCD Routine Macros
;==============================================================



.macro SndLCDEEPStr			 ; Syntax SndLCDStr <eepromaddr>
        ldi   r16, HIGH(@0)    ; Adresse laden
        out   EEARH, r16 
        ldi   r16, LOW(@0)
        out   EEARL, r16
		rcall SndLCDStrProc  ; subprozedur starten
.endmacro 





.macro SndLCDEEPHexVal 			; syntax:SndLCDEEPHexVal <dw address>
        ldi   ZL, HIGH(@0)      ; Adresse laden
        out   EEARH, ZL 
        ldi   ZL, LOW(@0)
        out   EEARL, ZL
		rcall SndLCDEEPHexVal_code	; jump to LCDRoutines.inc
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





.macro SndLCDEEPDecVal 			; syntax:SndLCDEEPDecVal <dw address>, 
  		push r25
    	movew r25, @0
		add   r25, ADDR_OFFSET
		bytetostr inttostr_buf, r25
		SndLCDRAMStr inttostr_buf, 3
  		pop r25
.endmacro





;==============================================================
;		memory slot address calculation
;==============================================================


.macro SelectSlot ; <Rd 0..7>

	; (r1:r0) = blocksize * (@0 & $00000111)

		andi @0, 0b00000111	; memory slots 0..7 correspond to display 0..7
		ldi zl, blocksize
		mul zl, @0 ; 	alters R0, R1 !

	; (r1:r0) = (r1:r0) + sysEx_buf ram offset

		ldi zl, sysEx_buf 
		add r0, zl
		brcc pc + 2	; skip if SREG, C clear
		inc r1
.endmacro





;==============================================================
