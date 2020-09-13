
;==============================================================
;
;	MAIN PROJECT FILE
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
.include "m8515def.inc"       ; definition
;==============================================================


;==============================================================
.dseg                         ; Data segment starts here
;==============================================================

; permanently used registers

	.def LCD_EN_MSK  = r19
	.def LCD_temp    = r20
	.def cnt1        = r21
	.def cnt2        = r22
	.def cnt3	     = r23
	.def cnt4        = r24
	.def MIDI_temp   = r25
	.def ADDR_OFFSET = r18 
	.def slot_buf    = r17
	.def mem_slot	 = r16

; memory slot status byte constants

	.equ display_write_in_progress   = 2 ;(memory slot locked)
	.equ slot_ready_for_transmission = 1 ;(memory slot filled and ready)
	.equ slot_unused_or_empty		 = 0 ;(memory slot processed or unused) 

; memory slot size constants

	.equ NumHeaderBytes = 2				; status-flags byte, colorcode byte	
	.equ DataBlockSize  = 32
	.equ blocksize 		= NumHeaderBytes + DataBlockSize

; Variable declarations

	midi_buf    :  .byte  3
	inttostr_buf:  .byte  3
	sysEx_pos   :  .byte  2
	sysEx_buf   :  .byte blocksize * 8 	;[header bytes][data0][header bytes][data1]...[header bytes][data7] 



;==============================================================









;==============================================================
.eseg                           ; EPROM segment starts here
;==============================================================

; base device ID and config
	disp_id:	.db $00,$00	;	byte 1, byte 2
	skip_intro: .db 'N'
	dark_stdby: .db 'Y'
	
	
; some display strings
	msg01_1:	.db "Midi",0
	msg01_2:	.db "Interface",0

	msg02_1:	.db "ROM ver. 1.02.8",0
	msg02_2:	.db "by H. Wurst 08",0

	msg03_1:	.db "           ready",0
	msg03_2:	.db "ID ",0

	msg04_1:	.db "bad addr. range",0

	msg04_4:	.db "base addr. must",0
	msg04_5:	.db "be of X0-XX",0

	msg04_6:	.db "device updated. ",0
	msg04_7:	.db "rebooting...",0


;==============================================================






;==============================================================
;						include macros
;==============================================================

.include "macros.inc"
.include "int2_rescue.inc"

;==============================================================






;==============================================================
.cseg                       ; Code segment starts here
;==============================================================

.org $0000 		rjmp initialize    
.org INT2addr 	rjmp IncomingMidiData ;(IRQ2 : RE0, see manual p.74,77) 
	    

initialize: 

	.include "midilcd8init.asm"					; initialization

	clr mem_slot



;______________________________________________________________
;			low-priority processing loop
;______________________________________________________________
loop:

; repeat for all 8 displays over and over

	SelectSlot mem_slot ;(addr in r1:r0)

; if slot status not "ready for TX" then skip slot

	rmovfw xl, r1, r0
	cpi xl, slot_ready_for_transmission
	brne loop_skipslot

; set slot status "being transmitted", lock slot to prevent corrupted content
; incoming messages for this display ID will be ignored until current data is written

	ldi xl, display_write_in_progress
	rmovwf r1, r0, xl					; write status byte to header

; transmit
	inc16 r1, r0 						; increment address pointer
	
; light?
; backlight on / off OK

	rmovfw xl, r1, r0					; read color code byte

	sb LCD_BACKLIGHT, mem_slot			; switch on display of index mem_slot
	tst xl								; display = 0 (off) ?
	skeq								; 
	rjmp loop_skipsetcommand			; no? then... ---.
	cb LCD_BACKLIGHT, mem_slot			; yes? then...   | (turn off)
	loop_skipsetcommand:				; done        <--' (leave on)

; check for ascii special commands ('.cmd '...)

	rcall cmd_check

; transmit display string
	
	rcall LCD_TX


; free current slot (thus unlock it)

	SelectSlot mem_slot
	ldi xl, slot_unused_or_empty
	rmovwf r1, r0, xl

; else skip and continue

loop_skipslot : 

	inc mem_slot
	andi mem_slot, 0b00000111 ; cycle slots 0..7, then start over again

	rjmp loop  
; loop ends here	

	
	    

; dedicated subroutine that transmits the two 16 byte text buffers to the corresponding LCD-Display
LCD_TX:
    clr LCD_EN_MSK
	sbl LCD_EN_MSK, mem_slot	; select display, disable other displays

	rcall lcd_line1				; set cursor to the beginning of line 1

		ldi yl, 16				; for 16 bytes do...
  	LCD_TX_loop01:

		inc16 r1, r0 			; increase address pointer
		rmovfw lcd_temp, r1, r0	; load character from RAM
		rcall lcd_send			; transmit
	
		dec yl				
		brne LCD_TX_loop01		; repeat until 16 characters are processed

	rcall lcd_line2				; set cursor to the beginning of line 2

		ldi yl, 16				; do the same as above
  	LCD_TX_loop02:

		inc16 r1, r0 
		rmovfw lcd_temp, r1, r0
		rcall lcd_send
	
		dec yl
		brne LCD_TX_loop02	

	ret

;______________________________________________________________











;______________________________________________________________
;			module command recognition subroutines
;______________________________________________________________


cmd_check:
	; Commands are experimental. Checks must be performed by the software tools to reconfigure the interface modules.
		push r0
		push r1				
				inc16 r1, r0
				rmovfw XH, r1, r0	
				cpi XH, '.'	
				brne skip_cmd_check
	
				inc16 r1, r0
				rmovfw XH, r1, r0	
				cpi XH, 'c'	
				brne skip_cmd_check
	
				inc16 r1, r0
				rmovfw XH, r1, r0	
				cpi XH, 'm'	
				brne skip_cmd_check
						
				inc16 r1, r0
				rmovfw XH, r1, r0	
				cpi XH, 'd'	
				brne skip_cmd_check
						
				inc16 r1, r0
				inc16 r1, r0
				rmovfw XH, r1, r0
				cpi XH, '0'	
				skne
				rcall cmd_set_base_disp_id 	;".cmd 0 X0-XX" -> set display ID (warning: avoid display indices that overlap with midi commands!)

				cpi XH, '1'	
				skne
				rcall cmd_set_skip_intro 	;".cmd 1 X" -> enable/disable intro text

				cpi XH, '2'	
				skne
				rcall cmd_set_dark_standby 	;".cmd 1 X" -> enable/disable backlight on/off default after start-up

	skip_cmd_check:

		pop r1
		pop r0
       ret
;______________________________________________________________






;______________________________________________________________
;			module command handing subroutines
;______________________________________________________________
		

cmd_set_base_disp_id:
			
		rcall wait1sec

		clr yl
		clr yh

		
		inc16 r1, r0 ; ".cmd_0_x0_xx" hex 
		inc16 r1, r0 ; --------^ now here

; convert hexadecimal chars to byte 1

		rmovfw xl, r1, r0
		cpi xl, ':' ; this is the char after '9'
		brlt pc + 2
		subi xl, 'A'-'9'
		subi xl, '0' ; xl now contains a value 0..15

		cpi xl, 16
		brge err_illegal_base_address ; just 0..15 allowed (accept only [0..9, 'A'..'F'])
		mov YL, xl
		swap YL

		inc16 r1, r0
		rmovfw XH, r1, r0
		cpi XH, '0'	
		brne err_illegal_base_address	; lower nibble must be zero in byte 1
; -
		inc16 r1, r0

; convert hexadecimal chars to byte 2

		inc16 r1, r0
		rmovfw xl, r1, r0
		cpi xl, ':' ; this is the char after '9'
		brlt pc + 2
		subi xl, 'A'-'9'
		subi xl, '0' ; xl now contains a value 0..15

		cpi xl, 16
		brge err_illegal_base_address ; just 0..15 allowed
		mov YH, xl
		swap YH

		inc16 r1, r0
		rmovfw xl, r1, r0
		cpi xl, ':' ; this is the char after '9'
		brlt pc + 2
		subi xl, 'A'-'9'
		subi xl, '0' ; xl now contains a value 0..15

		cpi xl, 16
		brge err_illegal_base_address ; just 0..15 allowed
		or YH, xl

			
		rcall update_base_id

		// no return (reboot)	




err_illegal_base_address:  ; display error message and restart device
                rcall LCD_clear
                SndLCDEEPStr msg04_1

				rcall wait1sec
				rcall wait1sec

                rcall LCD_line1
                SndLCDEEPStr msg04_4
                rcall LCD_line2
                SndLCDEEPStr msg04_5

				rcall wait1sec
				rcall wait1sec

	rjmp 0000	; reset	




update_base_id: 	
		
		movwe disp_id+1, YH
		movwe disp_id, YL
		
        rcall LCD_line1
        SndLCDEEPStr msg04_6
        rcall LCD_line2
        SndLCDEEPStr msg04_7

		rcall wait1sec
		rcall wait1sec

	rjmp 0000	; reset	





cmd_set_skip_intro:

		inc16 r1, r0 ; ".cmd_1_X" hex :o
		inc16 r1, r0 ; --------^ now here
		rmovfw XH, r1, r0
		movwe skip_intro, XH		

        rcall LCD_line1
        SndLCDEEPStr msg04_6
        rcall LCD_line2
        SndLCDEEPStr msg04_7

		rcall wait1sec
		rcall wait1sec

	rjmp 0000	; reset	




cmd_set_dark_standby:

		inc16 r1, r0 ; ".cmd_2_X" hex :o
		inc16 r1, r0 ; --------^ now here
		rmovfw XH, r1, r0
		movwe dark_stdby, XH		

        rcall LCD_line1
        SndLCDEEPStr msg04_6
        rcall LCD_line2
        SndLCDEEPStr msg04_7

		rcall wait1sec
		rcall wait1sec

	rjmp 0000	; reset	


;______________________________________________________________






;______________________________________________________________
;			include other code segments
;______________________________________________________________

.include "MidiInput.inc"
.include "LCDRoutines.inc"
;______________________________________________________________






;______________________________________________________________
;		interrupt handling (high-priority midi data reading)
;______________________________________________________________

; This is a custom midi reading routine. Suggested improvement:
; Use the USART registers for increased reliability and less strict
; timing in the source code.



; interrupt that handles incoming midi data

IncomingMidiData :	; (SREG, I has been cleared by hardware, p.10)
		int2_enter				; backup register contents

;------- <5µs since interrupt was detected --------------

		rcall wait_for_midi_data; (cnt1) (SREG)
		rcall read_midi_byte	; alters midi_temp, (cnt2), (xl) (SREG)
		cpi midi_temp, $F0		; system-exclusive message start (SREG)
		brne abort_INT2 

		rcall wait_for_midi_data
		rcall read_midi_byte
		cpi midi_temp, $7D		; manufacturer ID $7D
		brne abort_INT2 

		rcall wait_for_midi_data
		rcall read_midi_byte
		cpi midi_temp, $01		; LCD panel message $01
		brne abort_INT2 

		rcall wait_for_midi_data
		rcall read_midi_byte	; ID byte 1

		MOVEW xl,disp_id		; (ZL), (ZH)
        andi xl, 0b11110000								; mask out 4 lower bits (max. 16 devices per controller )
														; (reserved for future modifications : line splitting to use
														; two addresses for one 16x2 LCD display)	

        mov xh, midi_temp		
		andi xh, 0b11110000	
        cp xl, xh				; upper 4 bits equal ?
        brne abort_INT2	

		SelectSlot midi_temp
		mov slot_buf, midi_temp

		sbrc midi_temp, 3 		;till now 1000-1111 are ignored (display line multiplexing not implemented)
		rjmp abort_INT2

		rcall wait_for_midi_data ; ID byte 2
		rcall read_midi_byte	
		MOVEW xl, disp_id + 1
		cp midi_temp, xl		
		brne abort_INT2 
		
		rcall read_incoming_lcd_data ;(34 byte : <ccode>,<32 byte LCD data>, <$F7 sysex end>)		
	
abort_INT2: 
		int2_leave				; restore register contents
		reti					; (sei and ret in one)
		




read_incoming_lcd_data:						; read system-exclusive data block (store to slot if it is avaiable )

		rmovfw xl, r1, r0
		
	    cpi xl,display_write_in_progress 	; error: non-interrupt routine is transmitting data to LCD, Do not overwrite data
		breq abort_rild						; drop data packet

		cpi xl,slot_ready_for_transmission  ; error : data already stored and not yet transmitted. 
		breq abort_rild						; drop data packet
		
		
											; At this point, slot status must be "unused or empty". Continue...
		
		ldi xh, 33
	read_ild_loop: 

			inc16 r1, r0
			rcall wait_for_midi_data
			rcall read_midi_byte
			rmovwf r1, r0, midi_temp
			dec xh
		brne read_ild_loop

											; let's see if transmission is terminated and hence valid

		rcall wait_for_midi_data 
		rcall read_midi_byte	 
		cpi midi_temp, $F7					; system-exclusive message end		
		brne abort_rild
		
		SelectSlot slot_buf
		ldi xl, slot_ready_for_transmission	; if system-exclusive block is terminated correctly, data is assumed to be valid.
		rmovwf r1, r0, xl
	 
		 									; finished. return to processing cycle after interrupt occurence
	abort_rild: ;
		ret


;______________________________________________________________
	








;==============================================================
.exit      			; end of code segment and program
;==============================================================






















