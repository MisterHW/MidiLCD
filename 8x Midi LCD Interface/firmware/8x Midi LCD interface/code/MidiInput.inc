
;==============================================================
;
;	LOW-LEVEL MIDI COMMAND READING ROUTINE
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
 	    				     ; midi port definitions
;==============================================================

.equ midi_port = PInE
.equ midi_pin  = 0

;==============================================================





;==============================================================
 	    				     ; midi input subroutines
;==============================================================


wait_for_midi_data:
		sbic  midi_port,midi_pin 	; skip if bit in I/O Reg. cleared // start bit detection (falling edge)
		rjmp  wait_for_midi_data
		ldi   cnt1, 20              ; wait and see if value is stable 20
	wfmd_lbl01:	    
		sbic  midi_port,midi_pin 	; skip if bit in I/O Reg. cleared // state still stable 0
		rjmp  wait_for_midi_data	; if 1 then retry	
	    dec   cnt1
		brne  wfmd_lbl01
 	    ret



wait_31ys:							; wait for the duratoin of one bit ( 
									; 32µs ... 8 cycles are used for programm calls and jump commands 
		ldi   cnt1, 80
	w31ys_loop:	
		dec   cnt1
		brne  w31ys_loop
		nop
        ret



read_midi_byte:
		ldi   XL, 0b00000001        ; load bitmask
        clr   midi_temp		        ; clear temp
		ldi   cnt2, 8				; repeat for 8 bits
	rmb_lbl01:
		rcall wait_31ys			 	; wait...
		sbic  midi_port, midi_pin	; read, if 0 then skip
		or    midi_temp, XL		 	; else set equivalent bit
		lsl   XL				 	; shift bit mask left
		dec   cnt2
		brne  rmb_lbl01
	rmb_lbl02:
		sbis MIDI_PORT, MIDI_PIN	; wait for last bit to end (stopbit is irrelevant)
		rjmp rmb_lbl02	
		ret

;==============================================================
