.equ midi_port = PInD
.equ midi_pin  = 0

read_midi_append_byte:
		movfw xl, sysEx_pos				; load storage offset
		movwfex sysEx_buf, midi_temp, xl; store byte
		inc   xl						    ; increment storage offset
		movwf sysEx_pos, xl				; save storage offset
		ret

read_midi_sys_exclusive:
 		clr   xl
		movwf sysEx_pos, xl 		; 0
		
		rcall wait_for_midi_data 	; read a midi byte
		rcall read_midi_byte

		cpi   midi_temp, $F0 		; system exclusive start ?
		skeq	          			; skip if equal
		rjmp  read_midi_sys_exclusive ; byte is not a valid start byte, so keep waiting for it
									; es geht los :D

		rcall read_midi_append_byte ; store byte

	rmse_lbl01:
		rcall wait_for_midi_data    ; read a midi byte
		rcall read_midi_byte
			    	
		cpi   midi_temp, $F0 		; system exclusive start ?
		skne	           			; skip if not equal
		rjmp  read_midi_sys_exclusive ; anther beginning has occured before reading finished, start over

		rcall read_midi_append_byte ; store byte

		cpi   midi_temp, $F7 		; system exclusive end ?
		skeq	           			; skip if equal: end of midi frame
		rjmp  rmse_lbl01 			; go back to read another byte

		ret

wait_for_midi_data:
		sbic  midi_port,midi_pin 	; skip if bit in I/O Reg. cleared // start bit detection (falling edge)
		rjmp  wait_for_midi_data
		
		ldi   cnt1, 40              ; wait and see if value is stable
	wfmd_lbl01:	    
		sbic  midi_port,midi_pin 	; skip if bit in I/O Reg. cleared // state still stable 0
		rjmp  wait_for_midi_data	; if 1 then retry	
	    dec   cnt1
		brne  wfmd_lbl01		

 	    ret


wait_31ys:
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

		ret

read_midi_frame:
        ldi   cnt4, 0   			; reset buffer position
    rmf_lbl00:
									; wait for byte
        rcall wait_for_midi_data
        							; read byte
		rcall read_midi_byte
									; if byte contains cmd and chan
		sbrc  midi_temp, 7  		; MSB is 1 if first byte (bit 7 set)
		ldi   cnt4, 0               ; then reset buffer position
								 	; store byte to ram
        movwfex midi_buf, midi_temp, cnt4
		inc   cnt4
									; if count<>3 then read another byte
		cpi   cnt4, 3
		brne  rmf_lbl00

		rcall wait_31ys
		ret
