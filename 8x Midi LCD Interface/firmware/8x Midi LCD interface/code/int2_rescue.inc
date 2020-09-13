
;==============================================================
;
;	INTERRUPT ENTER/LEAVE HELPER MACROS
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
;		interrupt macros
;==============================================================

.macro int2_enter		; note: interrupt is now cleared and must be set when leaving
		push r0

		in r0, SREG		; save status register
		push r0			; push SREG on stack

		push r1			; save all the other registers alike
		push r2
		push r3
		push r4
		push r5
		push r6
		push r7
		push r8
		push r9
		push r10
		push r11
		push r12
		push r13
		push r14
		push r15
		push r16
		push r17
		push r18
		push r19
		push r20
		push r21
		push r22
		push r23
		push r24
		push r25
		push r26
		push r27
		push r28
		push r29
		push r30
		push r31
.endmacro


.macro int2_leave
		ldi xl, 1<<INTF2
		out GIFR, xl	 ; this is absurd : you have to SET the INTF2 Flag to clear it... but the manual says so

		pop r31		
		pop r30
		pop r29
		pop r28
		pop r27
		pop r26
		pop r25
		pop r24
		pop r23
		pop r22
		pop r21
		pop r20
		pop r19
		pop r18
		pop r17
		pop r16
		pop r15
		pop r14
		pop r13
		pop r12
		pop r11
		pop r10
		pop r9
		pop r8
		pop r7
		pop r6
		pop r5
		pop r4
		pop r3
		pop r2
		pop r1

		pop r0		
		out SREG, r0	; restore status register

		pop r0
.endmacro


;==============================================================
