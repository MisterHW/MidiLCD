
;==============================================================
; Here I will implement a midi to lcd interface - success :)
; AT Tiny 2313
; Apr 2005-Nov 2009
;==============================================================


;==============================================================
.include "2313def.inc"       ; definition
;==============================================================


;==============================================================
.dseg                                        ; Data segment starts here
;==============================================================

.def LCD_temp = r20
.def cnt1     = r21
.def cnt2     = r22
.def cnt3     = r23
.def cnt4     = r24
.def MIDI_temp= r25

; Variable declarations

        midi_buf    :  .byte  3
        sysEx_pos   :  .byte  1
        sysEx_buf   :  .byte 64 ; überdimensioniert

;==============================================================


;==============================================================
.eseg                                        ; EPROM segment starts here
;==============================================================

disp_id:
.db $A5,$2B
msg01_1:
.db "Midi",0
msg01_2:
.db "Interface",0
msg02_1:
.db "developed by",0
msg02_2:
.db "Helge Wurst 05",0
msg03_1:
.db "waiting...",0
msg03_2:
.db "ID: ",0





;==============================================================

.include "macros.inc"

;==============================================================
.cseg                                        ; Code segment starts here
;==============================================================

.org 0000                                    ; Start at address 0, no interrupts
                InitStrackPtr


                SetPortIOStat PORTB, 0b00011111 ;pin0..3: LCD data ; pin4: lcd backlight (inverted)
                SetPortIOStat PORTD, 0b01100000 ;pin0: midi input, pin5,6 :  LCD RS/EN

                rcall LCD_init

                rcall LCD_clear

        cbi PortB, 4      ; licht an
                rcall wait1sec
        sbi PortB, 4            ; licht aus!
                rcall wait1sec
        cbi PortB, 4      ; licht an
                rcall wait1sec


                SndLCDEEPStr msg01_1
                rcall LCD_line2
        SndLCDEEPStr msg01_2

                rcall wait1sec
                rcall wait1sec

                rcall LCD_clear
               SndLCDEEPStr msg02_1
                rcall LCD_line2
               SndLCDEEPStr msg02_2

                rcall wait1sec
                rcall wait1sec
       sbi PortB, 4            ; licht aus!

                rcall LCD_clear
                SndLCDEEPStr msg03_1
                rcall LCD_line2
                SndLCDEEPStr msg03_2


                SndLCDEEPHexVal disp_id + 1
                SndLCDEEPHexVal disp_id

                clr xl
                movwf sysEx_pos, xl ; 0

loop:

        rcall read_midi_sys_exclusive
                                                                        ; pre-check structure

                MOVFW XL, sysEx_pos
                cpi XL, 39                                      ; 39 chars ?
                brne loop

                ldi XL, 1
                MOVFWEX YH, sysEx_buf, XL
                cpi YH, $7D                                     ; manufacturer ID ok ?
                brne loop

                ldi XL, 2
                MOVFWEX YH, sysEx_buf, XL
                cpi YH, $01                                     ; 0x01 - message type code for HW2 LCD output message ?
                brne loop


                                                                        ; if wrong ID or errorenous then rjmp loop
                MOVEW YL,disp_id
                ldi XL, 3
                MOVFWEX YH, sysEx_buf, XL
                cp YL, YH
                brne loop


                MOVEW YL,disp_id+1
                ldi XL, 4
                MOVFWEX YH, sysEx_buf, XL
                cp YL, YH
                brne loop

                ;else


                ldi xl, 5
                movfwex midi_temp, sysEx_buf, xl

                sbi PortB,4                             ; licht aus?
                tst midi_temp
                skeq                                            ; überspringen wenn midi_temp 0, sieht richtig aus :D
                cbi PortB,4                             ; licht an
                                                                        ; string parsen, wegen der Umlaute
                ParseRamStr sysEx_buf   +6, 32

                                                                        ; Ausgabe
        rcall LCD_line1
                SndLCDRamStr sysEx_buf   +6, 16
                rcall LCD_line2
                SndLCDRamStr sysEx_buf+16+6, 16



        rjmp loop                       ; Sprung zu "loop:" -> Endlosschleife


;==============================================================

.include "MidiInput.inc"
.include "lcdprocs.inc"

.exit      
