#MAKE_BIN#

#LOAD_SEGMENT=FFFFH#
#LOAD_OFFSET=0000H#

#CS=0000H#
#IP=0000H#

#DS=0000H#
#ES=0000H#

#SS=0000H#
#SP=FFFEH#

#AX=0000H#
#BX=0000H#
#CX=0000H#
#DX=0000H#
#SI=0000H#
#DI=0000H#
#BP=0000H#

        ;JUMP TO THE START OF THE CODE - RESET ADDRESS IS KEPT AT 0000:0000
        ;AS THIS IS ONLY A LIMITED SIMULATION
         JMP     ST1 
        ;FIRST FEW LOCATIONS IN CS ARE FOR IVT 
        ;THAT IS THE FIRST 512 LOACTIONS 
         DB     512 DUP(0) ; THIS IS ACCORDING TO ON PAPER DESIGN
         ;WHILE SIMULATION (SINCE WE ARE USING 2732 IN PROTEUS) WE WOULD HAVE TO RESERVE 1024 LOCATIONS
        ;TO INITIALISE THE PORTS OF THE 8255S
        PORT1A EQU 00H   
        PORT1B EQU 02H
        PORT1C EQU 04H
        CREG1  EQU 06H

        PORT2A EQU 10H
        PORT2B EQU 12H
        PORT2C EQU 14H
        CREG2  EQU 16H
    
		;HEX CODES FOR KEYPAD
		TABLEK  DB  0EEH, 0EDH, 0EBH, 0E7H,     ;0, 1, 2, 3
				DB  0DEH, 0DDH, 0DBH, 0D7H,     ;4, 5, 6, 7,
				DB  0BEH, 0BDH, 0BBH, 0B7H,     ;8, 9, BACKSPACE, ENTER,
				DB  07EH                        ;TEST
				
		;HEX CODES FOR DISPLAY
		TABLED  DB  0C0H, 0F9H, 0A4H, 0B0H,     ;0, 1, 2, 3,
				DB  099H, 092H, 082H, 0F8H,     ;4, 5, 6, 7,
				DB  080H, 090H                  ;8,9
					
		; ACTUAL NUMERIC VALUES
		TABLEV  DB 30H, 31H, 32H, 33H, 34H, 35H, 36H, 37H, 38H, 39H
		
		
		;CREATE THE DATABASE WITH IC NUMBERS
		NANDIC  DB '7400'
		ANDIC   DB '7408'
		ORIC    DB '7432'
		XORIC   DB '7486'
		XNORIC  DB '747266'
		
		ICVAL   DB 6 DUP(?); TO STORE THE INPUT
		IC7SEG  DB 6 DUP(?); TO STORE THE 7 SEGMENT VALUES OF THE I/P
		CNTDGS  DB 0; COUNT OF THE DIGITS
		FAILW   DB 08EH, 088H, 0F9H, 0C7H; 7 SEGMENT VALUE OF FAIL
		PASSW   DB 08CH, 088H, 092H, 092H; 7 SEGMENT VALUE OF PASS

;MAIN PROGRAM

ST1:    CLI
        ;INTIALIZE DS, ES,SS TO START OF RAM THAT IS AT 01000H
        MOV AX,0100H ; THIS IS ACCORDING TO ON PAPER DESIGN
        ; WHILE SIMULATING WE WOULD HAVE TO CHANGE THE ABOVE TO 0200H 
        ; (SINCE WE ARE USING 2732 IN PROTEUS BECAUSE OF WHICH RAM STARTS FROM 02000H)
        MOV DS,AX
        MOV ES,AX
        MOV SS,AX
        MOV SP,0FFFEH
        MOV SI,0000
        
        
        ;INITIALISE THE 8255_1
        MOV AL,10001000B
        OUT CREG1,AL

        X01:        MOV SI,0000
                    MOV CH, 0
                    MOV CS:CNTDGS, CH
        X0:         MOV AL,00H  ;CHECK FOR KEY RELEASE
                    OUT PORT1C,AL 
        X1:     Z10:MOV CH,CS:CNTDGS ; DISPLAY 
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    JE ZX
                    MOV BP,0
                    MOV BH,CS:IC7SEG[BP]
                    MOV DL,1
                Z11:MOV AL,0
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:IC7SEG[BP]
                    DEC CH
                    JNZ Z11
                ZX: IN AL, PORT1C   
                    AND AL,0F0H        
                    CMP AL,0F0H        
                    JNZ X1

                      
                    
        X2:     Z20:MOV CH,CS:CNTDGS   ;CHECK FOR KEY PRESS
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    JE ZY
                    MOV BP,0
                    MOV BH,CS:IC7SEG[BP]
                    MOV DL,1
                Z21:MOV AL,0            ;DISPLAY
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:IC7SEG[BP]
                    DEC CH
                    JNZ Z21
                ZY: MOV AL,00H
                    OUT PORT1C ,AL
                    IN AL,04H
                    AND AL,0F0H
                    CMP AL,0F0H
                    JZ X2

                    MOV AL,0EH  ;CHECK FOR KEYPRESS IN COLUMN 0
                    MOV BL,AL
                    OUT PORT1C,AL
                    IN  AL,PORT1C
                    AND AL,0F0H
                    CMP AL,0F0H
                    JNZ X3

                    MOV AL,0DH  ;CHECK FOR KEYPRESS IN COLUMN 1
                    MOV BL,AL
                    OUT PORT1C,AL
                    IN AL,PORT1C
                    AND AL,0F0H
                    CMP AL,0F0H
                    JNZ X3

                    MOV AL,0BH  ;CHECK FOR KEYPRESS IN COLUMN 2
                    MOV BL,AL
                    OUT PORT1C,AL
                    IN  AL,PORT1C
                    AND AL,0F0H
                    CMP AL,0F0H
                    JNZ X3

                    MOV AL,07H ;CHECK FOR KEYPRESS IN COLUMN 3
                    MOV BL,AL
                    OUT PORT1C,AL
                    IN  AL,PORT1C
                    AND AL,0F0H
                    CMP AL,0F0H
                    JZ X2

        X3:         OR AL,BL    ;DECODE KEY
                    MOV CX,0FH
                    MOV DI,00H
        X4:         CMP AL,CS:TABLEK[DI]
                    JZ  X5
                    INC DI
                    LOOP X4
        X5:         CMP DI, 0AH; CHECK BCKSPC
                    JNZ L1
                    JMP BCKSPC 
                L1: CMP DI, 0BH; CHECK ENTER
                    JNZ DISP
                    JMP ENTR

        DISP:       INC CS:CNTDGS; INCREASE THE COUNT OF DIGITS
                    MOV DL, CS:TABLEV[DI]; STORING THE INPUTS IN ICVAL AND IC7SEG
                    MOV CS:ICVAL[SI],DL
                    MOV DL, CS:TABLED[DI]
                    MOV CS:IC7SEG[SI], DL
                    INC SI
                    JMP X0

        BCKSPC:     DEC CS:CNTDGS
                    DEC SI
                    MOV BL,0
                    MOV CS:ICVAL[SI], BL
                    MOV CS:IC7SEG[SI], BL
                    JMP X0
                    
                    ; SAME AS THE X0 AND Z CODE ABOVE
                    
        ENTR:       MOV AL,00H  ;CHECK FOR KEY RELEASE
                    OUT PORT1C,AL 
        E1:     T10:MOV CH,CS:CNTDGS
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    JE TX
                    MOV BP,0
                    MOV BH,CS:IC7SEG[BP]
                    MOV DL,1
                T11:MOV AL,0
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:IC7SEG[BP]
                    DEC CH
                    JNZ T11
                TX: IN AL, PORT1C   
                    AND AL,0F0H        
                    CMP AL,0F0H        
                    JNZ E1
                    
        E2:     T20:MOV CH,CS:CNTDGS   ;CHECK FOR KEY PRESS
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    JE TY
                    MOV BP,0
                    MOV BH,CS:IC7SEG[BP]
                    MOV DL,1
                T21:MOV AL,0
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:IC7SEG[BP]
                    DEC CH
                    JNZ T21
                TY: MOV AL,00H
                    OUT PORT1C ,AL
                    IN AL,04H
                    AND AL,0F0H
                    CMP AL,0F0H
                    JZ E2
                    
                    MOV AL,0EH  ;CHECK FOR KEYPRESS IN ONLY COLUMN 0 BECAUSE TEST IS IN COLUMN 0
                    MOV BL,AL
                    OUT PORT1C,AL
                    IN  AL,PORT1C
                    AND AL,0F0H
                    CMP AL,0F0H
                    JNZ E3
                
                E3: OR AL,BL    ;DECODE KEY
                    MOV CX,0FH
                    MOV DI,00H
                E4: CMP AL,CS:TABLEK[DI]
                    JZ  E5
                    INC DI
                    LOOP E4
                E5: CMP DI,0CH
                    JNZ ENTR
                    JMP TESTLOOP 
                    
        TESTLOOP:   MOV AH,CS:CNTDGS
                    CMP AH,4
                    JE DIGCHK4
                    CMP AH,6
                    JE DIGCHK6
                    JMP FAIL
                    ;CHECKING IN 4 DIGIT IC DATABASE
        DIGCHK4:    MOV CX,4
                    MOV BP,0
                                                                                            
                P1: MOV AL,CS:ICVAL[BP]
                    MOV AH,CS:NANDIC[BP]
					INC BP
                    CMP AH,AL ;CHECKING FOR NAND
                    JNE P2
                    DEC CX
                    JZ TESTNAND
                    JMP P1   
                                       
                                       
                P2: MOV BP,0
                    MOV CX,4
                P3: MOV AL,CS:ICVAL[BP]
                    MOV AH,CS:ANDIC[BP]
                    INC BP
                    CMP AH,AL ;CHECKING FOR AND
                    JNE P4
                    DEC CX                          
                    JZ TESTAND
                    JMP P3

                P4: MOV BP,0
                    MOV CX,4
                P5: MOV AL,CS:ICVAL[BP]
                    MOV AH,CS:ORIC[BP]
                    INC BP
                    CMP AH,AL ;CHECKING FOR OR
                    JNE P6
                    DEC CX
                    JZ TESTOR
                    JMP P5
                            
                P6: MOV BP,0
                    MOV CX,4
                P7: MOV AL,CS:ICVAL[BP]
                    MOV AH,CS:XORIC[BP]
                    INC BP
                    CMP AH,AL ;CHECKING FOR XOR
                    JNE P8
                    DEC CX
                    JZ TESTXOR
                    JMP P7

                P8: JMP FAIL ;IF NONE OF THE ICS IN THE DATABASE MATCH, THEN FAIL

            ;CHECK THE IC NUMBER IN THE 6 DIGIT IC DATABASE
        DIGCHK6:    MOV BP,0
                    MOV CX,6
            P9:     MOV AL,CS:ICVAL[BP]
                    MOV AH,CS:XNORIC[BP]
                    INC BP
                    CMP AH,AL ;CHECKING FOR XNOR
                    JNE P10
                    DEC CX                          
                    JZ TESTXNOR
                    JMP P9

            P10:    JMP FAIL ;IF NONE OF THE ICS IN THE DATABASE MATCH, THEN FAIL

        TESTAND:    MOV AL,10001010B
                    OUT CREG2,AL ;INTIALIZE 8255(2)

                    ;ACTUAL
                    MOV AL,00
                    OUT PORT2A,AL
                    OUT PORT2C,AL ;MAKE OUTPUTS 0
                    IN AL,PORT2B  ; OUTPUT OF THE ZIF GIVEN AS INPUT TO 8255(2)
                    AND AL,3  ;CHECKING FOR 00 CONDITION
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C ;CHECKING FOR 00
                    AND AL,30H
                    CMP AL,0
                    JNE FAIL

                    MOV AL,1AH
                    OUT PORT2A,AL
                    MOV AL,2H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0
                    JNE FAIL

                    MOV AL,25H
                    OUT PORT2A,AL
                    MOV AL,1H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0
                    JNE FAIL

                    MOV AL,3FH
                    OUT PORT2A,AL
                    MOV AL,3H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    JMP PASS

                    ;TESTING FOR NAND-IC
        TESTNAND:   MOV AL,10001010B
                    OUT CREG2,AL

                    MOV AL,00
                    OUT PORT2A,AL
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,1AH
                    OUT PORT2A,AL
                    MOV AL,2H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,25H
                    OUT PORT2A,AL
                    MOV AL,1H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,3FH
                    OUT PORT2A,AL
                    MOV AL,3H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0H
                    JNE FAIL

                    JMP PASS

                    ;TESTING FOR OR-IC
        TESTOR:     MOV AL,10001010B
                    OUT CREG2,AL

                    MOV AL,00
                    OUT PORT2A,AL
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0H
                    JNE FAIL

                    MOV AL,1AH
                    OUT PORT2A,AL
                    MOV AL,2H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,25H
                    OUT PORT2A,AL
                    MOV AL,1H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,3FH
                    OUT PORT2A,AL
                    MOV AL,3H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    JMP PASS

                    ;TESTING FOR XOR-IC
        TESTXOR:    MOV AL,10001010B
                    OUT CREG2,AL

                    MOV AL,00
                    OUT PORT2A,AL
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0H
                    JNE FAIL

                    MOV AL,1AH
                    OUT PORT2A,AL
                    MOV AL,2H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,25H
                    OUT PORT2A,AL
                    MOV AL,1H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,03
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,30H
                    JNE FAIL

                    MOV AL,3FH
                    OUT PORT2A,AL
                    MOV AL,3H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,30H
                    CMP AL,0H
                    JNE FAIL

                    JMP PASS

                    ;TESTING FOR XNOR-IC
        TESTXNOR:   MOV AL,10000011B  ;CHANGING CREG TO SUITE THIS PARTICULAR IC
                    OUT CREG2,AL
            
                    ;ACTUAL TESTING
                    MOV AL,00
                    OUT PORT2A,AL
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,3H
                    CMP AL,3H
                    JNE FAIL

                    MOV AL,1AH
                    OUT PORT2A,AL
                    MOV AL,20H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,3H
                    CMP AL,0H
                    JNE FAIL

                    MOV AL,25H
                    OUT PORT2A,AL
                    MOV AL,10H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,0
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,3H
                    CMP AL,0H
                    JNE FAIL

                    MOV AL,3FH
                    OUT PORT2A,AL
                    MOV AL,30H
                    OUT PORT2C,AL
                    IN AL,PORT2B
                    AND AL,3
                    CMP AL,3
                    JNE FAIL
                    IN AL,PORT2C
                    AND AL,3H
                    CMP AL,3H
                    JNE FAIL

                    JMP PASS



                    ;TO DISPLAY FAIL
        FAIL:       MOV DI, 8000H ;FOR DELAYING BY APPROX 5S
                F01:MOV CH, 4
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    MOV BP,0
                    MOV BH,CS:FAILW[BP]
                    MOV DL,1
                F11:MOV AL,0
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:FAILW[BP]
                    DEC CH
                    JNZ F11 
                    DEC DI
                    JNZ F01
                    JMP X01
                    

                    ;TO DISPLAY PASS
        PASS:       MOV DI, 8000H ;FOR DELAYING BY APPROX 5S
                P01:MOV CH, 4
                    MOV AL,0H
                    OUT PORT1B,AL
                    CMP CH,0
                    MOV BP,0
                    MOV BH,CS:PASSW[BP]
                    MOV DL,1
                P11:MOV AL,0
                    OUT PORT1B,AL
                    MOV AL,BH
                    OUT PORT1A,AL
                    MOV AL,DL
                    OUT PORT1B,AL
                    ROL DL,1
                    INC BP
                    MOV BH,CS:PASSW[BP]
                    DEC CH
                    JNZ P11
                    DEC DI
                    JNZ P01
                    JMP X01
 
    ;END