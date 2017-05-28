.org #0000h
.code

start:
;inicializa o stack pointer no endereco 0900h
LDH r1, #09h
LDL r1, #00h
LDSP r1
;configura as portas para receber interrupcoes.
LDH r1, #80h
LDL r1, #00h
jsrd #PortConfig
LDH r1, #90h
LDL r1, #00h
jsrd #PortConfig
;Carrega endereço das interrupções nos devidos indices do vetor.
LDH r1, #interruptions
LDL r1, #interruptions ; r1 <- &interruptions[0]
LDH r2, #CryptoMessage1_Handler
LDL r2, #CryptoMessage1_Handler ; r2 <- &CryptoMessage1_Handler
ST r2, r1, r0 					; interruptions[0] <- &CryptoMessage1_Handler
addi r1, #1 					; r1 <- &interruptions[1]
LDH r2, #CryptoMessage2_Handler	
LDL r2, #CryptoMessage2_Handler ; r2 <- &CryptoMessage2_Handler
ST r2, r1, r0 					; interruptions[1] <- &CryptoMessage2_Handler
LDH r1, #pic_mask_address
LDL r1, #pic_mask_address
LD r1, r1, r0 					; r1 <- pic_mask_address
LDH r2, #00h
LDL r2, #FFh 					
ST r2, r1, r0 					; pic_mask_address <- 00FF
jmpd #BubbleSort

BubbleSort:
   
    ; Initialization code
    xor r0, r0, r0          ; r0 <- 0
    
    ldh r1, #array          ;
    ldl r1, #array          ; r1 <- &array
    
    ldh r2, #size           ;
    ldl r2, #size           ; r2 <- &size
    ld r2, r2, r0           ; r2 <- size
    
    add r3, r2, r1          ; r3 points the end of array (right after the last element)
    
    ldl r4, #0              ;
    ldh r4, #1              ; r4 <- 1
   
   
; Main code
scan:
    addi r4, #0             ; Verifies if there was element swaping
    jmpzd #end              ; If r4 = 0 then no element swaping
    
    xor r4, r4, r4          ; r4 <- 0 before each pass
    
    add r5, r1, r0          ; r5 points the first arrar element
    
    add r6, r1, r0          ;
    addi r6, #1             ; r6 points the second array element
    
; Read two consecutive elements and compares them    
loop:
    ld r7, r5, r0           ; r7 <- array[r5]
    ld r8, r6, r0           ; r8 <- array[r6]
    sub r2, r8, r7          ; If r8 > r7, negative flag is set
    jmpnd #swap             ; (if array[r5] > array[r6] jump)
    
; Increments the index registers and verifies is the pass is concluded
continue:
    addi r5, #1             ; r5++
    addi r6, #1             ; r6++
    
    sub r2, r6, r3          ; Verifies if the end of array was reached (r6 = r3)
    jmpzd #scan             ; If r6 = r3 jump
    jmpd #loop              ; else, the next two elements are compared


; Swaps two array elements (memory)
swap:
    st r7, r6, r0           ; array[r6] <- r7
    st r8, r5, r0           ; array[r5] <- r8
    ldl r4, #1              ; Set the element swaping (r4 <- 1)
    jmpd #continue
    
    
end:    
    halt                    ; Suspend the execution

.org #0500h
InterruptionServiceRoutine:
	;salva o contexto
	PUSHF
	PUSH r15
	PUSH r14
	PUSH r13
	PUSH r12
	PUSH r11
	PUSH r10
	PUSH r9
	PUSH r8
	PUSH r7
	PUSH r6
	PUSH r5
	PUSH r4
	PUSH r3
	PUSH r2
	PUSH r1
	 
	LDH r1, #interruptions
	LDL r1, #interruptions ; r1 <- &interruptions
	LDH r2, #pic_id_address
	LDL r2, #pic_id_address
	LD r2, r2, r0 		   ; r2 <- pic_id
	LD r1, r1, r2 		   ; r1 <- *interruptions[r2]
	JSR r1   			   ; salta para interrupção atual
	
	LDL r2, #pic_ack_address
	LDH r2, #pic_ack_address
	LD r2, r2, r0
	ST r1, r2, r0  			; write anything on pic_ack_address

	RestoreContext:
		POP r1
		POP r2
		POP r3
		POP r4
		POP r5
		POP r6
		POP r7
		POP r8
		POP r9
		POP r10
		POP r11
		POP r12
		POP r13
		POP r14
		POP r15
		POPF
		rti

PortConfig:
	LDH r2, #port_enable
	LDL r2, #port_enable
	LD r2, r2, r0
	jsrd #set_port_value ; &per0.enable <- E0FF
	LDH r2, #key_exchange_mask
	LDL r2, #key_exchange_mask
	LD r2, r2, r0
	LDL r1, #03h
	jsrd #set_port_value ;&per.interrupt <- 4000h
	LDL r1, #01h
	jsrd #set_port_value ;&per.config <- 4000h (habilita leitura no bit da interrupcao).
	rts

CryptoMessage1_Handler:
;Carrega o endereco apropriado do periferico e salta para rotina de tratamento da mensagem.
LDH r4, #80h
LDL r4, #01h
LDH r7, #msg1
LDL r7, #msg1 ; r7 <- &msg1
jsrd #HandleCryptoMessage
rts

CryptoMessage2_Handler:
;Carrega o endereco apropriado do periferico e salta para rotina de tratamento da mensagem.
LDH r4, #90h
LDL r4, #01h
LDH r7, #msg2
LDL r7, #msg2 ; r7 <- &msg2
jsrd #HandleCryptoMessage
rts

;r4 <- &port_io
HandleCryptoMessage:
	xor r6, r6, r6 ; msg_counter = 0
	LDH r5, #random_counter
	LDL r5, #random_counter
	LD r5, r5, r0
	jsrd #increment_counter
	
	LDH r14, #key_exchange_mask
	LDL r14, #key_exchange_mask
	;r14 <- key_exchange_mask
	LD r14, r14, r0
	LDH r2, #read_config
	LDL r2, #read_config
	;r2 <- read_config
	LD r2, r2, r0
	;r1 <- r4
	add r1, r4, r0
	jsrd #set_port_value ; &per.enable <- read_config
	addi r4, #1h		 ; &r4 <- &per.data
	
	LDH r2, #eom_mask
	LDL r2, #eom_mask
	LD r2, r2, r0
		
	receive_and_send_magic_number:
	LD r15, r4, r0			; r15 <- port_data
	add r1, r15, r0			; r1 <- port_data
	LDH r1, #00h
	jsrd #calculate_exp_mod
	add r3, r13, r0			; r3 <- key
	LDL r1, #06h			; r1 <- 6 (generate a magic number to send out)
	LDH r1, #00h
	jsrd #calculate_exp_mod	; r13 <- magic_number
	LDH r2, #write_config
	LDL r2, #write_config
	LD r2, r2, r0			; r2 <- write_config
	subi r4, #1				; r4 <- &per.config
	add r1, r4, r0			; r1 <- &per.config
	jsrd #set_port_value	; per.config <- write_config
	addi r1, #1				; r1 <- &per.data
	add r2, r13, r0			; r2 <- magic_number
	LDH r2, #80h			; r2 <- ack ... & magic_number
	jsrd #set_port_value 	; per.data <- r2
	LDH r2, #00h			; disable ack
	jsrd #set_port_value
	subi r1, #1				; r1 <- &per.config
	LDH r2, #read_config
	LDL r2, #read_config
	LD r2, r2, r0			; r2 <- read_config
	jsrd #set_port_value	; per.config <- read_config
	addi r1, #1				; r1 <- &per.data
	
	LDH r14, #data_av_mask
	LDL r14, #data_av_mask
	LDH r10, #eom_mask
	LDL r10, #eom_mask
	;r14 <- data_av_mask
	LD r14, r14, r0	
	;r10 <- eom_mask
	LD r10, r10, r0
	jmpd #before_read_character
	
	before_read_character:
	xor r9, r9, r9
	xor r12, r12, r12
	addi r12, #1
	jmpd #check_eom
	
	check_eom: ; while (!data_av);
		LD r15, r1, r0
		and r8, r15, r10
		jmpd #check_data_av ; if eom = 0, check data_av

	check_data_av:
		and r11, r15, r14
		jmpzd #check_eom
		jmpd #read_character
	
	read_character:
		LDH r15, #00h 			; r15 <- encrypted char
		xor r15, r15, r3		; r15 <- decrypted char
		
		add r12, r12, r0
		jmpzd #concatenate_char
		
		;r15 = encrypted_char << 8 if r12 > 0
		shift_left:
			PUSH r1
			add r1, r15, r0
			jsrd #shift_left_8bits	; r15 <- r15 << 8
			add r15, r13, r0
			POP r1
		
		concatenate_char:
			or r9, r15, r9
			
		; if (eom), past ack, as it is due to store_character()
		add r8, r8, r0
		jmpzd #send_ack
		jmpd #store_character
		
		send_ack:
			;send ack back to crypto message
			LDH r2, #80h
			jsrd #set_port_value
			LDH r2, #00h			; ack = 0
			jsrd #set_port_value
			subi r12, #1
			jmpnd #store_character
			jmpd #check_eom
	
	store_character:
		ST r9, r7, r6			; msg[i] <- decrypted char
		addi r6, #1		; i++
		add r8, r8, r0
		; if (!eom), keep receiving characters, else jump back to magic key routine.
		jmpzd #before_read_character
		;send ack back to crypto message
		LDH r2, #80h
		jsrd #set_port_value
		LDH r2, #00h			; ack = 0
		jsrd #set_port_value
		PUSH r1
		LDH r1, #random_counter
		LDL r1, #random_counter	; r1 <- &random_counter
		ST r5, r1, r0	;save random counter to use later.
		POP r1
		rts

;r1 -> &port_address (including register)
;r2 -> port_value
set_port_value:
	ST r2, r1, r0
	rts
	
;r1 -> value
;r13 -> return	
shift_left_8bits:
	PUSH r15
	PUSH r14
	add r14, r1, r0
	xor r15, r15, r15
	LDL r15, #08h
	shift_left_8bits_loop:
	sl0 r14, r14
	subi r15, #1
	jmpzd #shift_left_8bits_before_return
	jmpd #shift_left_8bits_loop
	
	shift_left_8bits_before_return:
		add r13, r14, r0
		POP r14
		POP r15
		rts

;r1 -> 'a' value
;r13 <- value
calculate_exp_mod:
	PUSH r11
	PUSH r12
	PUSH r14
	PUSH r15
	;r15 <- q
	LDH r15, #00h
	LDL r15, #FBh
	;r14 <- b_mask
	LDH r14, #00h
	LDL r14, #80h
	;r12 <- i
	LDH r12, #00h
	LDL r12, #08h
	;r13 <- f = 1 (function return)
	xor r13, r13, r13
	addi r13, #1

	calculate_exp_mod_loop:
		;f <- f*f
		mul r13, r13
		mfh r13
		mfl r13
		;f <- f mod q
		div r13, r15
		ldh r13, #00h
		mfl r13
		and r11, r14, r5
		jmpzd #calculate_exp_mod_shift_mask
		;f <- f*a
		mul r13, r1
		mfh r13
		mfl r13
		;f <- f mod q
		div r13, r15
		ldh r13, #00h
		mfl r13

	calculate_exp_mod_shift_mask:
		;bitmask >> 1
		sr0 r14, r14
		;if (i = 0) return
		subi r12, #1
		jmpzd #calculate_exp_mod_before_return
		jmpd #calculate_exp_mod_loop

	calculate_exp_mod_before_return:
		POP r15
		POP r14
		POP r12
		POP r11
		rts

increment_counter:
	PUSH r15
	LDH r15, #counter_max_value
	LDL r15, #counter_max_value
	LD r15, r15, r0
	;counter <- (counter % counter_max_value) + 1
	DIV r5, r15
	LDH r5, #00h
	mfl r5 
	addi r5, #1
	POP r15
	rts
		
.endcode

.org #800h
.data
	pic_id_address: db #A000h
	pic_ack_address: db #A001h
	pic_mask_address: db #A002h
	interruptions: db #00h, #00h, #00h, #00h, #00h, #00h, #00h, #00h
    array:     db #50h, #49h, #48h, #47h, #46h, #45h, #44h, #43h, #42h, #41h, #40h, #39h, #38h, #37h, #36h, #35h, #34h, #33h, #32h, #31h, #30h, #29h, #28h, #27h, #26h, #25h, #24h, #23h, #22h, #21h, #20h, #19h, #18h, #17h, #16h, #15h, #14h, #13h, #12h, #11h, #10h, #9h, #8h, #7h, #6h, #5h, #4h, #3h, #2h, #1h
    size:      db #50    ; 'array' size 
	counter_max_value: 	db #00FBh
	key_exchange_mask:  db #4000h
	data_av_mask: 		db #2000h
	eom_mask: 			db #1000h
	read_config:  		db #70FFh
	write_config: 		db #7000h
	port_enable: 		db #F0FFh
	msg1:  db  #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0
	msg2:  db  #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0
	random_counter:			db #0, #0
.enddata