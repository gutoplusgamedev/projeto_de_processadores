-------------------------------------------------------------------------
--
--  R8 PROCESSOR   -  GOLD VERSION  -  05/JAN/2017
--
--  moraes - 30/09/2001  - project start
--  moraes - 22/11/2001  - instruction decoding bug correction
--  moraes - 22/03/2002  - store instruction correction            
--  moraes - 05/04/2003  - SIDLE state inclusion in the control unit
--  calazans - 02/05/2003  - translation of comments to English. Names of
--    some signals, entities, etc have been changed accordingly
--  carara - 03/2013 - project split in several files. Each entity is described in a file with the same name.
--  carara - 5/01/2017 - library std_logic_unsigned replaced by numeric_std
--
--  Notes: 1) In this version, the register bank is designed using 
--    for-generate VHDL construction
--         2) The top-level R8 entity is
--
--      entity R8 is
--            port( clk,rst: in std_logic;
--                  data_in:  in  std_logic_vector(15 downto 0);    -- Data from memory
--                  data_out: out std_logic_vector(15 downto 0);    -- Data to memory
--                  address: out std_logic_vector(15 downto 0);     -- Address to memory
--                  ce,rw: out std_logic );                         -- Memory control
--      end R8;
-- 
-------------------------------------------------------------------------



-------------------------------------------------------------------------
-- Design unit: R8
-- Description: Top-level instantiation of the R8 data and control paths
-------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.R8_pkg.all;

entity R8 is
    port( 
        clk     : in std_logic;
        rst     : in std_logic;
        
        -- Memory interface
        data_in : in std_logic_vector(15 downto 0);
        data_out: out std_logic_vector(15 downto 0);
        address : out std_logic_vector(15 downto 0);
        ce      : out std_logic;
        rw      : out std_logic;
	intr	: in  std_logic
    );
end R8;

architecture Behavioural of R8 is

    -- Data path multiplexers output
    signal opA, opB, s1, s2: std_logic_vector(15 downto 0);

	signal high_reg, low_reg: std_logic_vector (7 downto 0);
	
	signal div_result, mod_result, div_mod_concat: std_logic_vector(15 downto 0);

	signal mul_result: std_logic_vector(31 downto 0);
	
	signal adder_opA, adder_opB, adder_out: std_logic_vector (16 downto 0);
    
	signal flag : std_logic_vector (3 downto 0);
	
	signal negativeFlag, carryFlag, zeroFlag, overflowFlag : std_logic;
	
	signal adder_carryIn : integer;
    	
    -- Register file and ALU outputs
    signal outALU: std_logic_vector(15 downto 0);

	signal instructionFormat1, instructionFormat2 : boolean;

	type RegisterArray is array (natural range <>) of std_logic_vector (15 downto 0);
	signal registerFile : RegisterArray (0 to 15);
	signal instruction_register, ra_register, rb_register, alu_register, pc_register, sp_register : std_logic_vector (15 downto 0);
	
	signal decodedInstruction : instruction;
	
	type State is (Sidle, Sfetch, Sreg, Shalt, Salu, Srts, Spop, Sldsp, Sld, Sst, Swbk, Sjmp, Ssbrt, Spush, Srti);
    signal currentState : State;
    
	signal en_intr, interrupt, intr_trat : std_logic; --ativar interrupcao, pedido de interrupcao
	
	signal flags_16bit: std_logic_vector(15 downto 0); --pushf
begin
    
    ----------------------------------------------------------------------------------------------------------
    -- Data path stages
    --      Finstruction_registerst stage : instruction fetch (common to all instructions)
    --      Second stage: instruction decode and operands fetch (common to all instructions)
    --      Thinstruction_registerd stage : ALU operation (common to all instructions, except for HALT and NOP)
    --      Fouth stage : instruction execution. (depends on the specific type of operation, may not exist)
    ----------------------------------------------------------------------------------------------------------
    
    
    --==============================================================================
    -- Finstruction_registerst stage components
    --==============================================================================
	
	interrupt <= '1' when (rising_edge(clk) and en_intr = '1' and intr = '1') or (interrupt = '1' and intr_trat = '0') else '0'; 
	
	decodedInstruction <=   ADD     when instruction_register(15 downto 12) = x"0" else
                            SUB     when instruction_register(15 downto 12) = x"1" else
                            AAND    when instruction_register(15 downto 12) = x"2" else
                            OOR     when instruction_register(15 downto 12) = x"3" else
                            XXOR    when instruction_register(15 downto 12) = x"4" else
                            ADDI    when instruction_register(15 downto 12) = x"5" else
                            SUBI    when instruction_register(15 downto 12) = x"6" else
                            LDL     when instruction_register(15 downto 12) = x"7" else
                            LDH     when instruction_register(15 downto 12) = x"8" else
                            LD      when instruction_register(15 downto 12) = x"9" else
                            ST      when instruction_register(15 downto 12) = x"A" else
                            SL0     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"0" else
                            SL1     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"1" else
                            SR0     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"2" else
                            SR1     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"3" else
                            NOT_A   when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"4" else
                            NOP     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"5" else
                            HALT    when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"6" else
                            LDSP    when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"7" else
                            RTS     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"8" else
                            POP     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"9" else
                            PUSH    when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"A" else 
               		    MUL     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"B" else
			    DIV     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"C" else
			    MFH     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"D" else
			    MFL     when instruction_register(15 downto 12) = x"B" and instruction_register(3 downto 0) = x"E" else
				
				RTI		when instruction_register(15 downto 12) = x"C" and instruction_register(3 downto 0) = x"C" else
                            -- Jump instructions (18). 
                            -- Here the status flags are tested to jump or not
                            JUMP_R  when instruction_register(15 downto 12) = x"C" and (
                                     instruction_register(3 downto 0) = x"0" or                           -- JMPR
                                    (instruction_register(3 downto 0) = x"1" and negativeFlag = '1') or   -- JMPNR
                                    (instruction_register(3 downto 0) = x"2" and zeroFlag = '1') or       -- JMPZR
                                    (instruction_register(3 downto 0) = x"3" and carryFlag = '1') or      -- JMPCR
                                    (instruction_register(3 downto 0) = x"4" and overflowFlag = '1')      -- JMPVR
                                    ) else 

                            JUMP_A  when instruction_register(15 downto 12) = x"C" and (
                                     instruction_register(3 downto 0) = x"5" or                           -- JMP
                                    (instruction_register(3 downto 0) = x"6" and negativeFlag = '1') or   -- JMPN
                                    (instruction_register(3 downto 0) = x"7" and zeroFlag = '1') or       -- JMPZ
                                    (instruction_register(3 downto 0) = x"8" and carryFlag = '1') or      -- JMPC
                                    (instruction_register(3 downto 0) = x"9" and overflowFlag = '1')      -- JMPV
                                    ) else 

                            JUMP_D  when instruction_register(15 downto 12) = x"D" or (                           -- JMPD
                                        instruction_register(15 downto 12) = x"E" and ( 
                                            (instruction_register(11 downto 10) = "00" and negativeFlag = '1') or -- JMPND
                                            (instruction_register(11 downto 10) = "01" and zeroFlag = '1') or     -- JMPZD
                                            (instruction_register(11 downto 10) = "10" and carryFlag = '1') or    -- JMPCD
                                            (instruction_register(11 downto 10) = "11" and overflowFlag = '1')    -- JMPVD
                                        )   
                                    )  else 
			    PUSHF when instruction_register(15 downto 12) = x"C" and instruction_register(3 downto 0) = x"D" else
			    POPF when instruction_register(15 downto 12) = x"C" and instruction_register(3 downto 0) = x"E" else
                            
							JSRR  when instruction_register(15 downto 12) = x"C" and instruction_register(3 downto 0) = x"A" else
                            JSR   when instruction_register(15 downto 12) = x"C" and instruction_register(3 downto 0) = x"B" else
                            JSRD  when instruction_register(15 downto 12) = x"F" else

                            NOP;

	-- The target register is not source
    instructionFormat1 <= true when decodedInstruction=MFH or decodedInstruction=MFL or decodedInstruction=ADD or decodedInstruction=SUB or decodedInstruction=AAND or decodedInstruction=OOR or decodedInstruction=XXOR or decodedInstruction=NOT_A or decodedInstruction=SL0 or decodedInstruction=SR0 or decodedInstruction=SL1 or decodedInstruction=SR1 else false;

    -- The target register is ALSO source
    instructionFormat2 <= true when decodedInstruction=ADDI or decodedInstruction=SUBI or decodedInstruction=LDL or decodedInstruction=LDH else false; 
   
		--==============================================================================
    -- Thinstruction_registerd stage components
    --==============================================================================
    
	s1 <= registerFile(TO_INTEGER(UNSIGNED(instruction_register(7 downto 4))));

    -- Selects the read register 2 (Rtarget or Rsource2)         
	s2 <=  	registerFile(TO_INTEGER(UNSIGNED(instruction_register(11 downto 8)))) when decodedInstruction = MUL or decodedInstruction = DIV or instructionFormat2 or decodedInstruction = PUSH or currentState = Sst else
			registerFile(TO_INTEGER(UNSIGNED(instruction_register(3 downto 0))));

	
    -- Selects the A operand for the ALU    
    opA <= instruction_register when instructionFormat2 or decodedInstruction = JUMP_D or decodedInstruction = JSRD else
	   x"00" & high_reg when decodedInstruction = MFH else
	   x"00" & low_reg when decodedInstruction = MFL else
           ra_register;   -- MUX connected to the ALU 'A' input    
      
    -- Selects the B operand for the ALU, or memory
    opB <=  sp_register when decodedInstruction = RTS or decodedInstruction = POP or decodedInstruction = POPF or decodedInstruction = RTI else     -- MUX connected to the ALU 'B' input   
            pc_register when decodedInstruction=JUMP_R or decodedInstruction=JUMP_A or decodedInstruction=JUMP_D or decodedInstruction=JSRR or decodedInstruction=JSR or decodedInstruction=JSRD else 
            rb_register;
         
		 
	-- Sets the adder A input 
    adder_opA <=    "000000000" & opA(7 downto 0)                      when decodedInstruction = ADDI else    -- unsigned add
                    not ("000000000" & opA(7 downto 0))                when decodedInstruction = SUBI else    -- unsigned sub           
                    '0' & opA(11) & opA(11) & opA(11) & opA(11) & opA(11 downto 0)  when decodedInstruction = JSRD else    -- signal extention
                    '0' & opA(9) & opA(9) & opA(9) & opA(9) & opA(9) & opA(9) & opA(9 downto 0) when decodedInstruction = JUMP_D else -- signal extention
                    '0' & opA; -- ADD, SUB, LD, ST
             
    -- Sets the adder B input
    adder_opB <=  not ('0' & opB)  when decodedInstruction = SUB else '0' & opB; 
	
	adder_carryIn <= 1 when decodedInstruction = SUB or decodedInstruction = SUBI else 0;
	
    adder_out <= std_logic_vector(unsigned(adder_opB) + unsigned(adder_opA) + adder_carryIn);

	mul_result <= std_logic_vector(unsigned(opA) * unsigned(opB));

	div_result <= std_logic_vector (unsigned(opB) / unsigned(opA));
	mod_result <= std_logic_vector (unsigned(opB) mod unsigned(opA));

	div_mod_concat(15 downto 8) <= div_result (7 downto 0);
	div_mod_concat(7 downto 0) <= mod_result (7 downto 0);

	flags_16bit <= x"000" & overflowFlag & carryFlag & negativeFlag & zeroFlag;
	
    outALU <= x"0500" when decodedInstruction = JSRD and intr_trat = '1' else -- interrupcao
			opA and opB                         	when decodedInstruction = AAND else  
            opA or  opB                         	when decodedInstruction = OOR  else   
            opA xor opB                         	when decodedInstruction = XXOR else  
            opB(15 downto 8) & opA(7 downto 0)  	when decodedInstruction = LDL   else  -- A: immediate operand (wrapped in the instruction)
            opA(7 downto 0)  & opB(7 downto 0)  	when decodedInstruction = LDH   else  -- A: immediate operand (wrapped in the instruction)
            opA(14 downto 0) & '0'            	when decodedInstruction = SL0   else
            opA(14 downto 0) & '1'            	when decodedInstruction = SL1   else
            '0' & opA(15 downto 1)            	when decodedInstruction = SR0   else
            '1' & opA(15 downto 1)            	when decodedInstruction = SR1   else
            not opA                           	when decodedInstruction = NOT_A  else 
            STD_LOGIC_VECTOR(UNSIGNED(opB) + 1)	when decodedInstruction = RTS or decodedInstruction = POP or decodedInstruction = POPF or decodedInstruction = RTI else  
            opA                               	when decodedInstruction = MFH or decodedInstruction = MFL or decodedInstruction = JUMP_A or decodedInstruction = JSR  or decodedInstruction = LDSP else      
            mul_result	(15 downto 0)		when decodedInstruction = MUL else
	    div_mod_concat			when decodedInstruction = DIV else
	    flags_16bit				when decodedInstruction = PUSHF else
		adder_out (15 downto 0);     -- by default the ALU operation is adder_out!!
		
    -- Generates the zero flag
    flag(1) <= '1' when UNSIGNED(outALU) = 0 else '0';
    
    -- Generates the negative flag
    flag(0) <= '1' when outALU(15) = '1' else '0';
	
	flag(2) <= '1' when adder_out (16) = '1' else '0';
	
	flag(3) <= '1' when (adder_opA(15)=adder_opB(15) and adder_opA(15)/=outALU(15)) else '0';
    
    -- Controls the memory access
    --      rw = 0: write
    --      rw = 1: read


    -- Status flags register (n, z, c, v) depends on the ALU output
    process (clk, rst, decodedInstruction, currentState)
    begin
        if rst = '1' then
			currentState <= Sidle;  
			for i in 0 to 15 loop   				
				registerFile(i) <= (others => '0');
			end loop;   
			en_intr <= '1';
			
        elsif rising_edge(clk) then
		         
			case currentState is
                  
            when Sidle =>  
				for i in 0 to 15 loop   				
					registerFile(i) <= (others => '0');
				end loop;   
				instruction_register <= (others => '0');
				ra_register <= (others => '0');
				rb_register <= (others => '0');
				alu_register <= (others => '0');
				pc_register <= (others => '0');
				sp_register  <= (others => '0');
				en_intr <= '1';
				intr_trat <= '0';
                currentState <= Sfetch;
       
            
            -- Finstruction_registerst clock cycle after reset and after each instruction ends execution
            when Sfetch =>  
				
				if interrupt = '1' then
					intr_trat <= '1';
					en_intr <= '0'; --desabilita interrupcao 
					instruction_register <= x"F500";
					
				else
					instruction_register <= data_in;
					pc_register <= STD_LOGIC_VECTOR(UNSIGNED(pc_register) + 1);
					
				end if;
					currentState <= Sreg;
          
            -- Second clock cycle of every instruction
            when Sreg =>   
                if decodedInstruction = HALT then      -- HALT fount => stop generating microinstructions
                    currentState <= Shalt;
                else
					ra_register <= registerFile(TO_INTEGER(UNSIGNED(instruction_register(7 downto 4))));
					if instructionFormat2 or decodedInstruction = PUSH or currentState = Sst then
						rb_register <= registerFile(TO_INTEGER(UNSIGNED(instruction_register(11 downto 8))));
					else
						rb_register <= registerFile(TO_INTEGER(UNSIGNED(instruction_register(3 downto 0))));
					end if;
                   currentState <= Salu;
                end if;
                
  
            
            -- Thinstruction_registerd clock cycle of every instruction - there are 9 distinct destination states from here
            when Salu =>
				if (decodedInstruction = MUL or decodedInstruction = DIV) then
					high_reg <= outALU (15 downto 8);
					low_reg <= outALU (7 downto 0);
				else
					alu_register <= outALU;
				end if;

				if (instructionFormat1 or decodedInstruction = ADDI or decodedInstruction = SUBI) then
					negativeFlag <= flag(0);
					zeroFlag <= flag(1);  
				end if;
        
				if (decodedInstruction = ADD or decodedInstruction = ADDI or decodedInstruction = SUB or decodedInstruction = SUBI) then
					carryFlag <= flag(2);      
					overflowFlag <= flag(3);   
				end if; 
					
                if decodedInstruction = PUSH or decodedInstruction = PUSHF then   
                    currentState <= Spush;
                
                elsif decodedInstruction = POP or decodedInstruction = POPF then   
                    currentState <= Spop;
                
                elsif decodedInstruction = RTS then   
                    currentState <= Srts;
					
				elsif decodedInstruction = RTI then
					currentState <= Srti;
					
                elsif decodedInstruction = LDSP then   
                    currentState <= Sldsp;
                
                elsif decodedInstruction = LD then   
                    currentState <= Sld;
                      
                elsif decodedInstruction = ST then   
                    currentState <= Sst;
                      
                elsif instructionFormat1 or instructionFormat2 then   
                    currentState <= Swbk;
                
                elsif decodedInstruction = JUMP_R or decodedInstruction = JUMP_A or decodedInstruction = JUMP_D then   
                    currentState <= Sjmp;
                      
                elsif decodedInstruction = JSRR or decodedInstruction = JSR or decodedInstruction = JSRD then   
                    currentState <= Ssbrt; 
            
                else    -- ** ATTENTION ** NOP and jumps with corresponding flag=0 execute in just 3 clock cycles 
                    currentState <= Sfetch;   
                end if;
  
            
            -- Fourth clock cycle of several instructions 
            when Spop | Srts | Sldsp | Sld | Sst | Swbk | Sjmp | Ssbrt | Spush | Srti=>  
				if currentState = Srti then
					en_intr <= '1'; -- habilita interrupcao 
				end if;
				if currentState = Srts or currentState = Srti then
					pc_register <= data_in;
				elsif currentState = Sjmp or currentState = Ssbrt then
					pc_register <= alu_register;
				end if;

				if currentState = Sldsp  or currentState = Srts or currentState = Ssbrt or currentState = Spush or currentState = Spop or currentState = Srti then
					if decodedInstruction = JSRR or decodedInstruction = JSR or decodedInstruction = JSRD or decodedInstruction = PUSH or decodedInstruction = PUSHF  then
						sp_register <= STD_LOGIC_VECTOR(UNSIGNED(sp_register) - 1);
					else
						sp_register <= alu_register;
					end if;
				end if;
				intr_trat <= '0';
				currentState <= Sfetch;    -- Go back to fetch        
            
            -- The HALT instruction locks the processor until external reset occurs
            when Shalt  =>  
                currentState <= Shalt;    
        end case;  
	
			-- Generate the 16 registers
			for i in 0 to 15 loop   				
				if  TO_INTEGER(UNSIGNED(instruction_register(11 downto 8)))= i and (currentState = Swbk or currentState = Sld or currentState = Spop) then
					if decodedInstruction = LD or decodedInstruction = POP then
						registerFile(i) <= data_in;
					elsif decodedInstruction = POPF then -- If the instruction is POPF we don't write to register. We overwrite the value of flags so we can correctly execute any jump instructions.
						overflowFlag <= data_in(3);
						carryFlag <= data_in(2);
						negativeFlag <= data_in(1);
						zeroFlag <= data_in(0);
					elsif (decodedInstruction /= DIV and decodedInstruction /= MUL) then
						--We dont want to store data in the registers when the instruction is MUL or DIV
						if (decodedInstruction = MFH) then
							registerFile(i)(15 downto 8) <= alu_register (7 downto 0);
						elsif (decodedInstruction = MFL) then
							registerFile(i)(7 downto 0) <= alu_register (7 downto 0);
						else
							registerFile(i) <= alu_register;
						end if;
					end if;
				end if;
			end loop;       
			
			if currentState = Sreg then
				ra_register <= s1;
				rb_register <= s2;
			end if;
		end if;
        end process;
    
    --==============================================================================
    -- Fourth stage components
    --==============================================================================
	--setting outputs
	
    -- Selection of who addresses the external memory   
    address <=  sp_register when currentState = Spush or  currentState = Ssbrt or currentState = Srti else  -- In subroutine call memory is addressed by SP to store PC
                pc_register when currentState = Sfetch else    -- In instruction fetch memory is addressed by PC
				alu_register;          				-- RALU register. Used for LD/ST instructions. 
				
	-- Data selection to memory storing
	-- instruction_register(15 downto 12) = x"A": ST instruction
	data_out <=  s2 when instruction_register(15 downto 12)= x"A" else
		alu_register when decodedInstruction = PUSHF  -- alu_register will contain the flags
	else opB;    -- MUX connected to memory data bus

	ce <= '1' when rst = '0' and (currentState = Sfetch or currentState = Srts or currentState = Srti or currentState = Spop or currentState = Sld or currentState = Ssbrt or currentState = Spush or currentState = Sst) else '0';
    rw <= '1' when currentState = Sfetch or currentState = Srts or currentState = Srti or currentState = Spop or currentState = Sld else '0';
	
end Behavioural;