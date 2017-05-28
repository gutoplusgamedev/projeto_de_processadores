library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity BidirectionalPort  is
    generic (
        DATA_WIDTH          : integer;    -- Port width in bits
        PORT_DATA_ADDR      : std_logic_vector(1 downto 0);     -- NÃO ALTERAR!
        PORT_CONFIG_ADDR    : std_logic_vector(1 downto 0);     -- NÃO ALTERAR! 
        PORT_ENABLE_ADDR    : std_logic_vector(1 downto 0);      -- NÃO ALTERAR!
	PORT_INTERRUPT_ADDR : std_logic_vector(1 downto 0)
    );
    port (  
        clk         : in std_logic;
        rst         : in std_logic; 
        
        -- Processor interface
        data_i      : in std_logic_vector (DATA_WIDTH-1 downto 0);
        data_o      : out std_logic_vector (DATA_WIDTH-1 downto 0);
        address     : in std_logic_vector (1 downto 0);     -- NÃO ALTERAR!
        rw          : in std_logic; -- 0: read from peripherical; 1: write to peripherical
        ce          : in std_logic;
        
        -- External interface
        port_io     : inout std_logic_vector (DATA_WIDTH-1 downto 0)
    );
end BidirectionalPort ;


architecture Behavioral of BidirectionalPort  is
signal register_index: integer range 0 to 3;
type registers is array (0 to 4) of std_logic_vector (DATA_WIDTH-1 downto 0);
signal regs: registers;
alias port_data: std_logic_vector (DATA_WIDTH-1 downto 0) is regs (TO_INTEGER (UNSIGNED (PORT_DATA_ADDR)));
alias port_config: std_logic_vector (DATA_WIDTH-1 downto 0) is regs (TO_INTEGER (UNSIGNED (PORT_CONFIG_ADDR)));
alias port_enable: std_logic_vector (DATA_WIDTH-1 downto 0) is regs (TO_INTEGER (UNSIGNED (PORT_ENABLE_ADDR)));
alias port_interruption: std_logic_vector (DATA_WIDTH-1 downto 0) is regs (TO_INTEGER (UNSIGNED (PORT_INTERRUPT_ADDR)));

alias synch: std_logic_vector (DATA_WIDTH-1 downto 0) is regs (4);

begin

register_index <= TO_INTEGER (UNSIGNED (address));

--seta o data out de acordo com o endereco do registrador
data_o <= regs(register_index) when (ce = '1' and register_index < 3) else (others => 'Z');

set_asynchronous: for i in 0 to DATA_WIDTH-1 generate
	--seta os bits do barramento para ser and entre o dado, a config negada (que indica leitura) e o port enable.
	--bits que estiverem com a flag de leitura desligada retornarao 0.
	port_io(i) <= port_data(i) when (port_config(i) = '0' and port_enable(i) = '1') else 'Z';
	--interruptions(i) <= '1' when (port_interruption(i) = '1' and port_config(i) = '1' and port_enable(i) = '1' and port_data(i) = '1') else '0';
end generate set_asynchronous;

process (clk, rst)
begin

if (rst = '1') then
	port_data <= x"0000";
	port_config <= x"0000";
	port_enable <= x"0000";
	port_interruption <= x"0000";
	synch <= x"0000";
end if;

for i in 0 to DATA_WIDTH-1 loop
	--seta o reg de synch bit a bit
	if (rising_edge (clk)) then
		if (ce = '1') then
			--permite escrever no registrador caso ele nao seja de dados.
			if (register_index /= 2) then
				regs(register_index)(i) <= data_i(i);
			end if;
		end if;
		--se o bit for de escrita (no periferico) and o bit estiver habilitado, o synch recebe o barramento
		if (port_config(i) = '1' and port_enable(i) = '1') then
			synch(i) <= port_io(i);
		else
			synch(i) <= 'Z';
		end if;
			
		if (port_enable(i) = '1') then
			if (port_config(i) = '1' and rw = '0') then
				port_data(i) <= synch(i);
			elsif (port_config (i) = '0' and rw = '1' and ce = '1' and address = PORT_DATA_ADDR) then
				port_data(i) <= data_i(i);
			end if;
		end if;
	end if;
end loop;

end process;
end Behavioral;