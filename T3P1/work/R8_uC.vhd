library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.R8_pkg.all;

entity R8_uC is
    port( 
        clk     : in std_logic;
        rst     : in std_logic;
	port_ioA : inout std_logic_vector (15 downto 0);
	port_ioB : inout std_logic_vector (15 downto 0)
    );
end R8_uC;

architecture Behavioural of R8_uC is

component BidirectionalPort  is
    generic (
        DATA_WIDTH          : integer;    -- Port width in bits
        PORT_DATA_ADDR      : std_logic_vector(1 downto 0);     -- NÃO ALTERAR!
        PORT_CONFIG_ADDR    : std_logic_vector(1 downto 0);     -- NÃO ALTERAR! 
        PORT_ENABLE_ADDR    : std_logic_vector(1 downto 0);      -- NÃO ALTERAR!
	PORT_INTERRUPT_ADDR : std_logic_vector (1 downto 0)
    );
    port (  
        clk         : in std_logic;
        rst         : in std_logic; 
        
        -- Processor interface
        data_i      : in std_logic_vector (DATA_WIDTH-1 downto 0);
        data_o      : out std_logic_vector (DATA_WIDTH-1 downto 0);
        address     : in std_logic_vector (1 downto 0);     -- NÃO ALTERAR!
        rw          : in std_logic; -- 0: read; 1: write
        ce          : in std_logic;
        
        -- External interface
        port_io     : inout std_logic_vector (DATA_WIDTH-1 downto 0)
    );
end component;

type CryptoMessageData is record
	ack         : std_logic;
        data_in     : std_logic_vector(7 downto 0);
        data_out    : std_logic_vector(7 downto 0);
        data_av     : std_logic;
        keyExchange : std_logic;
        eom         : std_logic;
end record;

--memory interface
signal data_in : std_logic_vector(15 downto 0);
signal data_out: std_logic_vector(15 downto 0);
signal data_out_memory : std_logic_vector (15 downto 0);
signal data_out_peripheral : std_logic_vector (15 downto 0);
signal address : std_logic_vector(15 downto 0);
signal not_clk : std_logic;
signal ce      : std_logic;
signal rw      : std_logic;
signal ports_ce   : std_logic_vector (7 downto 0);
signal ports_rw   : std_logic_vector (7 downto 0);
signal current_port_index : integer range 0 to 7;
signal not_rw      :std_logic;
signal enMEM   :std_logic;
signal r8_interruption_in: std_logic;

type port_io_array is array (0 to 7) of std_logic_vector (15 downto 0);
signal port_io_a : port_io_array;
type files is array (0 to 1) of string (1 to 21);
signal file_names : files := ("RevolutionCalling.txt", "DoctorRockter0000.txt");

type crypto_data_array is array (0 to 1) of CryptoMessageData;
signal crypto_data_a: crypto_data_array;

--signal msg_status_crypto1, msg_status_crypto2: MessageStatus;
--signal crypto1_data, crypto2_data: CryptoMessageData;


begin

	R8: entity work.R8(Behavioural)
    	port map( 
       	clk => clk,
        rst => rst,
        
        -- Memory interface
        data_in    => data_in,
        data_out   => data_out,
        address	   => address,
        ce  => ce,
        rw         => rw,
	intr => r8_interruption_in
    );

	
	RAM: entity work.Memory(BlockRAM)
	generic map(	
 		DATA_WIDTH => 16,
       	ADDR_WIDTH  => 12,
		IMAGE => "teste_interrupt_BRAM.txt"  )
        
	port map(
		clk      => not_clk,
		data_in  => data_out,
        data_out => data_out_memory,
	    address  => address(11 downto 0),
	    en       => enMEM,	
	    wr       => not_rw 
	);
	
	not_rw <= not rw;	
	not_clk <= not clk;

signal pic_data: std_logic_vector (7 downto 0);
signal pic_ce: std_logic;

PIC: entity work.InterruptController(Behavioural)
	generic map (
		IRQ_ID_ADDR => "00",
		INT_ACK_ADDR => "01",
		MASK_ADDR => "10"
	);
	port map
	(
		clk 	=> clk,
		rst 	=> rst,
		data 	=> pic_data,
		address => address (1 downto 0),
		rw 		=> rw,
		ce 		=> pic_ce,
		intr 	=> r8_interruption_in,
		irq 	=> "0000" & port_ioB (12 downto 9)
	);

--ativa o pic só quando o processador estiver endereçando "1010" nos bits mais significativos.
pic_ce <= '1' when address(15 downto 12) = "1010" else '0';

--escreve no barramento do pic quando o mesmo estiver habilitado, o processador estiver fazendo uma escrita e o endereço do registrador do pic não for de leitura.
pic_data <= data_out(7 downto 0) when (pic_ce = '1' and rw = '1' and address (1 downto 0) /= "00") else (others => 'Z');

--mux de entrada de dados no processador
data_in <= 		x"00" & pic_data when pic_ce = '1' else
				data_out_memory when enMEM = '1' else 
				data_out_peripheral;

--habilita o ce da memoria quando o msb for 0.
enMEM <= '1' when address (15) = '0' and ce = '1' else '0';
--pega a porta com a qual o usuario deseja interagir. indice pode ser 0 a 7 (000, 001, 010.., 111)
current_port_index <= TO_INTEGER (UNSIGNED (address (14 downto 12)));

port_ioA <= port_io_a(0);
port_ioB <= port_io_a(1);

--seta os bits da porta b que são ligados no PIC para gerar interrupção.
port_io_a(1)(12 downto 11) <= crypto_data_a(0).keyExchange & crypto_data_a(1).keyExchange;

get_set_crypto_message_data: for i in 0 to 1 generate
begin
	--Only set ack when we're waiting for one (either data_av or keyExchange are 1)
	crypto_data_a(i).ack <= port_io_a(i)(15);
	port_io_a(i)(14 downto 12) <= crypto_data_a(i).keyExchange & crypto_data_a(i).data_av & crypto_data_a(i).eom; 
	--Only set data_in when key exchange is active.
	crypto_data_a(i).data_in <= port_io_a(i)(7 downto 0) when crypto_data_a(i).keyExchange = '1' else (others => 'Z');
	--Only set port io data when either key exchange or data_av are 1 and ack is 0. If ack is 1, we need to avoid conflicts in port_io'low.
	port_io_a(i)(7 downto 0) <= crypto_data_a(i).data_out when (crypto_data_a(i).ack = '0' and (crypto_data_a(i).keyExchange = '1' or crypto_data_a(i).data_av = '1')) else (others => 'Z');

	crypto: entity work.CryptoMessage (behavioral)
					generic map (15000 * (i + 1), file_names(i))
					port map (clk, rst, crypto_data_a(i).ack, crypto_data_a(i).data_in, crypto_data_a(i).data_out, crypto_data_a(i).data_av, crypto_data_a(i).keyExchange, crypto_data_a(i).eom
				);
	
end generate get_set_crypto_message_data;

generate_ports: for i in 0 to 7 generate
begin
	--seta o status de rw da porta baseado na porta selecionada, no registrador alvo da porta e na flag de rw vinda do processador, que indica operacao de escrita
	ports_rw(i) <= not_rw;
	--Habilita apenas o ce do indice da porta correta
	ports_ce(i) <= '1' when (address(15) = '1' and current_port_index = i and ce = '1') else '0';
	--gera 7 portas de perifericos usando as flags definidas acima.
	bid_port: BidirectionalPort 
	generic map (16, "10", "01", "00", "11") 
	port map (clk, rst, data_out, data_out_peripheral, address (1 downto 0), ports_rw(i), ports_ce(i), port_io_a(i));

end generate generate_ports;

end Behavioural;