library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity spi_master_flash is
   generic (
      WTIME    : integer   := 10000;
	  TXBITS   : integer   := 32;
      RXBITS   : integer   := 8;
	  N_BYTES  : integer   := 2		
      );
   port ( 
      clock    : in  std_logic;
      reset    : in  std_logic;
      txd      : in  std_logic_vector(TXBITS-1   downto 0);
      word     : out std_logic_vector(N_BYTES*RXBITS-1 downto 0); -- new in this project
      start    : in  std_logic;
      ready    : out std_logic;
      miso     : in  std_logic;
      mosi     : out std_logic;
      sclk     : out std_logic;
      cs       : out std_logic;
	  wr_pr_o  : out std_logic;
      we_out   : out std_logic
      );
end spi_master_flash;

architecture Behavioral of spi_master_flash is

   signal bufout  : std_logic_vector(txd'range);
   signal bufin   : std_logic_vector(RXBITS-1 downto 0);
   signal mosi_s  : std_logic;
   signal cs_s    : std_logic;
   signal sclk_s  : std_logic;
   signal ready_s  : std_logic;
	
   type state_t is (s_idle, s_getbyte, s_buildword, s_stop);
   signal state   : state_t;
 
   signal s_start, s_ready_fsm : std_logic;
   signal s_txd : std_logic_vector(TXBITS-1 downto 0);
   signal s_rxd : std_logic_vector(RXBITS-1 downto 0);
   signal s_word: std_logic_vector(N_BYTES*RXBITS-1 downto 0); --NEW

component spi_master is
   generic (
      WTIME    : integer   := 100;
      TXBITS   : integer   := 16;
      RXBITS   : integer   := 8
      );
   port ( 
      clock    : in  std_logic;
      reset    : in  std_logic;
      txd      : in  std_logic_vector(TXBITS-1 downto 0);
      rxd      : out std_logic_vector(RXBITS-1 downto 0);
      start    : in  std_logic;
      ready    : out std_logic;
      miso     : in  std_logic;
      mosi     : out std_logic;
      sclk     : out std_logic;
      cs       : out std_logic 
      );
end component;    

begin

process(clock, reset) -- clock frequency 25MHz
     variable bcnt : integer;
	 variable cnt  : integer;
	 variable cnt_o : integer;
	 variable start_p : std_logic;
   begin
      if rising_edge(clock) then
         if reset = '1' then
            bcnt    := N_BYTES - 1;
			cnt     := 0;
            state   <= s_idle;
            s_start <= '0';
			ready_s <= '0';
			cnt_o := 0;
			s_word  <= (others => '0');
			we_out <= '0';
         else
            case state is
            
            when s_idle =>
			   s_start <= '0';
		       ready_s <= '0';
               --state   <= s_getbyte;
			   cnt := cnt + 1;
			   if cnt <= WTIME then
				  state <= s_idle;
			   else
				  state <= s_getbyte;
			   end if;					
            
            when s_getbyte =>
				ready_s <= '0';
				if s_ready_fsm = '1' then
				   s_start <= '0';
				   state   <= s_buildword;
				else
				   s_start <= '1';
				   s_txd(TXBITS-1 downto TXBITS-1 - 7) <= txd(TXBITS-1 downto TXBITS-1 - 7);
				   s_txd(TXBITS-1 -8 downto 0) <= std_logic_vector(to_unsigned(to_integer(unsigned(txd(TXBITS-1 -8 downto 0)) + bcnt), TXBITS -8 ));
				end if;
					
			when s_buildword =>	
			    if cnt_o < 3 then
			       we_out <= '1';
			       word <= s_word; -- NEW
                   cnt_o := cnt_o + 1;                   
                   if bcnt = 0 then
				      s_word(RXBITS-1 downto 0 ) <= s_rxd; -- NEW
                      bcnt  := N_BYTES - 1;
                      state <= s_stop;
                      ready_s <= '1';
                   else
                      s_word(((bcnt+1)*RXBITS-1) downto (bcnt*RXBITS )) <= s_rxd; -- NEW
                      bcnt  := bcnt - 1;
                      state <= s_getbyte;
                      ready_s <= '0';
                   end if; 
                else	
                   we_out <= '0';
                   cnt_o := 0;                   
                end if;	
                	    			    					
            when s_stop =>		
                  ready_s <= '1';	
                  s_start <= '0';
                  state <= s_stop;						
            end case;
            start_p := start;
         end if;
      end if;
   end process;


  flash_master : spi_master 
   generic map (
      WTIME    => WTIME,
      TXBITS   => TXBITS,
	  RXBITS   => RXBITS
      )
   port map( 
      clock    => clock,
      reset    => reset,
      txd      => s_txd,
      rxd      => s_rxd,
      start    => s_start,
      ready    => s_ready_fsm,
      miso     => miso,
      mosi     => mosi_s,
      sclk     => sclk_s,
      cs       => cs_s
      );
		
   sclk  <= sclk_s;
   mosi  <= mosi_s;
   cs    <= cs_s;		
   ready <= ready_s; 		
   wr_pr_o <= '0';		
  
   
end Behavioral;

