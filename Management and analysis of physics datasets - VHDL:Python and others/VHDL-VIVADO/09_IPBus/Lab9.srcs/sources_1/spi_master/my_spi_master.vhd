library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity spi_master is
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
end spi_master;

architecture Behavioral of spi_master is

----------------------------------------
------  WRITE YOUR CODE HERE  ----------
----------------------------------------

--COMPONENT ila_0

--PORT (
--	clk : IN STD_LOGIC;

--	probe0 : IN STD_LOGIC_VECTOR(0 DOWNTO 0); 
--	probe1 : IN STD_LOGIC_VECTOR(0 DOWNTO 0); 
--	probe2 : IN STD_LOGIC_VECTOR(0 DOWNTO 0); 
--	probe3 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
--	probe4 : IN STD_LOGIC_VECTOR(0 DOWNTO 0)
--);
--END COMPONENT  ;

type state is (s_send, s_read, s_idle);

signal state_fsm : state;

signal sclk_s, mosi_s, ready_s, cs_s : std_logic;
signal start_p : std_logic;
signal bufout : std_logic_vector(TXBITS-1 downto 0); -- store READ+address (24 bits)
signal bufin  : std_logic_vector(RXBITS-1 downto 0); -- store data (8 bits)
   
begin

----------------------------------------
------  WRITE YOUR CODE HERE  ----------
-- and delete the following 3 lines  ---
----------------------------------------   
--cs <= '1'; 
--sclk <= '0';
--mosi <= miso; 
spi: process(clock, reset, txd, start, miso) is
variable tcnt, rcnt, wcnt : integer;  -- tcnt -> time counter
                                      -- rcnt -> read counter (to read the instruction, i.e. READ)
                                      -- wcnt -> write counter (to read the address)

begin


if reset = '1' then
  cs_s <= '1';
  mosi_s <= '1';
  sclk_s <= '0';
  state_fsm <= s_idle;
  ready_s <= '0';
  
elsif rising_edge(clock) then
  start_p <= start;

  case state_fsm is
  
    when s_idle =>
      --if rising_edge(start) then  -- use rising_edge only for clock signals
                                    -- initialize variable in 'reset' or befor 'if'
      tcnt := 0;
      wcnt := TXBITS - 1;
      cs_s <= '1'; --> there is not commuunication between master and slave
      ready_s <= '0';
      
      if start = '1' and start_p = '0' then  -- simulate rising_edge(start)    
        bufout <= txd;
        state_fsm <= s_send;
      else
        state_fsm <= s_idle;
      end if;
        
    when s_send => -- send the command (= READ) + address
                   -- performed TXBITS = 24 times (8 READ, 16 address)
      tcnt := tcnt + 1;  -- previously initialized at 0; used to slow down the process
      cs_s <= '0'; --> there is commuunication between master and slave
      ready_s <= '0';
      
      if tcnt = 1 then -- send info to slave (1 bit)
        sclk_s <= '0'; -- master generated clock to sincro data transmission
        mosi_s <= bufout(wcnt);  -- bufout, when the start (VIO) happens, is initialized at txd
      elsif tcnt = WTIME / 2 then
        sclk_s <= '1';  
      elsif tcnt = WTIME then -- pass to next state or to the next bit to transmit
        tcnt := 0;
        sclk_s <= '0';
        if wcnt = 0 then  -- READ+address transmitted -> pass to reading state
          wcnt := TXBITS - 1; -- variable previously initialized at TXBITS - 1
          mosi_s <= '1';
          rcnt := RXBITS - 1;
          state_fsm <= s_read;
        else -- READ+address NOT fully transmitted -> go to next bit
          wcnt := wcnt - 1;   -- variable previously initialized at TXBITS - 1
        end if;
      end if;
          
    when s_read => -- read data
                   -- performed RXBITS = 8 times
      tcnt := tcnt + 1;
      cs_s <= '0'; --> there is commuunication between master and slave
      
      if tcnt = 1 then
        sclk_s <= '0';
        bufin(rcnt) <= miso;
      elsif tcnt = WTIME / 2 then
        sclk_s <= '1';
      elsif tcnt = WTIME then
        tcnt := 0;
        sclk_s <= '0';
        if rcnt = 0 then  -- data read -> pass to idle state
          rcnt := RXBITS - 1; -- variable previously initialized at RXBITS - 1
          ready_s <= '1';
          rxd <= bufin;
          state_fsm <= s_idle;
        else -- data NOT fully read -> go to next bit
          rcnt := rcnt - 1;   -- variable previously initialized at RXBITS - 1
        end if;
      end if;
    
    end case;
end if;

sclk <= sclk_s;
mosi <= mosi_s;
ready <= ready_s;
cs <= cs_s;

end process;

--ila : ila_0
--PORT MAP (
--	clk => clock,

--	probe0(0) => cs_s,
--	probe1(0) => sclk_s,
--	probe2(0) => mosi_s,
--	probe3(0) => miso,
--	probe4(0) => ready_s
--);

end Behavioral;
