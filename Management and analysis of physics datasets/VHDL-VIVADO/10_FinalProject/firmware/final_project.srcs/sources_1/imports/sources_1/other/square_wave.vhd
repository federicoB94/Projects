library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity square_wave is
  generic( PERIOD      : integer := 20; -- in number of clock cycles
           DUTY_CYC    : integer := 50; -- in number of clock cycles
           SAMPLE_N    : integer := 1024
  );
  Port (clk        : in  std_logic;  
        rst        : in  std_logic; 
        en_gen_in  : in  std_logic;                       -- start signal
        we_out     : out std_logic;                       -- write enable for the dpram
        address_out: out std_logic_vector(9 downto 0);    -- address for the dpram 
        y_out      : out std_logic_vector(31 downto 0));  -- data to write in the dpram
end square_wave;

architecture Behavioral of square_wave is

signal y : std_logic_vector(31 downto 0);
signal en_gen_p : std_logic;

type state is (s_idle, s_high, s_low);
signal state_fsm : state;

-- HIGH LOGIC LEVEL
-- y <= std_logic_vector(to_signed(1024 , y'length)); 

-- LOW LOGIC LEVEL
-- y <= std_logic_vector(to_signed(-1024, y'length));


begin

p_fsm : process(clk, rst, en_gen_in) is
variable sample_cnt : integer; -- from 0 to SAMPLE_N-1 (acquisition time)
variable period_cnt : integer; -- from 0 to PERIOD
                               -- high state: from 0 to PERIOD*DUTY_CYC/100
                               -- low state:  from  PERIOD*DUTY_CYC/100+1 to PERIOD
constant HIGH_COUNT : integer := (PERIOD*DUTY_CYC)/100;
constant LOW_COUNT  : integer := PERIOD-HIGH_COUNT;

begin
  if rst = '1' then
    state_fsm <= s_idle;
    y <= (others => '0');
    sample_cnt := 0;
    period_cnt := 0;
    we_out <= '0';
    address_out <= (others => '0');
    en_gen_p <= '0';
  elsif rising_edge(clk) then
     en_gen_p <= en_gen_in;
     
     case state_fsm is
     
     when s_idle =>
       if en_gen_in = '1' and en_gen_p = '0' then
          state_fsm <= s_high;
       end if;
       y <= (others => '0');
       sample_cnt := 0;
       period_cnt := 0;
       we_out <= '0';
         
     when s_high =>
       if sample_cnt = SAMPLE_N-1 then
         state_fsm <= s_idle;
         we_out <= '0';
       else
         if period_cnt <  HIGH_COUNT then   
           we_out <= '1';
           state_fsm <= s_high;
         else
           state_fsm <= s_low;
         end if;
       end if;
       address_out <= std_logic_vector(to_unsigned(sample_cnt, address_out'length)); 
       y <= std_logic_vector(to_signed(1024 , y'length));
       period_cnt := period_cnt + 1;
       sample_cnt := sample_cnt + 1;
     
     when s_low =>
       if sample_cnt = SAMPLE_N-1 then
         state_fsm <= s_idle;
         we_out <= '0';
       else
         if period_cnt < PERIOD then
           we_out <= '1';
           state_fsm <= s_low;  
         else
           state_fsm <= s_high;
           period_cnt := 0;
         end if;  
       end if;
       address_out <= std_logic_vector(to_unsigned(sample_cnt, address_out'length));
       y <= std_logic_vector(to_signed(-1024, y'length));
       period_cnt := period_cnt + 1;
       sample_cnt := sample_cnt + 1;
     
     when others =>
       state_fsm <= s_idle;
     end case;
     
  end if;

end process;

y_out <= y;


end Behavioral;
