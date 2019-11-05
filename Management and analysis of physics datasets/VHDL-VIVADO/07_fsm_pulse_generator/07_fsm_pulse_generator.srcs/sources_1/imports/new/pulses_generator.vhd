library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity pulses_generator is
  generic( WTIME       : integer := 150; -- in number of clock cycles
           PULSE_WIDTH : integer :=   3; -- in number of clock cycles
           PULSE_DIST  : integer :=  17  -- in number of clock cycles
  );
  Port (clk        : in  std_logic;  
        rst        : in  std_logic; 
        en_gen_in  : in  std_logic;
        y_out      : out std_logic);
end pulses_generator;

architecture Behavioral of pulses_generator is


--COMPONENT vio_0
--  PORT (
--    clk : IN STD_LOGIC;
--    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
--  );
--END COMPONENT;

--COMPONENT ila_0
--PORT (
--	clk : IN STD_LOGIC;
--	probe0 : IN STD_LOGIC_VECTOR(0 DOWNTO 0); 
--	probe1 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
--	probe2 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
--	probe3 : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
--);
--END COMPONENT  ;

signal en_gen_p : std_logic;
signal y : std_logic;

type state is (s_idle, s_wait, s_1_pulse, s_distance, s_2_pulse);
signal state_fsm : state;

begin


p_fsm : process(clk, rst, en_gen_in) is
variable cnt : integer;
begin
   if rst = '1' then
      state_fsm <= s_idle;
      y <= '0';
      cnt := 0;
   elsif rising_edge(clk) then
      en_gen_p <= en_gen_in;
      case state_fsm is
      when s_idle =>
         if en_gen_in = '1' and en_gen_p = '0' then
            state_fsm <= s_wait;
         else
            state_fsm <= s_idle;
         end if;
         y <= '0';
         cnt:= 0;         
     when s_wait =>       
        if cnt < WTIME - 1 then
           cnt := cnt +1;
           state_fsm <= s_wait;
        else
           cnt := 0;
           state_fsm <= s_1_pulse;
        end if;
        y <= '0';
     when s_1_pulse =>       
        if cnt < PULSE_WIDTH - 1 then
           cnt := cnt + 1;
           state_fsm <= s_1_pulse;
        else
           cnt := 0;
           state_fsm <= s_distance;
        end if;
        y <= '1';       
    when s_distance =>       
        if cnt < PULSE_DIST - 1 then
           cnt := cnt +1;
           state_fsm <= s_distance;
        else
           cnt := 0;
           state_fsm <= s_2_pulse;
        end if;
        y <= '0';
     when s_2_pulse =>       
        if cnt < PULSE_WIDTH - 1 then
           cnt := cnt + 1;
           state_fsm <= s_2_pulse;
        else
           cnt := 0;
           state_fsm <= s_idle;
        end if;
        y <= '1';           
     when others => 
        state_fsm <= s_idle;
     end case;
   end if;
end process;

y_out <= y;

--vio : vio_0
--  PORT MAP (
--    clk => clk,
--    probe_out0(0) => en_gen_in
--  );
 
--ila : ila_0
--  PORT MAP (
--      clk => clk,  
--      probe0(0) => en_gen_in, 
--      probe1(0) => en_gen_p,
--      probe2(0) => y,
--      probe3    => std_logic_vector(to_unsigned(state'pos(state_fsm),3))
--  );    
  
end Behavioral;
