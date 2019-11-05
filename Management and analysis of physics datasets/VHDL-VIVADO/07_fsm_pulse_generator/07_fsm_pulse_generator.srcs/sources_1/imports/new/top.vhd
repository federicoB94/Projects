library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top is
  generic(DONE_TIME : integer := 100000000); -- in number of clock cycles -> 1 second
  Port (clk   : in  std_logic;
        --rst   : in  std_logic;
        --en_trig_in : in  std_logic;        
        gen_out    : out std_logic; -- Pulse generator output        
        pulses_in  : in  std_logic; -- Counter input
        done_out   : out std_logic);-- Calculation done
end top;

architecture Behavioral of top is

component pulses_generator 
  generic( WTIME       : integer := 150; -- in number of clock cycles
           PULSE_WIDTH : integer :=   3; -- in number of clock cycles
           PULSE_DIST  : integer :=  17  -- in number of clock cycles
  );
  Port (clk        : in  std_logic;  
        rst        : in  std_logic; 
        en_gen_in  : in  std_logic;
        y_out      : out std_logic);
end component;

component break_measure is
  generic(DONE_TIME : integer := 100); -- in number of clock cycles -> 1 second
  Port (clk   : in  std_logic;
        rst   : in  std_logic;            
        pulses_in  : in  std_logic; -- Counter input
        count_out  : out unsigned(5 downto 0);
        done_out   : out std_logic);-- Calculation done
end component;

signal rst, en_trig_in : std_logic;
signal counter : unsigned(5 downto 0);

-- Debug 
signal vio_rst, vio_en_trig : std_logic;

COMPONENT vio_1
  PORT (
    clk : IN STD_LOGIC;
    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END COMPONENT;

begin

rst <= vio_rst;
en_trig_in <= vio_en_trig;


gen: pulses_generator 
     generic map(PULSE_DIST => 21)
     port map(clk => clk, rst => rst, en_gen_in => en_trig_in, y_out => gen_out);
     
count : break_measure
     generic map (DONE_TIME => 100000000) -- in number of clock cycles -> 1 second
     port map(clk => clk, rst => rst, pulses_in => pulses_in, count_out => counter, done_out => done_out);          

vio1 : vio_1
  PORT MAP (
    clk => clk,
    probe_out0(0) => vio_rst,
    probe_out1(0) => vio_en_trig
  );

end Behavioral;
