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

-- HIGH LOGIC LEVEL
-- y <= std_logic_vector(to_signed(1024 , y'length)); 

-- LOW LOGIC LEVEL
-- y <= std_logic_vector(to_signed(-1024, y'length));


begin

y_out <= y;

end Behavioral;
