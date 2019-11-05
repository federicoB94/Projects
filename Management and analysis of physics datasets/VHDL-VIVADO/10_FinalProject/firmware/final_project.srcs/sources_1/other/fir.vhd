library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity fir is
  generic(N      : integer := 32;
          N_coef : integer := 4;
          N_tap  : integer := 5 );
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic
     -- all the other in/out ports
     );
end fir;

architecture Behavioral of fir is

component ffd is
  generic (N : integer := 32);
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic;
     d_in  : in  std_logic_vector(N-1 downto 0);
     q_out : out std_logic_vector(N-1 downto 0));
end component;


begin


end Behavioral;
