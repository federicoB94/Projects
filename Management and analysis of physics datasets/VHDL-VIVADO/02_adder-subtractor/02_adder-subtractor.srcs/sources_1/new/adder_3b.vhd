----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 03:06:37 PM
-- Design Name: 
-- Module Name: adder_3b - rtl
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity adder_3b is
  Port ( a_in  : in std_logic_vector(2 downto 0);
         b_in  : in std_logic_vector(2 downto 0);
         y_out : out std_logic_vector(3 downto 0) );
end adder_3b;

architecture rtl of adder_3b is

component adder_1b is
  Port ( a_in  : in std_logic;
         b_in  : in std_logic;
         c_in  : in std_logic;
         y_out : out std_logic;
         c_out : out std_logic );
end component;

signal y_a1_a2, y_a2_a3 : std_logic;

begin

a1 : adder_1b port map ( a_in => a_in(0), b_in => b_in(0), c_in => '0',     y_out => y_out(0), c_out => y_a1_a2 );
a2 : adder_1b port map ( a_in => a_in(1), b_in => b_in(1), c_in => y_a1_a2, y_out => y_out(1), c_out => y_a2_a3 );
a3 : adder_1b port map ( a_in => a_in(2), b_in => b_in(2), c_in => y_a2_a3, y_out => y_out(2), c_out => y_out(3) );

end rtl;
