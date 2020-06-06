----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 02:58:37 PM
-- Design Name: 
-- Module Name: adder_1b - rtl
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

entity adder_1b is
  Port ( a_in  : in std_logic;
         b_in  : in std_logic;
         c_in  : in std_logic;
         y_out : out std_logic;
         c_out : out std_logic );
end adder_1b;

architecture rtl of adder_1b is

begin

y_out <= a_in xor b_in xor c_in;
c_out <= (a_in and b_in) or (a_in and c_in) or (b_in and c_in);

end rtl;
