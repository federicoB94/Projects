----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/19/2019 03:38:13 PM
-- Design Name: 
-- Module Name: flipflop - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity flipflop is
  Port ( clk   : in std_logic;
         rst   : in std_logic;
         ff_in : in std_logic_vector;
         ff_out: out std_logic_vector );
end flipflop;

architecture Behavioral of flipflop is

begin

p_ff : process(clk, rst, ff_in) is
begin
    if rst = '1' then
        ff_out <= (others => '0');
    elsif rising_edge(clk) then
        ff_out <= ff_in;
    end if;
end process;

end Behavioral;
