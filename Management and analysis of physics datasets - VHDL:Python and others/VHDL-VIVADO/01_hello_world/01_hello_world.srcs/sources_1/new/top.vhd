----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 10:18:26 AM
-- Design Name: 
-- Module Name: top - Behavioral
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

entity top is
  Port (btn_in : in std_logic_vector(3 downto 0);
        led_out : out std_logic_vector(3 downto 0) );
end top;

architecture Behavioral of top is

begin

led_out(0) <= btn_in(0);
led_out(1) <= btn_in(1) and btn_in(2);
led_out(2) <= btn_in(3);
led_out(3) <= btn_in(3);

end Behavioral;
