----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 11:30:39 AM
-- Design Name: 
-- Module Name: tb_top - Behavioral
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

entity tb_top is
--  Port ( );
end tb_top;

architecture Behavioral of tb_top is

component top is
  Port (btn_in : in std_logic_vector(3 downto 0);
        led_out : out std_logic_vector(3 downto 0) );
end component;

signal btn, led : std_logic_vector(3 downto 0);

begin

uut : top port map (btn_in => btn, led_out => led);

p1 : process
    begin
        btn(0) <= '0';
        btn(1) <= '0';
        btn(2) <= '0';
        btn(3) <= '0';
    wait for 200 ns;
        btn(0) <= '1';
    wait for 200 ns;
        btn(0) <= '0';
        btn(3) <= '1';
    wait for 200 ns;
    end process;

end Behavioral;
