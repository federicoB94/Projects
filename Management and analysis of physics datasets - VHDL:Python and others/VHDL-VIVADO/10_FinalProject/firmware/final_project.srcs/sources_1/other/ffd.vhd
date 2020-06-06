----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/26/2018 08:43:22 AM
-- Design Name: 
-- Module Name: ffd - Behavioral
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

entity ffd is
  generic (N : integer := 4);
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic;
     d_in  : in  std_logic_vector(N-1 downto 0);
     q_out : out std_logic_vector(N-1 downto 0));
end ffd;

architecture Behavioral of ffd is

begin

process(clk,rst) is
begin
   if rst = '1' then
      q_out <= (others => '0');
   elsif rising_edge(clk) then
      q_out <= d_in;
   end if;
end process;

end Behavioral;
