----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.09.2019 23:32:08
-- Design Name: 
-- Module Name: mux21 - Behavioral
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

entity mux21 is
    generic (
    N_BITS : integer := 10
    );
    Port ( a_in : in std_logic_vector(N_BITS-1 downto 0);
           b_in : in std_logic_vector(N_BITS-1 downto 0);
           sel_in : in std_logic;
           y_out : out std_logic_vector(N_BITS-1 downto 0));
end mux21;

architecture rtl of mux21 is

begin

selp : process(a_in, b_in, sel_in)
    begin
    if sel_in = '0' then
        y_out <= a_in;
    else
        y_out <= b_in;
    end if;
end process;


end rtl;
