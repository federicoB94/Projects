----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 03:18:26 PM
-- Design Name: 
-- Module Name: add_sub - rtl
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

entity add_sub is
--  Port ( a_in );
end add_sub;

architecture rtl of add_sub is

component adder_3b is
  Port ( a_in  : in std_logic_vector(2 downto 0);
         b_in  : in std_logic_vector(2 downto 0);
         y_out : out std_logic_vector(3 downto 0) );
end component;

component mux21 is
  Port (a_in   : in std_logic;
        b_in   : in std_logic;
        sel_in : in std_logic;
        y_out  : out std_logic );
end component;

begin


end rtl;
