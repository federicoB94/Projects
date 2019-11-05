----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/20/2019 10:09:56 AM
-- Design Name: 
-- Module Name: counter - Behavioral
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

entity counter is
  generic(WTIME : integer := 50000000);
  Port (clk   : in  std_logic;
        rst   : in  std_logic;
        x_in  : in  std_logic;  -- x_in = '0' -> to next state
        y_out : out std_logic_vector(1 downto 0));
end counter;

architecture Behavioral of counter is

type state is (s_00, s_01, s_10, s_11);
signal state_fsm : state;

signal y : std_logic_vector (1 downto 0);

begin

p_fsm : process(clk, rst, x_in) is
variable cnt : integer;
begin
   if rst = '1' then
      state_fsm <= s_00;
      y <= "00";
      cnt := 0;
   elsif rising_edge(clk) then
      case state_fsm is
      when s_00 =>
        y <= "00";
        if cnt < WTIME then
            cnt := cnt+1;
            state_fsm <= state_fsm;
        else
            cnt := 0;
            if x_in = '0' then
                state_fsm <= s_01;
            else
                state_fsm <= s_11;
            end if;
        end if;
      
      when s_01 =>
        y <= "01";
        if cnt < WTIME then
            cnt := cnt+1;
            state_fsm <= state_fsm;
        else
            cnt := 0;
            if x_in = '0' then
                state_fsm <= s_10;
            else
                state_fsm <= s_00;
            end if;
        end if;
        
      when s_10 =>
        y <= "10";
        if cnt < WTIME then
            cnt := cnt+1;
            state_fsm <= state_fsm;
        else
            cnt := 0;
            if x_in = '0' then
                state_fsm <= s_11;
            else
                state_fsm <= s_01;
            end if;
        end if;
         
      when s_11 =>
        y <= "11";
        if cnt < WTIME then
            cnt := cnt+1;
            state_fsm <= state_fsm;
        else
            cnt := 0;
            if x_in = '0' then
                state_fsm <= s_00;
            else
                state_fsm <= s_10;
            end if;
        end if;
        
      when others =>
         state_fsm <= s_00;   
      end case;
   end if;   
end process;

y_out <= y;

end Behavioral;