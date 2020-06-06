----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/18/2019 03:32:43 PM
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity counter is
  Port ( clk   : in  std_logic;
         rst   : in  std_logic;
         sw0   : in  std_logic;
         sw1   : in  std_logic;
         y_out : out std_logic_vector(3 downto 0) );
end counter;

architecture rtl of counter is

signal slow_clk, slow_clk_p : std_logic;
signal counter : unsigned (27 downto 0);
signal slow_counter : unsigned (3 downto 0);

-- debug component
COMPONENT vio_0
  PORT (
    clk : IN STD_LOGIC;
    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out2 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out3 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
END COMPONENT;

COMPONENT ila_0

PORT (
	clk : IN STD_LOGIC;

	probe0 : IN STD_LOGIC_VECTOR(0 DOWNTO 0); 
	probe1 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
	probe2 : IN STD_LOGIC_VECTOR(3 DOWNTO 0)
);
END COMPONENT  ;

-- debug signals
signal vio_rst, vio_sw0, vio_sw1 : std_logic;
signal vio_slow_counter : std_logic_vector(3 downto 0);

signal ila_sw0, ila_sw1 : std_logic;
signal ila_y : std_logic_vector(3 downto 0);


begin

p_cnt : process(clk, vio_rst) is
begin
    if vio_rst = '1' then
        counter <= (others => '0');
    elsif rising_edge(clk) then
        counter <= counter + 1; 
    end if;
end process;

slow_clk <= counter(3);

p_slow_cnt : process(clk, vio_rst, slow_clk) is
begin
    if vio_rst = '1' then
        slow_counter <= unsigned(vio_slow_counter);
    elsif rising_edge(clk) then
        slow_clk_p <= slow_clk;
        if slow_clk = '1' and slow_clk_p = '0' then -- Rising Edge
            if vio_sw1 = '1' then
                slow_counter <= slow_counter;
            else
                if vio_sw0 = '0' then
                    slow_counter <= slow_counter + 1;
                else
                    slow_counter <= slow_counter - 1;
                end if;
            end if;
        end if; 
    end if;
end process;

y_out <= std_logic_vector(slow_counter);

ila_sw0 <= vio_sw0;
ila_sw1 <= vio_sw1;
ila_y   <= std_logic_vector(slow_counter);

-- VIO debug
vio0 : vio_0
  PORT MAP (
    clk => clk,
    probe_out0(0) => vio_rst,
    probe_out1(0) => vio_sw0,
    probe_out2(0) => vio_sw1,
    probe_out3 => vio_slow_counter
  );

-- ILA debug
ila0 : ila_0
PORT MAP (
	clk => clk,

	probe0(0) => ila_sw0, 
	probe1(0) => ila_sw1,
	probe2 => ila_y
);

end rtl;
