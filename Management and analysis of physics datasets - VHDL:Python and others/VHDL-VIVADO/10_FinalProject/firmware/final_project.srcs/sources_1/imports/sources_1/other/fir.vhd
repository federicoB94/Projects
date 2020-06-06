library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity fir is
  generic(N      : integer := 32;
          N_coef : integer := 13;
          N_tap  : integer := 5;
          N_SAMPLE : integer := 1024 );
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic;
     -- all the other in/out ports
     en_in     : in std_logic;                      -- starting signal
     fir_in    : in std_logic_vector(N-1 downto 0);  -- fir input
     fir_out   : out std_logic_vector(N-1 downto 0); -- fir output
     we_out    : out std_logic;                     -- writing enable
     addr_out0 : out std_logic_vector(9 downto 0);  -- reading address (square register)
     addr_out1 : out std_logic_vector(9 downto 0);  -- writing address (fir register)
     sel_mux_out : out std_logic                    -- select mux
     );
end fir;

architecture Behavioral of fir is

component ffd is
  generic (N : integer := 32);
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic;
     d_in  : in  std_logic_vector(N-1 downto 0);
     q_out : out std_logic_vector(N-1 downto 0));
end component;

signal C0, C1, C2, C3, C4 : signed(N_coef-1 downto 0);
signal m0, m1, m2, m3, m4 : signed((N+N_coef)-1 downto 0)  := (others => '0');
signal o0, o1, o2, o3, o4 : std_logic_vector(N-1 downto 0) := (others => '0'); -- flip flop outputs
signal s0,s1,s2,s3,s4     : signed((N+N_coef)-1 downto 0)  := (others => '0');
signal x_sum              : std_logic_vector(N-1 downto 0) := (others => '0');

signal en_p, selector, we_s : std_logic;

type state is (s_idle, s_read, s_filter, s_write);
signal state_fsm : state;

constant Q : integer := 11; -- shift size

begin

C0 <= to_signed(1934*(2**Q)/10000, N_coef); -- 0.193353
C1 <= to_signed(2033*(2**Q)/10000, N_coef); -- 0.203303
C2 <= to_signed(2067*(2**Q)/10000, N_coef); -- 0.206686
C3 <= to_signed(2033*(2**Q)/10000, N_coef); -- 0.203303
C4 <= to_signed(1934*(2**Q)/10000, N_coef); -- 0.193353

f_01 : ffd port map (clk => clk, rst => rst, d_in => o0 , q_out => o1);
f_12 : ffd port map (clk => clk, rst => rst, d_in => o1 , q_out => o2);
f_23 : ffd port map (clk => clk, rst => rst, d_in => o2 , q_out => o3);
f_34 : ffd port map (clk => clk, rst => rst, d_in => o3 , q_out => o4);
f_end : ffd port map (clk => clk, rst => rst, d_in => x_sum, q_out => fir_out);


fir_proc : process(clk, rst, en_in)
variable cnt : integer := 0;

begin
      
if rst = '1' then
    state_fsm <= s_idle;
    x_sum <= (others => '0');
    selector <= '0';
    cnt := 0;
    
  elsif rising_edge(clk) then
     en_p <= en_in;
     
     case state_fsm is
     
     when s_idle =>
       if en_in = '1' and en_p = '0' then
         state_fsm <= s_read;
         selector <= '1';    -- take 'ownership' of square register
         we_s <= '0';        -- don't allow writing: still reading
       else
         state_fsm <= s_idle;
         selector <= '0';
       end if;
       cnt := 0;
     
     when s_read =>
       if cnt < N_sample then
         addr_out0 <= std_logic_vector(to_unsigned(cnt, addr_out0'length));
         we_s <= '0';
         selector <= '1'; --we say the mux to look at our address
         state_fsm <= s_filter;
       else
         state_fsm <= s_idle; --finished
       end if;
       
     when s_filter =>
     
       o0 <= fir_in;
       
       m0 <= signed(o0) * C0;
       m1 <= signed(o1) * C1;
       m2 <= signed(o2) * C2;
       m3 <= signed(o3) * C3;
       m4 <= signed(o4) * C4;
       
       s0 <= m0;
       s1 <= s0 + m1;
       s2 <= s1 + m2;
       s3 <= s2 + m3;
       s4 <= s3 + m4;
       
       x_sum <= std_logic_vector(resize(SHIFT_RIGHT(s4, Q), N));
       
       state_fsm <= s_write;
         
     when s_write =>
       we_s <= '1';
       addr_out1 <= std_logic_vector(to_unsigned(cnt,addr_out1'length));
       cnt := cnt + 1;
       state_fsm <= s_read;
        
     end case;

end if;

sel_mux_out <= selector;
we_out <= we_s;

end process;


end Behavioral;
