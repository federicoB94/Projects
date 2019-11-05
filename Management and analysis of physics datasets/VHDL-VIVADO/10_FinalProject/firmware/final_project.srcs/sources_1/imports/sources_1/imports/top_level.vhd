library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library unisim;
use unisim.vcomponents.all;

use work.ipbus.all;

entity top_level is
  generic( 
    g_NSLV            : positive := 11
    );
  port(
    clk_base_xc7a_i : in std_logic;

    xc7a_rst_i  : in std_logic;         -- hw reset input
    xc7a_led0_o : out std_logic;        -- heart beat - system clock

    -- IPBUS
    mii_tx_en_o  : out std_logic;
    mii_tx_clk_i : in  std_logic;
    mii_txd_o    : out std_logic_vector(3 downto 0);
    mii_rx_clk_i : in  std_logic;
    mii_rx_dv_i  : in  std_logic;
    mii_rxd_i    : in  std_logic_vector(3 downto 0);
    clk_phy_o    : out std_logic;
    rst_n_phy_o  : out std_logic;
    
    -- FLASH Signals
    flash_mosi_o      : out std_logic;
    flash_miso_i      : in  std_logic;
    flash_clk_o       : out std_logic;
    flash_cs_o        : out std_logic;
    flash_w_o         : out std_logic -- write protect. To set low.    
     
    );
end top_level;

architecture rtl of top_level is


-- clocks related signals
  signal s_sysclk          : std_logic;
  signal s_sysclk_x4       : std_logic;
  signal s_sysclk_x2       : std_logic;
  signal s_locked_tx       : std_logic;
  signal s_clk_200         : std_logic;

  signal reset_i : std_logic;


-- heart beat
  signal u_led_count  : unsigned(24 downto 0);
  signal s_led_count  : std_logic_vector(24 downto 0);

----IPBUS signals
  signal ipb_clk, locked, rst_125, rstn_125, rst_ipb, onehz : std_logic;
  signal mac_tx_data, mac_rx_data : std_logic_vector(7 downto 0);
  signal mac_tx_valid, mac_tx_last, mac_tx_error,
    mac_tx_ready,mac_rx_valid, mac_rx_last, mac_rx_error : std_logic;
  signal ipb_master_out                                  : ipb_wbus;
  signal ipb_master_in                                   : ipb_rbus;
  signal sys_rst : std_logic;
  signal s_ip_addr                                       : std_logic_vector(31 downto 0);
  signal s_mac_addr                                      : std_logic_vector(47 downto 0);
  
  signal ipbw : ipb_wbus_array(2 downto 0);
  signal ipbr : ipb_rbus_array(2 downto 0);
  
  -- FLASH NOR signals 
  signal flash_rxd: std_logic_vector(7 downto 0);
  signal flash_txd: std_logic_vector(8*4  -1 downto 0);
	
  signal flash_mosi_s : std_logic;
  signal flash_miso_s : std_logic;
  signal flash_clk_s : std_logic;
  signal flash_cs_s : std_logic;
  signal flash_w_s : std_logic;
	 
  signal s_size : std_logic_vector(15 downto 0);
 	 
  signal rxd_s : std_logic_vector(7 downto 0);
  signal we_s  : std_logic;	
  signal addr_s : std_logic_vector(9 downto 0);
	
  signal we_sq, we_fir, fir_on : std_logic;
  signal addr, addr_sq, addr_fir_0,  addr_fir_1: std_logic_vector(9 downto 0);
  signal y_sq, q_sq, y_fir : std_logic_vector(31 downto 0);
  
  signal mux_sel_s : std_logic;
  
  --signal we_sq_p, we_fir_p : std_logic;
  
  --type state is (s_idle, s_wave, s_fir);
  --signal state_fsm : state;
  
  -- debug
  COMPONENT vio_0
  PORT (
    clk : IN STD_LOGIC;
    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out2 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END COMPONENT;

  -- debug signals
  signal start_fir, start_sq, vio_rst : std_logic;
  
  
 
---------------------- COMPONENTS ------------------------------------------------
component ipbus_dpram
        generic(
            ADDR_WIDTH: natural
        );
        port(
            clk: in std_logic;
            rst: in std_logic;
            ipb_in: in ipb_wbus;
            ipb_out: out ipb_rbus;
            rclk: in std_logic;
            we: in std_logic := '0';
            d: in std_logic_vector(31 downto 0) := (others => '0');
            q: out std_logic_vector(31 downto 0);
            addr: in std_logic_vector(9 downto 0)
        );	
end component;

component spi_master_flash
   generic (
      WTIME    : integer   := 10000;
	  TXBITS   : integer   := 8;
      RXBITS   : integer   := 8;
	  N_BYTES  : integer   := 1		
      );
   port ( 
      clock    : in  std_logic;
      reset    : in  std_logic;
      txd      : in  std_logic_vector(TXBITS-1   downto 0);
      word     : out std_logic_vector(N_BYTES*RXBITS-1 downto 0);
      start    : in  std_logic;
      ready    : out std_logic;
      miso     : in  std_logic;
      mosi     : out std_logic;
      sclk     : out std_logic;
      cs       : out std_logic;
      wr_pr_o  : out std_logic;
      we_out   : out std_logic
      );
end component; 

component square_wave is
  generic( PERIOD      : integer := 20; -- in number of clock cycles
           DUTY_CYC    : integer := 30; -- in number of clock cycles
           SAMPLE_N    : integer := 1024
  );
  Port (clk        : in  std_logic;  
        rst        : in  std_logic; 
        en_gen_in  : in  std_logic;                       -- start signal
        we_out     : out std_logic;                       -- write enable for the dpram
        address_out: out std_logic_vector(9 downto 0);    -- address for the dpram 
        y_out      : out std_logic_vector(31 downto 0));  -- data to write in the dpram
end component;
    
component fir is
  generic(N      : integer := 32;
          N_coef : integer := 13;
          N_tap  : integer := 5;
          N_SAMPLE : integer := 1024 );
  Port (
     clk   : in  std_logic;
     rst   : in  std_logic;
     -- all the other in/out ports
     en_in     : in std_logic;
     fir_in    : in std_logic_vector(N-1 downto 0);
     fir_out   : out std_logic_vector(N-1 downto 0);
     we_out    : out std_logic;
     addr_out0 : out std_logic_vector(9 downto 0);
     addr_out1 : out std_logic_vector(9 downto 0);
     sel_mux_out : out std_logic
     );
end component;    

component mux21 is
    generic (
    N_BITS : integer := 10
    );
    Port ( a_in : in std_logic_vector(N_BITS-1 downto 0);
           b_in : in std_logic_vector(N_BITS-1 downto 0);
           sel_in : in std_logic;
           y_out : out std_logic_vector(N_BITS-1 downto 0));
end component;

begin
-------------------------------------IP, MAC and GCU ID---------------------------------------
  s_ip_addr  <= X"0A0A0A64";
  s_mac_addr <= X"020ddba11599";
  
  reset_i <= rst_ipb;
  rst_n_phy_o <= not rst_ipb;

------------------------------------------------------------------------------------
-------------------------------PLL and system clock---------------------------------
  Inst_system_clocks : entity work.system_clocks
    port map(
      sysclk_p    => clk_base_xc7a_i,
      clko_ipb    => ipb_clk,           -- 31.25 MHz
      sysclk_o    => s_sysclk,          -- 62.5 MHz
      sysclk_x2_o => s_sysclk_x2,       -- 125 MHz
      sysclk_x4_o => s_sysclk_x4,       -- 250 MHz
      phy_clk_o   => clk_phy_o,         -- 10 MHz
      clk_200_o   => s_clk_200,
      locked      => s_locked_tx,
      nuke        => sys_rst,
      rsto_125    => rst_125,
      rsto_ipb    => rst_ipb,
      onehz       => onehz
      );

     rstn_125 <= not rst_125;
------------------------------------------------------------------------------------
----------------------------------------IPBUS---------------------------------------

  eth_mac_block_1 : entity work.tri_mode_eth_mac_v5_5_fifo_block
    port map (
      gtx_clk                 => s_sysclk_x2,  -- 125 MHz
      glbl_rstn               => rstn_125,
      rx_axi_rstn             => rstn_125,
      tx_axi_rstn             => rstn_125,
      rx_mac_aclk             => open,
      rx_reset                => open,
      rx_statistics_vector    => open,
      rx_statistics_valid     => open,
      rx_fifo_clock           => s_sysclk_x2,  -- 125 MHz
      rx_fifo_resetn          => rstn_125,
      rx_axis_fifo_tdata      => mac_rx_data,
      rx_axis_fifo_tvalid     => mac_rx_valid,
      rx_axis_fifo_tready     => '1',
      rx_axis_fifo_tlast      => mac_rx_last,
      tx_mac_aclk             => open,
      tx_reset                => open,
      tx_ifg_delay            => X"00",
      tx_statistics_vector    => open,
      tx_statistics_valid     => open,
      tx_fifo_clock           => s_sysclk_x2,  -- 125 MHz
      tx_fifo_resetn          => rstn_125,
      tx_axis_fifo_tdata      => mac_tx_data,
      tx_axis_fifo_tvalid     => mac_tx_valid,
      tx_axis_fifo_tready     => mac_tx_ready,
      tx_axis_fifo_tlast      => mac_tx_last,
      pause_req               => '0',
      pause_val               => X"0000",
      mii_txd                 => mii_txd_o,
      mii_tx_en               => mii_tx_en_o,
      mii_tx_er               => open,
      mii_rxd                 => mii_rxd_i,
      mii_rx_dv               => mii_rx_dv_i,
      mii_rx_er               => '0',
      mii_rx_clk              => mii_rx_clk_i,
      mii_tx_clk              => mii_tx_clk_i,
      rx_configuration_vector => X"0000_0000_0000_0000_1012",
      tx_configuration_vector => X"0000_0000_0000_0000_1012");

-- ipbus control logic
  ipbus : entity work.ipbus_ctrl
    port map(
      mac_clk      => s_sysclk_x2,      -- 125 MHz
      rst_macclk   => rst_125,
      ipb_clk      => ipb_clk,          -- 31.25 MHz
      rst_ipb      => rst_ipb,
      mac_rx_data  => mac_rx_data,
      mac_rx_valid => mac_rx_valid,
      mac_rx_last  => mac_rx_last,
      mac_rx_error => mac_rx_error,
      mac_tx_data  => mac_tx_data,
      mac_tx_valid => mac_tx_valid,
      mac_tx_last  => mac_tx_last,
      mac_tx_error => mac_tx_error,
      mac_tx_ready => mac_tx_ready,
      ipb_out      => ipb_master_out,
      ipb_in       => ipb_master_in,
      mac_addr     => s_mac_addr,
      ip_addr      => s_ip_addr
      );

  fabric: entity work.ipbus_fabric
    generic map(NSLV => 3)
    port map(
      ipb_in => ipb_master_out,
      ipb_out => ipb_master_in,
      ipb_to_slaves => ipbw,
      ipb_from_slaves => ipbr
    );
    
----------------------------------------------------------------------------
--------------------------  END IPBUS CORE ---------------------------------
----------------------------------------------------------------------------    



----------------------------------------------------------------------------
--------------------------      3 DPRAM    ---------------------------------
---------------------------------------------------------------------------- 
-- Size data register
   size_reg: ipbus_dpram 
      generic map( ADDR_WIDTH => 1)
      port map(
           clk        => ipb_clk,
           rst        => rst_ipb,
           ipb_in     => ipbw(2),
           ipb_out    => ipbr(2),
           rclk       => clk_base_xc7a_i,
           we         => we_s,
           d          => x"0000" & s_size,
           q          => open,
           addr       => (others => '0')
      );   

-- Square wave registers
  sq_regs: ipbus_dpram 
	generic map( ADDR_WIDTH => 10)
	port map(
            clk        => ipb_clk,
            rst        => rst_ipb,
            ipb_in     => ipbw(0),
            ipb_out    => ipbr(0),
		    rclk       => clk_base_xc7a_i,
		    we         => we_sq,
		    d          => y_sq,
		    q          => q_sq,
		    addr       => addr
	);
	
-- FIR registers
   fir_regs: ipbus_dpram 
      generic map( ADDR_WIDTH => 10)
      port map(
           clk        => ipb_clk,
           rst        => rst_ipb,
           ipb_in     => ipbw(1),
           ipb_out    => ipbr(1),
           rclk       => clk_base_xc7a_i,
           we         => we_fir,
           d          => y_fir,
           q          => open,
           addr       => addr_fir_1
        );	
        
----------------------------------------------------------------------------
--------------------------    END DPRAMs   ---------------------------------
---------------------------------------------------------------------------- 


-----------------------------------------------------------------
------------   DESIGN LOGIC -------------------------------------  
-----------------------------------------------------------------      
              
-----------------------------------------------------------------
--- Flash registers reading  ------------------------------------
-----------------------------------------------------------------
   flash_master: spi_master_flash
   generic map (
      WTIME    => 32,
	  TXBITS   => 8*4,
	  RXBITS   => 8,
      N_BYTES  => 2
      )
   port map ( 
      clock    => clk_base_xc7a_i,
      reset    => vio_rst or rst_ipb,
      txd      => x"03" & x"000000", -- Command & address
      word     => s_size,
      start    => '1',
      ready    => open,
      miso     => flash_miso_s,
      mosi     => flash_mosi_s,
      sclk     => flash_clk_s,
      cs       => flash_cs_s,
	  wr_pr_o  => flash_w_s,
	  we_out   => we_s  
      );

	flash_mosi_o <= flash_mosi_s ;
	flash_miso_s <= flash_miso_i;
	flash_clk_o <= flash_clk_s ;
	flash_cs_o <= flash_cs_s;
	flash_w_o <= flash_w_s ;

-----------------------------------------------------------------
--- Square wave generator    ------------------------------------
-----------------------------------------------------------------
 square_w : square_wave 
  port map (clk  => clk_base_xc7a_i,  
            rst  => vio_rst,
            en_gen_in  => start_sq,
            we_out     => we_sq,
            address_out => addr_sq,
            y_out  => y_sq);

-----------------------------------------------------------------
--- FIR processing  ---------------------------------------------
-----------------------------------------------------------------            
  fir_dsp : fir 
              port map (
                 clk   => clk_base_xc7a_i,
                 rst   => vio_rst,
                 en_in => start_fir,
                 fir_in  => q_sq,
                 fir_out => y_fir,
                 we_out  => we_fir,
                 addr_out0 => addr_fir_0,
                 addr_out1 => addr_fir_1,
                 sel_mux_out => mux_sel_s
             );
             
  mux : mux21
              port map (
                 a_in => addr_sq,
                 b_in => addr_fir_0,
                 sel_in => mux_sel_s,
                 y_out => addr
             );     
            
                 
 -----------------------------------------------------------------
 ------------    DESIGN LOGIC END     ----------------------------
 -----------------------------------------------------------------
 
--p_top : process(clk_base_xc7a_i,vio_rst,we_sq, we_fir)
--begin
--   if rising_edge(clk_base_xc7a_i) then
--       we_sq_p <= we_sq;
--       we_fir_p <= we_fir;
--       if vio_rst = '1' then
--           state_fsm <= s_idle;
--           fir_on <= '0';
--           start_sq <= '0';
--           we_sq_p <= '0';
--           we_fir_p <= '0';
--       else
--           case state_fsm is
--           
--           when s_idle =>
--               if start_sq = '1' then
--                   state_fsm <= s_wave;
--               end if;
--           when s_wave =>  
--               if we_sq= '0' and we_sq_p = '1' then
--                   --finished to write to sq_reg
--                   state_fsm <= s_fir;
--                   start_sq <= '0';
--                   fir_on <= '1';
--               end if; 
--           when s_fir =>
--               if we_fir= '0' and we_fir_p = '1' then
--                   --finished to write to fir_reg
--                   state_fsm <= s_idle;
--                   start_sq <= '0';
--                   fir_on <= '0';
--                   we_sq_p <= '0';
--                   we_fir_p <= '0';
--               end if; 
--           end case;
--       end if;
--   end if;
--end process p_top;
 
                 
------------------------------------------------------------------------------------
---------------------------------------heart beat-----------------------------------
  p_heart_beat_counter : process(s_sysclk)
  begin
    if (rising_edge(s_sysclk)) then
        u_led_count <= u_led_count + 1;
    end if;
  end process p_heart_beat_counter;
  s_led_count <= std_logic_vector(u_led_count);

-----------------------LEDs output ---------------------
  xc7a_led0_o <= s_led_count(24);       -- heart beat  
  
  
  ---------------------VIO port map---------------------
  vio0 : vio_0
  PORT MAP (
    clk => clk_base_xc7a_i,
    probe_out0(0) => vio_rst,
    probe_out1(0) => start_sq,
    probe_out2(0) => start_fir
  );
  
end rtl;
