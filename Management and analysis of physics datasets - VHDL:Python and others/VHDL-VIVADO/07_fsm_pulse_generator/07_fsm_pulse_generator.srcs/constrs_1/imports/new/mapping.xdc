## Clock signal
set_property -dict { PACKAGE_PIN E3    IOSTANDARD LVCMOS33 } [get_ports { clk }]; #IO_L12P_T1_MRCC_35 Sch=gclk[100]
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports { clk }];

## Buttons
#set_property -dict { PACKAGE_PIN D9    IOSTANDARD LVCMOS33 } [get_ports { rst }]; #IO_L6N_T0_VREF_16 Sch=btn[0]

## Switches
#set_property -dict { PACKAGE_PIN A8    IOSTANDARD LVCMOS33 } [get_ports { en_trig_in }]; #IO_L12N_T1_MRCC_16 Sch=sw[0]

## Loop back
set_property -dict { PACKAGE_PIN P18   IOSTANDARD LVCMOS33 } [get_ports { gen_out }]; #IO_L9N_T1_DQS_D13_14 Sch=ck_io[40]
set_property -dict { PACKAGE_PIN N17   IOSTANDARD LVCMOS33 } [get_ports { pulses_in }]; #IO_L9P_T1_DQS_14 Sch=ck_io[41]

## RGB LEDs
set_property -dict { PACKAGE_PIN E1    IOSTANDARD LVCMOS33 } [get_ports { done_out }]; #IO_L18N_T2_35 Sch=led0_b
