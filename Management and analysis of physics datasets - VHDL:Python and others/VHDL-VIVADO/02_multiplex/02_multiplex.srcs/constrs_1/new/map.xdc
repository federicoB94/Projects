# buttons
set_property -dict {PACKAGE_PIN D9  IOSTANDARD LVCMOS33} [get_ports { a_in }];
set_property -dict {PACKAGE_PIN C9  IOSTANDARD LVCMOS33} [get_ports { b_in }];
set_property -dict {PACKAGE_PIN B9  IOSTANDARD LVCMOS33} [get_ports { c_in }];
set_property -dict {PACKAGE_PIN B8  IOSTANDARD LVCMOS33} [get_ports { d_in }];

# switches
set_property -dict {PACKAGE_PIN A8  IOSTANDARD LVCMOS33} [get_ports { sel_in[0] }];
set_property -dict {PACKAGE_PIN C11 IOSTANDARD LVCMOS33} [get_ports { sel_in[1] }];

# RGB LEDs
set_property -dict {PACKAGE_PIN E1  IOSTANDARD LVCMOS33} [get_ports { y_out }]; 