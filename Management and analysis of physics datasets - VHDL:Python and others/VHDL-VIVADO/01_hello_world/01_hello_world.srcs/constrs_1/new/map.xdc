set_property -dict { PACKAGE_PIN E1 IOSTANDARD LVCMOS33 } [get_ports { led_out[0] }];
set_property -dict { PACKAGE_PIN J4 IOSTANDARD LVCMOS33 } [get_ports { led_out[1] }];
set_property -dict { PACKAGE_PIN J3 IOSTANDARD LVCMOS33 } [get_ports { led_out[2] }];
set_property -dict { PACKAGE_PIN H5 IOSTANDARD LVCMOS33 } [get_ports { led_out[3] }];

set_property -dict { PACKAGE_PIN D9 IOSTANDARD LVCMOS33 } [get_ports { btn_in[0]  }];
set_property -dict { PACKAGE_PIN C9 IOSTANDARD LVCMOS33 } [get_ports { btn_in[1]  }];
set_property -dict { PACKAGE_PIN B9 IOSTANDARD LVCMOS33 } [get_ports { btn_in[2]  }];
set_property -dict { PACKAGE_PIN A8 IOSTANDARD LVCMOS33 } [get_ports { btn_in[3]  }];