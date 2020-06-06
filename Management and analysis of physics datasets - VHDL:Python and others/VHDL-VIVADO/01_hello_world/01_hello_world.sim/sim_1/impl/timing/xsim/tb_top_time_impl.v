// Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2019.1 (lin64) Build 2552052 Fri May 24 14:47:09 MDT 2019
// Date        : Wed Sep 18 11:42:54 2019
// Host        : agostini-XPS-15 running 64-bit Ubuntu 18.04.3 LTS
// Command     : write_verilog -mode timesim -nolib -sdf_anno true -force -file
//               /mnt/DATA/SharedFolders/University/Magistrale/Anno1_Sem1/MAPD/FPGA/Exercise/01_hello_world/01_hello_world.sim/sim_1/impl/timing/xsim/tb_top_time_impl.v
// Design      : top
// Purpose     : This verilog netlist is a timing simulation representation of the design and should not be modified or
//               synthesized. Please ensure that this netlist is used with the corresponding SDF file.
// Device      : xc7a35tcsg324-1
// --------------------------------------------------------------------------------
`timescale 1 ps / 1 ps
`define XIL_TIMING

(* ECO_CHECKSUM = "1a349c01" *) 
(* NotValidForBitStream *)
module top
   (btn_in,
    led_out);
  input [3:0]btn_in;
  output [3:0]led_out;

  wire [3:0]btn_in;
  wire [2:1]btn_in_IBUF;
  wire [3:0]led_out;
  wire [2:0]led_out_OBUF;

initial begin
 $sdf_annotate("tb_top_time_impl.sdf",,,,"tool_control");
end
  IBUF \btn_in_IBUF[0]_inst 
       (.I(btn_in[0]),
        .O(led_out_OBUF[0]));
  IBUF \btn_in_IBUF[1]_inst 
       (.I(btn_in[1]),
        .O(btn_in_IBUF[1]));
  IBUF \btn_in_IBUF[2]_inst 
       (.I(btn_in[2]),
        .O(btn_in_IBUF[2]));
  IBUF \btn_in_IBUF[3]_inst 
       (.I(btn_in[3]),
        .O(led_out_OBUF[2]));
  OBUF \led_out_OBUF[0]_inst 
       (.I(led_out_OBUF[0]),
        .O(led_out[0]));
  OBUF \led_out_OBUF[1]_inst 
       (.I(led_out_OBUF[1]),
        .O(led_out[1]));
  LUT2 #(
    .INIT(4'h8)) 
    \led_out_OBUF[1]_inst_i_1 
       (.I0(btn_in_IBUF[1]),
        .I1(btn_in_IBUF[2]),
        .O(led_out_OBUF[1]));
  OBUF \led_out_OBUF[2]_inst 
       (.I(led_out_OBUF[2]),
        .O(led_out[2]));
  OBUF \led_out_OBUF[3]_inst 
       (.I(led_out_OBUF[2]),
        .O(led_out[3]));
endmodule
`ifndef GLBL
`define GLBL
`timescale  1 ps / 1 ps

module glbl ();

    parameter ROC_WIDTH = 100000;
    parameter TOC_WIDTH = 0;

//--------   STARTUP Globals --------------
    wire GSR;
    wire GTS;
    wire GWE;
    wire PRLD;
    tri1 p_up_tmp;
    tri (weak1, strong0) PLL_LOCKG = p_up_tmp;

    wire PROGB_GLBL;
    wire CCLKO_GLBL;
    wire FCSBO_GLBL;
    wire [3:0] DO_GLBL;
    wire [3:0] DI_GLBL;
   
    reg GSR_int;
    reg GTS_int;
    reg PRLD_int;

//--------   JTAG Globals --------------
    wire JTAG_TDO_GLBL;
    wire JTAG_TCK_GLBL;
    wire JTAG_TDI_GLBL;
    wire JTAG_TMS_GLBL;
    wire JTAG_TRST_GLBL;

    reg JTAG_CAPTURE_GLBL;
    reg JTAG_RESET_GLBL;
    reg JTAG_SHIFT_GLBL;
    reg JTAG_UPDATE_GLBL;
    reg JTAG_RUNTEST_GLBL;

    reg JTAG_SEL1_GLBL = 0;
    reg JTAG_SEL2_GLBL = 0 ;
    reg JTAG_SEL3_GLBL = 0;
    reg JTAG_SEL4_GLBL = 0;

    reg JTAG_USER_TDO1_GLBL = 1'bz;
    reg JTAG_USER_TDO2_GLBL = 1'bz;
    reg JTAG_USER_TDO3_GLBL = 1'bz;
    reg JTAG_USER_TDO4_GLBL = 1'bz;

    assign (strong1, weak0) GSR = GSR_int;
    assign (strong1, weak0) GTS = GTS_int;
    assign (weak1, weak0) PRLD = PRLD_int;

    initial begin
	GSR_int = 1'b1;
	PRLD_int = 1'b1;
	#(ROC_WIDTH)
	GSR_int = 1'b0;
	PRLD_int = 1'b0;
    end

    initial begin
	GTS_int = 1'b1;
	#(TOC_WIDTH)
	GTS_int = 1'b0;
    end

endmodule
`endif
