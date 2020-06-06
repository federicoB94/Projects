// Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2019.1 (lin64) Build 2552052 Fri May 24 14:47:09 MDT 2019
// Date        : Wed Sep 25 10:59:48 2019
// Host        : agostini-XPS-15 running 64-bit Ubuntu 18.04.3 LTS
// Command     : write_verilog -force -mode synth_stub
//               /mnt/DATA/SharedFolders/University/Magistrale/Anno1_Sem1/MAPD/FPGA/Exercise/09_IPBus/Lab9.srcs/sources_1/ip/vio_dpram/vio_dpram_stub.v
// Design      : vio_dpram
// Purpose     : Stub declaration of top-level module interface
// Device      : xc7a35tcsg324-1
// --------------------------------------------------------------------------------

// This empty module with port declaration file causes synthesis tools to infer a black box for IP.
// The synthesis directives are for Synopsys Synplify support to prevent IO buffer insertion.
// Please paste the declaration into a Verilog source file or add the file as an additional source.
(* X_CORE_INFO = "vio,Vivado 2019.1" *)
module vio_dpram(clk, probe_in0, probe_out0)
/* synthesis syn_black_box black_box_pad_pin="clk,probe_in0[31:0],probe_out0[9:0]" */;
  input clk;
  input [31:0]probe_in0;
  output [9:0]probe_out0;
endmodule
