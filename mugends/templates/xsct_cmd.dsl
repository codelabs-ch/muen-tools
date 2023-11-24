# xsct command stream with CMD:REGEX:JTAG_CABLE:FILE
FPGA:PL*:Xilinx HW-Z1-ZCU104 FT4232H*:default_system.bit
HDF:APU*:Xilinx HW-Z1-ZCU104 FT4232H*:default_system.hdf:list {0x80000000 0xbfffffff} {0x400000000 0x5ffffffff} {0x1000000000 0x7fffffffff}
PMU:MicroBlaze PMU*:Xilinx HW-Z1-ZCU104 FT4232H*:debug_pmufw.elf
FSBL:*A53*0:Xilinx HW-Z1-ZCU104 FT4232H*:debug_fsbl.elf
__entries__