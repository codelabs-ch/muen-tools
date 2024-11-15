connect
jtag target 1
jtag frequency 30000000

### FPGA
targets -set -nocase -filter {name =~ "PL*" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
fpga -file "default_system.bit"
after 5000
###

### HDF
targets -set -nocase -filter {name =~ "APU*" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
loadhw -hw "default_system.hdf" -mem-ranges "{0x80000000 0xbfffffff} {0x400000000 0x5ffffffff} {0x1000000000 0x7fffffffff}"
configparams force-mem-access 1
after 5000
###

### PMU
targets -set -nocase -filter {name =~ "PSU*" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
mwr 0xffca0038 0x1ff
after 500
targets -set -nocase -filter {name =~ "MicroBlaze PMU*" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
dow debug_pmufw.elf
con
after 5000
###

### FSBL
targets -set -nocase -filter {name =~ "*A53*0" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
rst -processor
dow debug_fsbl.elf
con
after 10000
stop
###

### RST
__rst_entries__
###

### FILES
targets -set -nocase -filter {name =~ "*A53*0" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
__file_entries__
###

### Run
__run_entries__
###

disconnect
