targets -set -nocase -filter {name =~ "*A53*__cpu__" && jtag_cable_name =~ "Xilinx HW-Z1-ZCU104 FT4232H*"}
con -addr 0x00000000
