/*************************************************************************
*                                                                        *
*  Copyright (C) codelabs gmbh, Switzerland - all rights reserved        *
*                <https://www.codelabs.ch/>, <contact@codelabs.ch>       *
*                                                                        *
*  This program is free software: you can redistribute it and/or modify  *
*  it under the terms of the GNU General Public License as published by  *
*  the Free Software Foundation, either version 3 of the License, or     *
*  (at your option) any later version.                                   *
*                                                                        *
*  This program is distributed in the hope that it will be useful,       *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*  GNU General Public License for more details.                          *
*                                                                        *
*  You should have received a copy of the GNU General Public License     *
*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *
*                                                                        *
*                                                                        *
*    @contributors                                                       *
*        2023, 2023  David Loosli <david@codelabs.ch>                    *
*                                                                        *
*                                                                        *
*    @description                                                        *
*        Linux subject device tree source generated according to a       *
*        policy in format b.                                             *
*    @project                                                            *
*        MuenOnARM                                                       *
*    @interface                                                          *
*        Tool Mugendts                                                   *
*    @target                                                             *
*        Xilinx UltraScale+ ZCU104 evaluation board                      *
*    @reference                                                          *
*        ARM Architecture Reference Manual ARMv8 for ARMv8-A             *
*        architecture profile, Zynq UltraScale+ Device TRM UG1085,       *
*        Linux and U-Boot documentation                                  *
*                                                                        *
*************************************************************************/

/dts-v1/;

#include <dt-bindings/interrupt-controller/arm-gic.h>
#include <dt-bindings/interrupt-controller/irq.h>
#include <dt-bindings/muen/channel-muensk.h>

/ {
	model = "Xilinx UltraScale+ ZCU104 - On MuenSK";
	compatible = "xlnx,zynqmp-zcu104-revA", "xlnx,zynqmp-zcu104", "xlnx,zynqmp";
	interrupt-parent = <0x1>;
	
	#address-cells = <0x2>;
	#size-cells = <0x2>;

	aliases {
		serial = &serial0;
	};

	chosen {
		bootargs = "hostname=lnx1 console=ttyPS0,115200 root=/dev/ram0 initrd=0x2b000000,60M earlycon";
		stdout-path = "serial:115200n8";
	};

	memory@0 {
		device_type = "memory";
		reg = <0x0 0x00000000 0x0 0x30000000>;
	};

	cpus {
		#address-cells = <0x1>;
		#size-cells = <0x0>;

		cpu@0 {
			device_type = "cpu";
			compatible = "arm,cortex-a53";
			reg = <0x0>;
			operating-points-v2 = <0x2>;
		};
	};

	cpu-opp-table {
		compatible = "operating-points-v2";
		opp-shared;
		linux,phandle = <0x2>;
		phandle = <0x2>;

		opp00 {
			opp-hz = /bits/ 64 <1199999988>;
			opp-microvolt = <1000000>;
			clock-latency-ns = <500000>;
		};

		opp01 {
			opp-hz = /bits/ 64 <599999994>;
			opp-microvolt = <1000000>;
			clock-latency-ns = <500000>;
		};

		opp02 {
			opp-hz = /bits/ 64 <399999996>;
			opp-microvolt = <1000000>;
			clock-latency-ns = <500000>;
		};

		opp03 {
			opp-hz = /bits/ 64 <299999997>;
			opp-microvolt = <1000000>;
			clock-latency-ns = <500000>;
		};
	};

	clk100 {
		compatible = "fixed-clock";
		#clock-cells = <0x0>;
		clock-frequency = <99999999>;
		linux,phandle = <0x3>;
		phandle = <0x3>;
	};

	clk125 {
		compatible = "fixed-clock";
		#clock-cells = <0>;
		clock-frequency = <125000000>;
		linux,phandle = <0x4>;
		phandle = <0x4>;
	};

	clk250 {
		compatible = "fixed-clock";
		#clock-cells = <0>;
		clock-frequency = <250000000>;
		linux,phandle = <0x7>;
		phandle = <0x7>;
	};

	timer {
		compatible = "arm,armv8-timer";
		interrupts = <GIC_PPI 0xd (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
			<GIC_PPI 0xe (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
			<GIC_PPI 0xb (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
			<GIC_PPI 0xa (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>;
		clock-frequency = <100000000>;
	};

	amba-apu@30000000 {
		compatible = "simple-bus";
		#address-cells = <0x2>;
		#size-cells = <0x1>;
		ranges = <0x0 0x0 0x0 0x0 0xffffffff>;

		interrupt-controller@30000000 {
			compatible = "muensk,irq-v0";
			#interrupt-cells = <0x3>;
			interrupt-controller;
			reg = <0x0 0x30000000 0x2000>;
			interrupts = <GIC_PPI 0x9 (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_HIGH)>;
			linux,phandle = <0x1>;
			phandle = <0x1>;
		};
	};

	amba-soc@31000000 {
		compatible = "simple-bus";
		#address-cells = <0x2>;
		#size-cells = <0x2>;
		ranges;

		serial0: serial@31000000 {
			compatible = "cdns,uart-r1p12", "xlnx,xuartps";
			reg = <0x0 0x31000000 0x0 0x1000>;
			interrupts = <GIC_SPI 0x00 IRQ_TYPE_LEVEL_HIGH>;
			clocks = <0x3 0x3>;
			clock-names = "uart_clk", "pclk";
			status = "okay";
		};

		usb0@31010000 {
			compatible = "snps,dwc3";
			reg = <0x0 0x31010000 0x0 0x40000>;
			interrupt-parent = <0x1>;
			interrupts = <GIC_SPI 0x02 IRQ_TYPE_LEVEL_HIGH>,
				<GIC_SPI 0x03 IRQ_TYPE_LEVEL_HIGH>,
				<GIC_SPI 0x04 IRQ_TYPE_LEVEL_HIGH>;
			clocks = <0x7>, <0x7>, <0x7>, <0x7>, <0x7>;
			clock-names = "ref", "bus_early", "suspend", "clk_xin", "clk_ahb";
			dr_mode = "host";
			status = "okay";
		};

		ethernet@31010000 {
			compatible = "cdns,zynqmp-gem", "cdns,gem";
			reg = <0x0 0x31010000 0x0 0x1000>;
			interrupt-parent = <0x1>;
			interrupts = <GIC_SPI 0x02 IRQ_TYPE_LEVEL_HIGH>,
				<GIC_SPI 0x02 IRQ_TYPE_LEVEL_HIGH>;
			clocks = <0x4 0x4 0x4 0x4 0x4>;
			clock-names = "pclk", "hclk", "tx_clk", "rx_clk", "tsu_clk";
			phy-handle = <0xf>;
			phy-mode = "rgmii-id";
			xlnx,ptp-enet-clock = <0x0>;
			local-mac-address = [00 0a 35 00 22 01];
			status = "okay";

			#address-cells = <0x1>;
			#size-cells = <0x0>;
			#stream-id-cells = <0x1>;
			
			phy@c {
				reg = <0xc>;
				ti,rx-internal-delay = <0x8>;
				ti,tx-internal-delay = <0xa>;
				ti,fifo-depth = <0x1>;
				ti,dp83867-rxctrl-strap-quirk;
				phandle = <0xf>;
			};
		};

		cchannel@31050000 {
			compatible = "muen,communication-channel";
			reg = <0x0 0x31050000 0x0 0x1000>;
			interrupts = <GIC_SPI 0x8 IRQ_TYPE_LEVEL_HIGH>;
			type = <WRITEONLY_CHANNEL>;
			status = "okay";
		};

		cchannel@31051000 {
			compatible = "muen,communication-channel";
			reg = <0x0 0x31051000 0x0 0x1000>;
			interrupts = <GIC_SPI 0x9 IRQ_TYPE_LEVEL_HIGH>;
			type = <READONLY_CHANNEL>;
			status = "okay";
		};

		cchannel@31052000 {
			compatible = "muen,communication-channel";
			reg = <0x0 0x31052000 0x0 0x1000>;
			interrupts = <GIC_SPI 0xa IRQ_TYPE_LEVEL_HIGH>;
			type = <READONLY_CHANNEL>;
			status = "okay";
		};

		cchannel@31053000 {
			compatible = "muen,communication-channel";
			reg = <0x0 0x31053000 0x0 0x1000>;
			interrupts = <GIC_SPI 0xb IRQ_TYPE_LEVEL_HIGH>;
			type = <WRITEONLY_CHANNEL>;
			status = "okay";
		};

	};
};
