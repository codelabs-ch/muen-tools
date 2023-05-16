
		__nic_bus_alias__ __nic_bus_name__@__nic_bus_base__ {
			compatible = "cdns,zynqmp-gem", "cdns,gem";
			reg = <0x__nic_base_high__ 0x__nic_base_low__ 0x__nic_size_high__ 0x__nic_size_low__>;
			interrupt-parent = <0x7>;
			interrupts = <GIC_SPI 0x__nic_irq_controller__ IRQ_TYPE_LEVEL_HIGH>,
				<GIC_SPI 0x__nic_irq_wakeup__ IRQ_TYPE_LEVEL_HIGH>;
			clocks = <0x4 0x4 0x4 0x4 0x4>;
			clock-names = "pclk", "hclk", "tx_clk", "rx_clk", "tsu_clk";
			status = "okay";
			
			phy-handle = <0xf>;
			phy-mode = "rgmii-id";
			xlnx,ptp-enet-clock = <0x0>;
			local-mac-address = [00 0a 35 00 22 01];

			#address-cells = <0x1>;
			#size-cells = <0x0>;
			
			phy@c {
				reg = <0xc>;
				ti,rx-internal-delay = <0x8>;
				ti,tx-internal-delay = <0xa>;
				ti,fifo-depth = <0x1>;
				ti,dp83867-rxctrl-strap-quirk;
				phandle = <0xf>;
			};
		};

