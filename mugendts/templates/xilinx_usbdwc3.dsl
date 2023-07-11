__usb_bus_alias____usb_bus_name__@__usb_bus_base__ {
    compatible = "snps,dwc3";
    __usb_registers__
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI 0x__usb_irq_endpoint_0__ IRQ_TYPE_LEVEL_HIGH>,
        <GIC_SPI 0x__usb_irq_otg__ IRQ_TYPE_LEVEL_HIGH>,
        <GIC_SPI 0x__usb_irq_wakeup__ IRQ_TYPE_LEVEL_HIGH>;
    clocks = <&clk250>, <&clk250>, <&clk250>, <&clk250>, <&clk250>;
    clock-names = "ref", "bus_early", "suspend", "clk_xin", "clk_ahb";
    dr_mode = "host";
    status = "okay";
};
