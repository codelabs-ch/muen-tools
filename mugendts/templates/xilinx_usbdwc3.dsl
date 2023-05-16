__usb_bus_alias__ __usb_bus_name__@__usb_bus_base__ {
    compatible = "snps,dwc3";
    reg = <0x__usb_base_high__ 0x__usb_base_low__ 0x__usb_size_high__ 0x__usb_size_low__>;
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI 0x__usb_irq_endpoint0__ IRQ_TYPE_LEVEL_HIGH>,
        <GIC_SPI 0x__usb_irq_otg__ IRQ_TYPE_LEVEL_HIGH>,
        <GIC_SPI 0x__usb_irq_wakeup__ IRQ_TYPE_LEVEL_HIGH>;
    clocks = <0x5>, <0x5>, <0x5>, <0x5>, <0x5>;
    clock-names = "ref", "bus_early", "suspend", "clk_xin", "clk_ahb";
    dr_mode = "host";
    status = "okay";
};
