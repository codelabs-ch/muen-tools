__usb_ctlr_alias____usb_ctlr_name__@__usb_ctlr_base__ {
    #address-cells = <0x2>;
    #size-cells = <0x2>;
    compatible = "xlnx,zynqmp-dwc3";
    __usb_ctlr_registers__
    status = "okay";
    ranges;

    __usb_iface_alias____usb_iface_name__@__usb_iface_base__ {
        compatible = "snps,dwc3";
        __usb_iface_registers__
        interrupt-parent = <0x7>;
        interrupts = <GIC_SPI 0x__usb_irq_endpoint_0__ IRQ_TYPE_LEVEL_HIGH>,
            <GIC_SPI 0x__usb_irq_otg__ IRQ_TYPE_LEVEL_HIGH>,
            <GIC_SPI 0x__usb_irq_wakeup__ IRQ_TYPE_LEVEL_HIGH>;
        clocks = <&clk250>, <&clk250>;
        clock-names = "ref", "bus_early";
        snps,quirk-frame-length-adjustment = <0x20>;
        snps,resume-hs-terminations;
        snps,usb2-lpm-disable;
        snps,dis_u3_susphy_quirk;
        snps,dis_u2_susphy_quirk;
        maximum-speed = "super-speed";
        dr_mode = "host";
        status = "okay";
    };
};
