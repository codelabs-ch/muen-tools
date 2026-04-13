__gpio_bus_alias____gpio_bus_name__@__gpio_bus_base__ {
    compatible = "xlnx,zynqmp-gpio-1.0";
    __gpio_registers__
    clocks = <&clk100>;
    gpio-controller;
    #gpio-cells = <0x2>;
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI 0x__gpio_irq_irq__ IRQ_TYPE_LEVEL_HIGH>;
    interrupt-controller;
    #interrupt-cells = <0x2>;
    emio-gpio-width = <0x20>;
    gpio-mask-high = <0x0>;
    gpio-mask-low = <0x5600>;
    status = "okay";
};
