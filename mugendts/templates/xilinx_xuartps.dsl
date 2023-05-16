__uart_bus_alias__ __uart_bus_name__@__uart_bus_base__ {
    compatible = "cdns,uart-r1p12", "xlnx,xuartps";
    reg = <0x__uart_base_high__ 0x__uart_base_low__ 0x__uart_size_high__ 0x__uart_size_low__>;
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI 0x__uart_irq_irq__ IRQ_TYPE_LEVEL_HIGH>;
    clocks = <0x3 0x3>;
    clock-names = "uart_clk", "pclk";
    status = "okay";
};
