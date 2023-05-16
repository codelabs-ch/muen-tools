__cchannel_bus_alias__ __cchannel_bus_name__@__cchannel_bus_base__ {
    compatible = "muen,communication-channel";
    reg = <0x__cchannel_base_high__ 0x__cchannel_base_low__ 0x__cchannel_size_high__ 0x__cchannel_size_low__>;
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI __cchannel_irq_irq__ IRQ_TYPE_LEVEL_HIGH>;
    type = <__cchannel_type__>;
    status = "okay";
};
