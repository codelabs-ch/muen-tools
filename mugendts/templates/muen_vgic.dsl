interrupt-controller@__vgic_bus_base__ {
    compatible = "muensk,irq-v0";
    #interrupt-cells = <0x3>;
    interrupt-controller;
    __vgic_registers__
    interrupts = <GIC_PPI 0x9 (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_HIGH)>;
    linux,phandle = <0x7>;
    phandle = <0x7>;
};
