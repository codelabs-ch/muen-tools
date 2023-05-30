/dts-v1/;

#include <dt-bindings/interrupt-controller/arm-gic.h>
#include <dt-bindings/interrupt-controller/irq.h>
#include <dt-bindings/muen/channel-muensk.h>

/ {
    model = "Xilinx UltraScale+ ZCU104 - Muen ARM64";
    compatible = "xlnx,zynqmp-zcu104-revA", "xlnx,zynqmp-zcu104", "xlnx,zynqmp";
    interrupt-parent = <0x7>;

    #address-cells = <0x2>;
    #size-cells = <0x2>;

    aliases {
        serial = &__serial_alias__;
    };

    chosen {
        bootargs = "__chosen_bootparams__";
        stdout-path = "serial:115200n8";
    };

    memory@__memory_base__ {
        device_type = "memory";
        __memory_registers__
    };

    cpus {
        #address-cells = <0x1>;
        #size-cells = <0x0>;

        cpu@0 {
            device_type = "cpu";
            compatible = "arm,cortex-a53";
            reg = <0x0>;
            operating-points-v2 = <0x1>;
        };
    };

    cpu-opp-table {
        compatible = "operating-points-v2";
        opp-shared;
        linux,phandle = <0x1>;
        phandle = <0x1>;

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
        linux,phandle = <0x5>;
        phandle = <0x5>;
    };

    timer {
        compatible = "arm,armv8-timer";
        interrupts = <GIC_PPI 0xd (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
            <GIC_PPI 0xe (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
            <GIC_PPI 0xb (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>,
            <GIC_PPI 0xa (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_LOW)>;
        clock-frequency = <100000000>;
    };

    amba-apu@__amba_apu_base__ {
        compatible = "simple-bus";
        #address-cells = <0x2>;
        #size-cells = <0x1>;
        ranges = <0x0 0x0 0x0 0x0 0xffffffff>;

        interrupt-controller@__vgic_bus_base__ {
            compatible = "muensk,irq-v0";
            #interrupt-cells = <0x3>;
            interrupt-controller;
            reg = <0x__vgic_base_high__ 0x__vgic_base_low__ 0x2000>;
            interrupts = <GIC_PPI 0x9 (GIC_CPU_MASK_SIMPLE(4) | IRQ_TYPE_LEVEL_HIGH)>;
            linux,phandle = <0x7>;
            phandle = <0x7>;
        };
    };

    amba-soc-soc@__amba_soc_base__ {
        compatible = "simple-bus";
        #address-cells = <0x2>;
        #size-cells = <0x2>;
        ranges;

__amba_soc_devices__

    };
};
