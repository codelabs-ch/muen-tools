cpu@__cpu_base__ {
    device_type = "cpu";
    compatible = "arm,cortex-a53";
    __cpu_registers__
    operating-points-v2 = <0x1>;
    enable-method = "spin-table";
    cpu-release-addr = <0x0 0x10000>;
    /*^ Just any address in ram, since we jumped straight to
     * secondary_holding_pen from heads.S header no actual release is
     * necessary */
};
