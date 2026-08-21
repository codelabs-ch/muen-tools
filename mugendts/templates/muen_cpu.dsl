cpu@1 {
    device_type = "cpu";
    compatible = "arm,cortex-a53";
    reg = <0x1>; /* Matched against MPIDR Aff0-3 (MPIDR_HWID_BITMASK) */
    operating-points-v2 = <0x1>;
    enable-method = "spin-table";
    cpu-release-addr = <0x0 0x20000000>;
    /*^ Just any address in ram, since we jumped straight to
     * secondary_holding_pen from heads.S header no actual release is
     * necessary */
};

cpu@2 {
    device_type = "cpu";
    compatible = "arm,cortex-a53";
    reg = <0x2>; /* Matched against MPIDR Aff0-3 (MPIDR_HWID_BITMASK) */
    operating-points-v2 = <0x1>;
    enable-method = "spin-table";
    cpu-release-addr = <0x0 0x20000000>;
    /*^ Just any address in ram, since we jumped straight to
     * secondary_holding_pen from heads.S header no actual release is
     * necessary */
};
