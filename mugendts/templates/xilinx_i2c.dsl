__i2c_bus_alias____i2c_bus_name__@__i2c_bus_base__ {
    compatible = "cdns,i2c-r1p14";
    __i2c_registers__
    interrupt-parent = <0x7>;
    interrupts = <GIC_SPI 0x__i2c_irq_irq__ IRQ_TYPE_LEVEL_HIGH>;
    clocks = <&clk100>;
    clock-names = "pclk";
    clock-frequency = <400000>;__i2c_gpio_scl_sda__
    status = "okay";

    #address-cells = <1>;
    #size-cells = <0>;

    /*
     * TI TCA6416 is an I2C / SMBus expander that provides 16-bits of
     * general purpose parallel input/output (GPIO) expansion for the
     * two-line bidirectional I2C bus.
     */
    i2c-gpio@20 {
        compatible = "ti,tca6416";
        reg = <0x00000020>;
        gpio-controller;
        #gpio-cells = <2>;
    };

    /* The PCA9548A is an octal bidirectional translating multiplexer
       controlled via the I2C-bus. The SCL / SDA upstream pair fans out
       to eight downstream pairs.*/
    i2c-mux@74 {
        compatible = "nxp,pca9548";
        reg = <0x00000074>;

        #address-cells = <1>;
        #size-cells = <0>;

        i2c-ds@0 {
            reg = <0>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * IIC EEPROM 1kB memory which uses 256B blocks
             * where every block has different address.
             *        0 -  256B  address  0x54
             *     256B -  512B  address  0x55
             *     512B -  768B  address  0x56
             *     768B - 1024B  address  0x57
             */
            i2c-eeprom@54 {
                compatible = "atmel,24c08";
                reg = <0x00000054>;
            };
        };

        i2c-ds@1 {
            reg = <1>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * IDT FemtoClock PLL is an octal universal frequency translator.
             * NOTE that the driver is not supported upstream - the Xilinx
             * Linux fork has to be used, if required. In addition and in
             * contradiction to the Xilinx ZCU104 manual, the IC is detected
             * at address '6c' and not '7c'.
             * i2c-pll@6c {
             *     compatible = "idt,8t49n287";
             *     reg = <0x0000006c>;
             * };
             */
        };

        i2c-ds@2 {
            reg = <2>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * Infineon IRPS5401 is a complete power management unit
             * delivering up to 5 output voltages. NOTE that the PMBus
             * addresses are required, not i2c.
             */
            pmbus-pmic1@43 {
                compatible = "infineon,irps5401";
                reg = <0x00000043>;
            };
            pmbus-pmic2@44 {
                compatible = "infineon,irps5401";
                reg = <0x00000044>;
            };
        };

        i2c-ds@3 {
            reg = <3>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * TI INA226 is an ultra-precise current shunt and power monitor.
             */
            i2c-vcc@40 {
                compatible = "ti,ina226";
                #io-channel-cells = <1>;
                reg = <0x00000040>;
                shunt-resistor = <5000>;
            };
        };

        i2c-ds@5 {
            reg = <5>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * FPGA mezzanine card FMC LPC Connector J5.
             */
        };

        i2c-ds@7 {
            reg = <7>;

            #address-cells = <1>;
            #size-cells = <0>;

            /*
             * PL DDR4 SODIMM Connector J1 at 0x51.
             */
        };
    };
};
