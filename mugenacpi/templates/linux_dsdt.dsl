DefinitionBlock ("DSDT.aml", "DSDT", 2, "Muen  ", "Homebrew", 0x00000000)
{
    Device (BORD)
    {
        Name (_HID, EisaId ("PNP0C01"))
        Name (_UID, "Muen Board")
        Method (_CRS)
        {
            Return (ResourceTemplate () {
                DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed, NonCacheable, ReadWrite,
                    0x00000000,
                    0xf8000000,
                    0xf8ffffff,
                    0x00000000,
                    0x01000000,
                    ,, , AddressRangeReserved, TypeStatic)
            })
        }
    }

    Scope (_SB)
    {
        Device (PCI0)
        {
            Name (_HID, EisaId ("PNP0A08"))
            Name (_CID, EisaId ("PNP0A03"))
            Name (_ADR, Zero)
            Name (_BBN, Zero)
            Method (_CBA, 0, Serialized)
            {
                /* Point to MMConf region */
                Return (0x__config_base_address__)
            }
            Method (_CRS, 0, Serialized)
            {
                Name (MCRS, ResourceTemplate ()
                {
                    /* 16 buses */
                    WordBusNumber (ResourceProducer, MinFixed, MaxFixed, PosDecode,
                        0x0000,             // Granularity
                        0x0000,             // Range Minimum
                        0x000f,             // Range Maximum
                        0x0000,             // Translation Offset
                        0x0010,             // Length
                        ,, )
                    /* All i/o ports */
                    DWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                        0x00000000,         // Granularity
                        0x00000000,         // Range Minimum
                        0x0000FFFF,         // Range Maximum
                        0x00000000,         // Translation Offset
                        0x00010000,         // Length
                        ,, , TypeStatic)
                    /* eth0->mmio1 */
                    DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed, Cacheable, ReadWrite,
                        0x00000000,
                        0xd2500000,
                        0xd251ffff,
                        0x00000000,
                        0x00020000,
                        ,,, AddressRangeMemory, TypeStatic)
                    /* eth0->mmio2 */
                    DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed, Cacheable, ReadWrite,
                        0x00000000,
                        0xd253b000,
                        0xd253bfff,
                        0x00000000,
                        0x00001000,
                        ,,, AddressRangeMemory, TypeStatic)
                })
                Return (MCRS)
            }

            Method (_DMA, 0, NotSerialized)
            {
                Return (ResourceTemplate () {
                    /* 4M~256M are identity mapped and thus available
                     * for DMA. (IMO, Linux ignores this setting) */
                    DWordMemory (ResourceConsumer, PosDecode, MinFixed, MaxFixed, Prefetchable, ReadWrite,
                        0x00000000,
                        0x00400000,
                        0x0fffffff,
                        0x00000000,
                        0x0fc00000,
                        ,, , , )
                })
            }

            Method (_PRT, 0, NotSerialized)
            {
                Return (Package (0x01)
                {
                    /* eth0->eth0_irq */
                    Package (0x04)
                    {
                        0x0019FFFF,
                        0x00,
                        Zero,
                        0x0b
                    }
                })
            }

            Method (_OSC, 4, NotSerialized)
            {
                CreateDWordField (Arg3, Zero, CDW1)
                CreateDWordField (Arg3, One,  CDW2)
                CreateDWordField (Arg3, 0x2,  CDW3)

                If (LEqual (Arg0, ToUUID ("33db4d5b-1ff7-401c-9657-7441c03dd766")))
                {
                    If (LNotEqual (Arg1, 1))
                    {
                        /* Unknown Revision */
                        Or (CDW1, 0x08, CDW1)
                    }
                    /* Clear all capabilities */
                    Store (Zero, CDW2)
                    Store (Zero, CDW3)
                    Return (Arg3)
                }
                Else
                {
                    /* Unknown UUID */
                    Or (CDW1, 0x04, CDW1)
                    Return (Arg3)
                }
            }

            Device (ISA)
            {
                Device (SER1)
                {
                    Name (_HID, EisaId ("PNP0501"))
                    Name (_UID, "debugport")
                    Method (_STA)
                    {
                       Return (0x0f)
                    }
                    Method (_CRS)
                    {
                        Return (ResourceTemplate () {
                            IO (Decode16, 0x3f8, 0x3f8, 0x08, 0x08,)
                            IRQNoFlags () { 0 }
                        })
                    }
                }
            }
        }
    }

    Name (_S0, Package (0x04)
    {
        Zero,
        Zero,
        Zero,
        Zero
    })

    Name (_S5, Package (0x04)
    {
        Zero,
        Zero,
        Zero,
        Zero
    })
}

