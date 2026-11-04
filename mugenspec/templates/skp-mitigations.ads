--D @Interface
--D This package contains the transient-execution mitigation policy facts
--D required at boot - the minimum microcode revision and the
--D IA32_ARCH_CAPABILITIES required bits.
package Skp.Mitigations
is

   --  Minimum microcode revision the loaded update must meet.
   Min_MCU_Revision : constant := __min_mcu_revision__;

   --  IA32_ARCH_CAPABILITIES (MSR 0x10A) required mask checked at boot:
   --  (Value and Mask) = Mask.
   Arch_Cap_Mask : constant := __arch_cap_mask__;

end Skp.Mitigations;
