--D @Interface
--D This package contains the transient-execution mitigation policy facts
--D required at boot, in particular the minimum microcode revision.
package Skp.Mitigations
is

   --  Minimum microcode revision the loaded update must meet.
   Min_MCU_Revision : constant := __min_mcu_revision__;

end Skp.Mitigations;
