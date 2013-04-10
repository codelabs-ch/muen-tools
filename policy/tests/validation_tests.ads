with Ahven.Framework;

package Validation_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Verify invalid memory region size check.
   procedure Invalid_Memregion_Size;

   --  Verify invalid memory region addresses.
   procedure Invalid_Memregion_Addrs;

   --  Verify invalid VMXON addresses.
   procedure Invalid_Vmxon_Addrs;

   --  Verify invalid VMCS addresses.
   procedure Invalid_Vmcs_Addrs;

end Validation_Tests;
