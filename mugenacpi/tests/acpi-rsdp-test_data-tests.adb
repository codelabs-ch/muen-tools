--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.RSDP.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Acpi.RSDP.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_c33834 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/c3383472bb920369/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  acpi-rsdp.ads:20:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Linux_RSDP : constant String := "obj/linux_rsdp";
   begin
      Write (XSDT_Address => 16#000e_1000#,
             Filename     => Linux_RSDP);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_rsdp.ref",
               Filename2 => Linux_RSDP),
              Message   => "RSDP mismatch");

      Ada.Directories.Delete_File (Name => Linux_RSDP);
--  begin read only
   end Test_Write;
--  end read only

end Acpi.RSDP.Test_Data.Tests;
