--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.FADT.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Acpi.FADT.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_c33834 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/c3383472bb920369/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Linux_FADT : constant String := "obj/linux_fadt";
   begin
      Write (DSDT_Address => 16#000e_3000#,
             Filename     => Linux_FADT);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_fadt.ref",
               Filename2 => Linux_FADT),
              Message   => "FADT mismatch");

      Ada.Directories.Delete_File (Name => Linux_FADT);
--  begin read only
   end Test_Write;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Acpi.FADT.Test_Data.Tests;
