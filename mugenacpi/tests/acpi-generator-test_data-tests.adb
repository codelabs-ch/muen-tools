--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Acpi.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  acpi-generator.ads:26:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Linux_RSDP : constant String := "obj/linux_rsdp";
      Linux_XSDT : constant String := "obj/linux_xsdt";
      Linux_FADT : constant String := "obj/linux_fadt";
      Linux_DSDT : constant String := "obj/linux_dsdt";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_rsdp.ref",
               Filename2 => Linux_RSDP),
              Message   => "RSDP table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_xsdt.ref",
               Filename2 => Linux_XSDT),
              Message   => "XSDT table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_fadt.ref",
               Filename2 => Linux_FADT),
              Message   => "FADT table mismatch");

      Ada.Directories.Delete_File (Name => Linux_RSDP);
      Ada.Directories.Delete_File (Name => Linux_XSDT);
      Ada.Directories.Delete_File (Name => Linux_FADT);
      Ada.Directories.Delete_File (Name => Linux_DSDT & ".dsl");
      Ada.Directories.Delete_File (Name => Linux_DSDT & ".aml");
--  begin read only
   end Test_Write;
--  end read only

end Acpi.Generator.Test_Data.Tests;