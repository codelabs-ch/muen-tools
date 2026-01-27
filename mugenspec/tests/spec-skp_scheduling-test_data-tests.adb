--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Scheduling.Test_Data.

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
package body Spec.Skp_Scheduling.Test_Data.Tests is

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
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Output_Dir : constant String := "obj";
      Spec_File  : constant String := Output_Dir & "/skp-scheduling.ads";
   begin
      Write_X86_64 :
      declare
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/skp-scheduling.ads",
                  Filename2 => Spec_File),
                 Message   => "Scheduling spec mismatch (x86/64)");
         Ada.Directories.Delete_File (Name => Spec_File);
      end Write_X86_64;

      Write_ARM64 :
      declare
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy-armv8a.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/skp-scheduling-arm64.ref",
                  Filename2 => Spec_File),
                 Message   => "Scheduling spec mismatch (ARM64)");
         Ada.Directories.Delete_File (Name => Spec_File);
      end Write_ARM64;
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
end Spec.Skp_Scheduling.Test_Data.Tests;
