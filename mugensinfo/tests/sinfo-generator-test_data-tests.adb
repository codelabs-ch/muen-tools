--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Generator.Test_Data.

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
package body Sinfo.Generator.Test_Data.Tests is

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

      ----------------------------------------------------------------------

      procedure Write_Sinfo (Arch : String)
      is
         Policy        : Muxml.XML_Data_Type;
         Subject_Sinfo : constant String := "obj/lnx_sinfo";
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy_" & Arch & ".xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/lnx_sinfo",
                  Filename2 => Subject_Sinfo),
                 Message   => "Subject info file mismatch (" & Arch & ")");
         Ada.Directories.Delete_File (Name => Subject_Sinfo);
      end Write_Sinfo;
   begin
      Write_Sinfo (Arch => "x86_64");
      Write_Sinfo (Arch => "arm64");
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
end Sinfo.Generator.Test_Data.Tests;
