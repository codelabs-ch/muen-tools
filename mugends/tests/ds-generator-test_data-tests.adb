--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DS.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Test_Utils;

--  begin read only
--  end read only
package body DS.Generator.Test_Data.Tests is

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

      Policy : Muxml.XML_Data_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) write deploy scripts to obj directory
      Write (Output_Dir => "obj",
             Policy     => Policy);

      --  (3) test reference files
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootgen_config_full.ref",
               Filename2 => "obj/bootgen.config"),
              Message   => "bootgen.config mismatch (full test policy)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootscript_cmd_full.ref",
               Filename2 => "obj/bootscript.cmd"),
              Message   => "bootscript.cmd mismatch (full test policy)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/gdbinit_config_full.ref",
               Filename2 => "obj/gdbinit.config"),
              Message   => "gdbinit.config mismatch (full test policy)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/xsct_cmd_full.ref",
               Filename2 => "obj/xsct.cmd"),
              Message   => "xsct.cmd mismatch (full test policy)");

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
end DS.Generator.Test_Data.Tests;
