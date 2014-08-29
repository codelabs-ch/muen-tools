--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expand.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expand.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_aabd4c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/aabd4c81c4d5a23a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  expand.ads:25:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      --  Execute run procedure.
      procedure Execute_Run;

      --  Test exception handling by trying to expand an invalid src policy.
      procedure Trigger_Exception;

      ----------------------------------------------------------------------

      procedure Execute_Run
      is
         Filename : constant String := "obj/execute_run.xml";
         Policy   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Run (Policy      => Policy,
              Output_File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => "data/execute_run.ref.xml"),
                 Message   => "Policy mismatch");

         Ada.Directories.Delete_File (Name => Filename);
         Assert (Condition => Stage1.Pre_Checks.Get_Count = 0,
                 Message   => "Pre-checks not zero (stage 1)");
         Assert (Condition => Stage2.Pre_Checks.Get_Count = 0,
                 Message   => "Pre-checks not zero (stage 2)");
         Assert (Condition => Stage2.Expansion.Get_Count = 0,
                 Message   => "Expanders not zero (stage 2)");
         Assert (Condition => Post_Checks.Get_Count = 0,
                 Message   => "Post-checks not zero");
      end Execute_Run;

      ----------------------------------------------------------------------

      procedure Trigger_Exception
      is
         Filename : constant String := "obj/execute_run.xml";
         Policy   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         --  Trigger validation error via invalid physical memory reference.

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='dummy']",
            Name  => "name",
            Value => "foobar");

         begin
            Run (Policy      => Policy,
                 Output_File => Filename);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : others => null;
         end;

         Assert (Condition => Stage1.Pre_Checks.Get_Count = 0,
                 Message   => "Pre-checks not zero (stage 1)");
         Assert (Condition => Stage2.Pre_Checks.Get_Count = 0,
                 Message   => "Pre-checks not zero (stage 2)");
         Assert (Condition => Stage2.Expansion.Get_Count = 0,
                 Message   => "Expanders not zero (stage2)");
         Assert (Condition => Post_Checks.Get_Count = 0,
                 Message   => "Post-checks not zero");
      end Trigger_Exception;
   begin
      Execute_Run;
      Trigger_Exception;
--  begin read only
   end Test_Run;
--  end read only

end Expand.Test_Data.Tests;
