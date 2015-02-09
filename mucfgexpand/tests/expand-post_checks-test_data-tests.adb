--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expand.Post_Checks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expand.Post_Checks.Test_Data.Tests is


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_86826d (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/86826d71989a86e2/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  expand-post_checks.ads:26:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Register_All (Data => Policy);
      Assert (Condition => Check_Procs.Get_Count = 2,
              Message   => "Count mismatch:" & Get_Count'Img);
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_9b6b0d (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/9b6b0dee792a1a08/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  expand-post_checks.ads:29:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Run (Data => Policy);
      Assert (Condition => Test_Counter = 1,
              Message   => "Counter not 1:" & Test_Counter'Img);
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  expand-post_checks.ads:32:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Count = 0,
              Message   => "Count not zero");

      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Count not 1");
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  expand-post_checks.ads:35:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not empty");

      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count /= 0,
              Message   => "Procs count still zero");

      Clear;
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not cleared");
--  begin read only
   end Test_Clear;
--  end read only

end Expand.Post_Checks.Test_Data.Tests;
