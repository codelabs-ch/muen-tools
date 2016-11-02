--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Pre_Checks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Pack.Pre_Checks.Test_Data.Tests is


--  begin read only
   procedure Test_Files_Size (Gnattest_T : in out Test);
   procedure Test_Files_Size_d33017 (Gnattest_T : in out Test) renames Test_Files_Size;
--  id:2.2/d3301771bce21920/Files_Size/1/0/
   procedure Test_Files_Size (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:26:4:Files_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure File_Larger_Than_Memory
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Mucfgcheck.Files.Set_Input_Directory (Dir => "data");

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Policy,
            Name        => "linux|acpi_rsdp",
            Address     => "16#0010_0000#",
            Size        => "16#0000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "subject_acpi_rsdp",
            File_Name   => "pattern",
            File_Offset => "none");

         begin
            Files_Size (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Check_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "File 'data/pattern' too large for physical memory"
                       & " region 'linux|acpi_rsdp': 16#001e# > 16#0000#",
                       Message   => "Exception mismatch");
         end;
      end File_Larger_Than_Memory;

      ----------------------------------------------------------------------

      procedure Offset_Larger_Than_File
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Mucfgcheck.Files.Set_Input_Directory (Dir => "data");

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Policy,
            Name        => "linux|acpi_rsdp",
            Address     => "16#0010_0000#",
            Size        => "16#0000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "subject_acpi_rsdp",
            File_Name   => "pattern",
            File_Offset => "16#ffff#");

         begin
            Files_Size (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Check_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Offset of file 'data/pattern' referenced by "
                       & "physical memory region 'linux|acpi_rsdp' larger than "
                       & "file size: 16#ffff# > 16#001e#",
                       Message   => "Exception mismatch");
         end;
      end Offset_Larger_Than_File;
   begin
      File_Larger_Than_Memory;
      Offset_Larger_Than_File;
--  begin read only
   end Test_Files_Size;
--  end read only


--  begin read only
   procedure Test_Unresolved_Hash_References (Gnattest_T : in out Test);
   procedure Test_Unresolved_Hash_References_0f3dc3 (Gnattest_T : in out Test) renames Test_Unresolved_Hash_References;
--  id:2.2/0f3dc38b88516469/Unresolved_Hash_References/1/0/
   procedure Test_Unresolved_Hash_References (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:29:4:Unresolved_Hash_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Impl   : DOM.Core.DOM_Implementation;
      Node   : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Unresolved_Hash_References (Data => Policy);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "hashRef");
      Muxml.Utils.Append_Child
        (Node      => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory"),
         New_Child => Node);

      begin
         Unresolved_Hash_References (Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Policy contains 1 unresolved hash reference(s)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Unresolved_Hash_References;
--  end read only


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:32:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Register_All;
      Assert (Condition => Check_Procs.Get_Count = 3,
              Message   => "Count mismatch:" & Check_Procs.Get_Count'Img);
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_aabd4c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/aabd4c81c4d5a23a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:35:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Unused : Muxml.XML_Data_Type;
   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Run (Data      => Unused,
           Input_Dir => "input_dir");

      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch");
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:40:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Check_Procs.Get_Count'Img);
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  pack-pre_checks.ads:43:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Check_Procs.Get_Count'Img);

      Clear;
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not cleared:" & Check_Procs.Get_Count'Img);
--  begin read only
   end Test_Clear;
--  end read only

end Pack.Pre_Checks.Test_Data.Tests;
