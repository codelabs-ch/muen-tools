--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_IOMMU.Test_Data.

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
package body Spec.Skp_IOMMU.Test_Data.Tests is

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

      Policy     : Muxml.XML_Data_Type;
      Dummy      : DOM.Core.Node;
      Output_Dir : constant String := "obj/test-iommu-write";
      Path       : constant String := Output_Dir & "/skp-iommu.ad";
   begin
      if not Ada.Directories.Exists (Name => Output_Dir) then
         Ada.Directories.Create_Directory (New_Directory => Output_Dir);
      end if;

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Path & "s",
               Filename2 => "data/skp-iommu.ads"),
              Message   => "IOMMU spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Path & "b",
               Filename2 => "data/skp-iommu.adb"),
              Message   => "IOMMU body mismatch");
      Ada.Directories.Delete_Tree (Directory => Output_Dir);

      Ada.Directories.Create_Directory (New_Directory => Output_Dir);
      Dummy := DOM.Core.Nodes.Remove_Child
        (N         => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices"),
         Old_Child => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices/device[@name='iommu_2']"));

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      declare
         Spec_Content : constant String
           := Test_Utils.Read_File (Filename => Path & "s");
         Body_Content : constant String
           := Test_Utils.Read_File (Filename => Path & "b");
      begin
         Assert (Condition => Ada.Strings.Fixed.Index
                 (Source  => Spec_Content,
                  Pattern => "pragma Warnings (GNATprove, Off,") > 0,
                 Message   => "Pragma Warnings Off not found in spec");
         Assert (Condition => Ada.Strings.Fixed.Index
                 (Source  => Spec_Content,
                  Pattern => "pragma Warnings (GNATprove, On,") > 0,
                 Message   => "Pragma Warnings On not found in spec");
         Assert (Condition => Ada.Strings.Fixed.Index
                 (Source  => Body_Content,
                  Pattern => "pragma Warnings (GNATprove, Off,") > 0,
                 Message   => "Pragma Warnings Off not found in body");
         Assert (Condition => Ada.Strings.Fixed.Index
                 (Source  => Body_Content,
                  Pattern => "pragma Warnings (GNATprove, On,") > 0,
                 Message   => "Pragma Warnings On not found in body");
      end;
      Ada.Directories.Delete_Tree (Directory => Output_Dir);
--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Write_Empty (Gnattest_T : in out Test);
   procedure Test_Write_Empty_eaf063 (Gnattest_T : in out Test) renames Test_Write_Empty;
--  id:2.2/eaf063fe0f04d4a6/Write_Empty/1/0/
   procedure Test_Write_Empty (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Output_Dir : constant String := "obj";
      Name       : constant String := Output_Dir & "/skp-iommu.ad";
   begin
      Write_Empty (Output_Dir => Output_Dir);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Name & "s",
               Filename2 => "data/skp-iommu_noiommus_ads.ref"),
              Message   => "IOMMU spec mismatch");
      Ada.Directories.Delete_File (Name => Name & "s");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Name & "b",
               Filename2 => "data/skp-iommu_noiommus_adb.ref"),
              Message   => "IOMMU body mismatch");
      Ada.Directories.Delete_File (Name => Name & "b");
--  begin read only
   end Test_Write_Empty;
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
end Spec.Skp_IOMMU.Test_Data.Tests;
