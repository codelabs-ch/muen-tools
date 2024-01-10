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

      Output_Dir : constant String := "obj/test-iommu-write";
      Path       : constant String := Output_Dir & "/skp-iommu.ad";
   begin
      if not Ada.Directories.Exists (Name => Output_Dir) then
         Ada.Directories.Create_Directory (New_Directory => Output_Dir);
      end if;

      Write_X86_64:
      declare
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Path & "s",
                  Filename2 => "data/skp-iommu.ads"),
                 Message   => "IOMMU spec mismatch (x86/64)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Path & "b",
                  Filename2 => "data/skp-iommu.adb"),
                 Message   => "IOMMU body mismatch (x86/64)");

         Ada.Directories.Delete_File (Name => Path & "s");
         Ada.Directories.Delete_File (Name => Path & "b");
      end Write_X86_64;

      Write_ARMv8a:
      declare
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy-armv8a.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Path & "s",
                  Filename2 => "data/skp-iommu-armv8a.ref"),
                 Message   => "IOMMU spec mismatch (ARMv8a)");
         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end Write_ARMv8a;

--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Write_ARMv8a (Gnattest_T : in out Test);
   procedure Test_Write_ARMv8a_aa89b6 (Gnattest_T : in out Test) renames Test_Write_ARMv8a;
--  id:2.2/aa89b6b5d2a505b7/Write_ARMv8a/1/0/
   procedure Test_Write_ARMv8a (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Output_Dir : constant String := "obj/test-iommu-write";
      Spec       : constant String := Output_Dir & "/skp-iommu.ads";

      Policy : Muxml.XML_Data_Type;
   begin
      if not Ada.Directories.Exists (Name => Output_Dir) then
         Ada.Directories.Create_Directory (New_Directory => Output_Dir);
      end if;

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy-armv8a.xml");

      declare
         Devices : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='lnx2']/devices");
         USB_Device : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='lnx2']/devices" &
              "/device[@physical='USB3_1_XHCI']");

         Tmp : DOM.Core.Node;
         pragma Unreferenced (Tmp);
      begin
            Tmp := DOM.Core.Nodes.Remove_Child
              (N         => Devices,
               Old_Child => USB_Device);

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Spec,
                  Filename2 => "data/skp-iommu_nousb-armv8a.ref"),
                 Message   =>
                   "IOMMU spec mismatch (ARMv8a, USB removed)");
         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end;

--  begin read only
   end Test_Write_ARMv8a;
--  end read only


--  begin read only
   procedure Test_Write_X86_64 (Gnattest_T : in out Test);
   procedure Test_Write_X86_64_93a87b (Gnattest_T : in out Test) renames Test_Write_X86_64;
--  id:2.2/93a87b7e3333f8da/Write_X86_64/1/0/
   procedure Test_Write_X86_64 (Gnattest_T : in out Test) is
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
              Message   => "IOMMU spec mismatch (x86/64)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Path & "b",
               Filename2 => "data/skp-iommu.adb"),
              Message   => "IOMMU body mismatch (x86/64)");
      Ada.Directories.Delete_Tree (Directory => Output_Dir);

--  begin read only
   end Test_Write_X86_64;
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
