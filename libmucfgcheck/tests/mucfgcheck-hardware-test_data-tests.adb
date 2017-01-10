--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Hardware.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mucfgcheck.Hardware.Test_Data.Tests is


--  begin read only
   procedure Test_Memory_Space (Gnattest_T : in out Test);
   procedure Test_Memory_Space_20d775 (Gnattest_T : in out Test) renames Test_Memory_Space;
--  id:2.2/20d775fbae27b871/Memory_Space/1/0/
   procedure Test_Memory_Space (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:25:4:Memory_Space
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/"
         & "memoryBlock[@name='extended_mem_1']",
         Name  => "size",
         Value => "16#1000#");

      begin
         Memory_Space (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Allocated 16#13ec_f000# bytes of physical memory but "
                    & "only 16#042a_1000# bytes available by the hardware",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Space;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Overlap_12597b (Gnattest_T : in out Test) renames Test_Memory_Block_Overlap;
--  id:2.2/12597bd947918ca6/Memory_Block_Overlap/1/0/
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:28:4:Memory_Block_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock[@name='base_mem']",
         Name  => "size",
         Value => "16#1000_0000#");

      begin
         Memory_Block_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of hardware memory block 'base_mem' and"
                    & " 'extended_mem_1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Block_Overlap;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Size (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Size_2aa436 (Gnattest_T : in out Test) renames Test_Memory_Block_Size;
--  id:2.2/2aa436e73a56a43f/Memory_Block_Size/1/0/
   procedure Test_Memory_Block_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:31:4:Memory_Block_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Memory_Block_Size (XML_Data => Data);

      --  Set invalid memory block size.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock",
         Name  => "size",
         Value => "16#0123#");

      begin
         Memory_Block_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0123#' of 'base_mem' hardware "
                    & "memory block element not multiple of page size (4K)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Block_Size;
--  end read only


--  begin read only
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test);
   procedure Test_PCI_Config_Space_Address_4663d9 (Gnattest_T : in out Test) renames Test_PCI_Config_Space_Address;
--  id:2.2/4663d97b4d1f43a4/PCI_Config_Space_Address/1/0/
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:35:4:PCI_Config_Space_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices",
         Name  => "pciConfigAddress",
         Value => "");

      begin
         PCI_Config_Space_Address (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing PCI configuration space address",
                    Message   => "Exception mismatch");
      end;

--  begin read only
   end Test_PCI_Config_Space_Address;
--  end read only


--  begin read only
   procedure Test_CPU_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Count_07d30c (Gnattest_T : in out Test) renames Test_CPU_Count;
--  id:2.2/07d30c7e1521c027/CPU_Count/1/0/
   procedure Test_CPU_Count (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:38:4:CPU_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor",
         Name  => "cpuCores",
         Value => "2");

      begin
         CPU_Count (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System requires 4 but hardware only provides 2 CPU(s)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_Count;
--  end read only


--  begin read only
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test);
   procedure Test_IOMMU_Presence_6c934e (Gnattest_T : in out Test) renames Test_IOMMU_Presence;
--  id:2.2/6c934e0540bf7353/IOMMU_Presence/1/0/
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:41:4:IOMMU_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Presence (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='iommu']",
         Name  => "name",
         Value => "foo");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_2']/"
         & "capabilities/capability[@name='iommu']",
         Name  => "name",
         Value => "bar");

      begin
         IOMMU_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Two IOMMU devices are required, found 0 in hardware "
                    & "spec",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IOMMU_Presence;
--  end read only


--  begin read only
   procedure Test_IOMMU_Cap_Agaw (Gnattest_T : in out Test);
   procedure Test_IOMMU_Cap_Agaw_f3e91e (Gnattest_T : in out Test) renames Test_IOMMU_Cap_Agaw;
--  id:2.2/f3e91eeb5d9a71cb/IOMMU_Cap_Agaw/1/0/
   procedure Test_IOMMU_Cap_Agaw (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:45:4:IOMMU_Cap_Agaw
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Cap  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Cap_Agaw (XML_Data => Data);

      --  Unknown AGAW value.

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='agaw']/text()");

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "51");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "AGAW capability of IOMMU 'iommu_1' set to invalid "
                    & "value '51'",
                    Message   => "Exception mismatch");
      end;

      --  AGAW value not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "AGAW capability of IOMMU 'iommu_1' is not set",
                    Message   => "Exception mismatch");
      end;

      --  Differing AGAW values.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "48");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMUs have different AGAW capabilities set ('48' vs. "
                    & "'39')",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IOMMU_Cap_Agaw;
--  end read only


--  begin read only
   procedure Test_IOMMU_Cap_Register_Offsets (Gnattest_T : in out Test);
   procedure Test_IOMMU_Cap_Register_Offsets_8d8dd2 (Gnattest_T : in out Test) renames Test_IOMMU_Cap_Register_Offsets;
--  id:2.2/8d8dd224a6cf5960/IOMMU_Cap_Register_Offsets/1/0/
   procedure Test_IOMMU_Cap_Register_Offsets (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:48:4:IOMMU_Cap_Register_Offsets
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Cap  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Cap_Register_Offsets (XML_Data => Data);

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='fr_offset']/text()");

      --  FRO value not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'fr_offset' of IOMMU 'iommu_1' is not set",
                    Message   => "Exception mismatch (1)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "512");

      --  FRO invalid.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "16369");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'fr_offset' of IOMMU 'iommu_1' not in "
                    & "allowed range 0 .. 16368",
                    Message   => "Exception mismatch (2)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "512");

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='iotlb_invalidate_offset']/text()");

      --  IOTLB invalidate register offset not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'iotlb_invalidate_offset' of IOMMU 'iommu_1'"
                    & " is not set",
                    Message   => "Exception mismatch (3)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "264");

      --  IOTLB invalidate register offset invalid.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "16377");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'iotlb_invalidate_offset' of IOMMU 'iommu_1'"
                    & " not in allowed range 0 .. 16376",
                    Message   => "Exception mismatch");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "264");
--  begin read only
   end Test_IOMMU_Cap_Register_Offsets;
--  end read only


--  begin read only
   procedure Test_System_Board_Presence (Gnattest_T : in out Test);
   procedure Test_System_Board_Presence_06a6c3 (Gnattest_T : in out Test) renames Test_System_Board_Presence;
--  id:2.2/06a6c3de430e8a9f/System_Board_Presence/1/0/
   procedure Test_System_Board_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:51:4:System_Board_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      System_Board_Presence (XML_Data => Policy);

      Node := DOM.Core.Nodes.Remove_Child
        (N         => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices"),
         Old_Child => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices/device[capabilities/"
            & "capability/@name='systemboard']"));

      begin
         System_Board_Presence (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required system board device with reset port missing",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_System_Board_Presence;
--  end read only

end Mucfgcheck.Hardware.Test_Data.Tests;
