--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutools.XML_Utils.Test_Data.Tests is


--  begin read only
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_6142bd (Gnattest_T : in out Test) renames Test_1_Add_Memory_Region;
--  id:2.2/6142bd7e03979890/Add_Memory_Region/1/0/
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:29:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#9000_1000#",
         Size        => "16#3000#",
         Caching     => "UC",
         Alignment   => "16#1000#",
         Memory_Type => "");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "noaddress",
         Address     => "",
         Size        => "16#8000#",
         Caching     => "WC",
         Alignment   => "16#0020_0000#",
         Memory_Type => "");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_1_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_741476 (Gnattest_T : in out Test) renames Test_2_Add_Memory_Region;
--  id:2.2/741476e4715f4faa/Add_Memory_Region/0/0/
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:40:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory_with_file.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#2000#",
         Size        => "16#4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "",
         File_Name   => "testfile",
         File_Offset => "16#1000#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory_with_file.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_2_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Node_4c3d32 (Gnattest_T : in out Test) renames Test_Create_Memory_Node;
--  id:2.2/4c3d32628a5ec36a/Create_Memory_Node/1/0/
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:52:4:Create_Memory_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := Create_Memory_Node (Policy      => Data,
                                  Name        => "region1",
                                  Address     => "16#1000#",
                                  Size        => "16#0200_1000#",
                                  Caching     => "WB",
                                  Alignment   => "16#1000#",
                                  Memory_Type => "subject");

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "name") = "region1",
              Message   => "Name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physicalAddress") = "16#1000#",
              Message   => "Address mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "size") = "16#0200_1000#",
              Message   => "Size mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "caching") = "WB",
              Message   => "Caching mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "alignment") = "16#1000#",
              Message   => "Alignment mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "type") = "subject",
              Message   => "Memory type mismatch");

      Node := Create_Memory_Node (Policy      => Data,
                                  Name        => "region2",
                                  Address     => "",
                                  Size        => "16#1000#",
                                  Caching     => "UC",
                                  Alignment   => "",
                                  Memory_Type => "");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physicalAddress") = "",
              Message   => "Address set");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "alignment") = "",
              Message   => "Alignment set");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "type") = "",
              Message   => "Memory type set");


--  begin read only
   end Test_Create_Memory_Node;
--  end read only


--  begin read only
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test);
   procedure Test_Has_Managed_DEBUGCTL_07c840 (Gnattest_T : in out Test) renames Test_Has_Managed_DEBUGCTL;
--  id:2.2/07c840ea4cf93188/Has_Managed_DEBUGCTL/1/0/
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:64:4:Has_Managed_DEBUGCTL
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_DEBUGCTL (Controls => Ctrls),
              Message   => "DEBUGCTL is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadDebugControls")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveDebugControls")),
         Value => "1");
      Assert (Condition => Has_Managed_DEBUGCTL (Controls => Ctrls),
              Message   => "DEBUGCTL not managed");
--  begin read only
   end Test_Has_Managed_DEBUGCTL;
--  end read only


--  begin read only
   procedure Test_Has_Managed_PERFGLOBALCTRL (Gnattest_T : in out Test);
   procedure Test_Has_Managed_PERFGLOBALCTRL_811a8a (Gnattest_T : in out Test) renames Test_Has_Managed_PERFGLOBALCTRL;
--  id:2.2/811a8a093040ed89/Has_Managed_PERFGLOBALCTRL/1/0/
   procedure Test_Has_Managed_PERFGLOBALCTRL (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:68:4:Has_Managed_PERFGLOBALCTRL
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_PERFGLOBALCTRL (Controls => Ctrls),
              Message   => "PERFGLOBALCTL is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32PERFGLOBALCTRL")),
         Value => "1");
      Assert (Condition => Has_Managed_PERFGLOBALCTRL (Controls => Ctrls),
              Message   => "DEBUGCTL not managed");
--  begin read only
   end Test_Has_Managed_PERFGLOBALCTRL;
--  end read only


--  begin read only
   procedure Test_Has_Managed_PAT (Gnattest_T : in out Test);
   procedure Test_Has_Managed_PAT_0e0b54 (Gnattest_T : in out Test) renames Test_Has_Managed_PAT;
--  id:2.2/0e0b54fd46c7da60/Has_Managed_PAT/1/0/
   procedure Test_Has_Managed_PAT (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:74:4:Has_Managed_PAT
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_PAT (Controls => Ctrls),
              Message   => "PAT is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32PAT")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveIA32PAT")),
         Value => "1");
      Assert (Condition => Has_Managed_PAT (Controls => Ctrls),
              Message   => "PAT not managed");
--  begin read only
   end Test_Has_Managed_PAT;
--  end read only


--  begin read only
   procedure Test_Has_Managed_EFER (Gnattest_T : in out Test);
   procedure Test_Has_Managed_EFER_29e528 (Gnattest_T : in out Test) renames Test_Has_Managed_EFER;
--  id:2.2/29e528793cfc9400/Has_Managed_EFER/1/0/
   procedure Test_Has_Managed_EFER (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:78:4:Has_Managed_EFER
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_EFER (Controls => Ctrls),
              Message   => "EFER is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32EFER")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveIA32EFER")),
         Value => "1");
      Assert (Condition => Has_Managed_EFER (Controls => Ctrls),
              Message   => "EFER not managed");
--  begin read only
   end Test_Has_Managed_EFER;
--  end read only


--  begin read only
   procedure Test_Calculate_MSR_Count (Gnattest_T : in out Test);
   procedure Test_Calculate_MSR_Count_5d62ce (Gnattest_T : in out Test) renames Test_Calculate_MSR_Count;
--  id:2.2/5d62ce190007559a/Calculate_MSR_Count/1/0/
   procedure Test_Calculate_MSR_Count (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:83:4:Calculate_MSR_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      MSRs : DOM.Core.Node_List;

      --  Append MSR with given attributes to MSR list.
      procedure Append_MSR
        (MSR_Start : String;
         MSR_End   : String;
         Mode      : String)
      is
         Node : DOM.Core.Node;
      begin
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "msr");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "mode",
            Value => Mode);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => MSR_Start);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => MSR_End);

         DOM.Core.Append_Node (List => MSRs,
                               N    => Node);
      end Append_MSR;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => False) = 0,
              Message   => "Empty list count not 0");

      Append_MSR (MSR_Start => "16#0010#",
                  MSR_End   => "16#0010#",
                  Mode      => "r");
      Append_MSR (MSR_Start => "16#0174#",
                  MSR_End   => "16#0176#",
                  Mode      => "rw");
      Append_MSR (MSR_Start => "16#c000_0080#",
                  MSR_End   => "16#c000_0084#",
                  Mode      => "rw");
      Append_MSR (MSR_Start => "16#c000_0100#",
                  MSR_End   => "16#c000_0102#",
                  Mode      => "rw");

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => True) = 6,
              Message   => "MSR count mismatch");
--  begin read only
   end Test_Calculate_MSR_Count;
--  end read only


--  begin read only
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test);
   procedure Test_Get_Occupied_PCI_Buses_0b9ce6 (Gnattest_T : in out Test) renames Test_Get_Occupied_PCI_Buses;
--  id:2.2/0b9ce63ef86880a3/Get_Occupied_PCI_Buses/1/0/
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:97:4:Get_Occupied_PCI_Buses
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Policy : Muxml.XML_Data_Type;
      Buses  : PCI_Bus_Set.Set;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Buses := Get_Occupied_PCI_Buses (Data => Policy);

      Assert (Condition => Buses.Length = 2,
              Message   => "Bus count not 2");
      Assert (Condition => Buses.First_Element = 16#02#,
              Message   => "First bus not 16#02#");
      Assert (Condition => Buses.Last_Element = 16#23#,
              Message   => "Last bus not 16#23#");
--  begin read only
   end Test_Get_Occupied_PCI_Buses;
--  end read only

end Mutools.XML_Utils.Test_Data.Tests;
