--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.XML_Utils.Test_Data.

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
package body Expanders.XML_Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Source_Event_Node (Gnattest_T : in out Test);
   procedure Test_Create_Source_Event_Node_5e651b (Gnattest_T : in out Test) renames Test_Create_Source_Event_Node;
--  id:2.2/5e651bdd7c1b31ff/Create_Source_Event_Node/1/0/
   procedure Test_Create_Source_Event_Node (Gnattest_T : in out Test) is
   --  expanders-xml_utils.ads:30:4:Create_Source_Event_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "log_name";
      Physical : constant String := "phys_name";
      ID       : constant String := "42";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := Create_Source_Event_Node
        (Policy        => Policy,
         ID            => ID,
         Logical_Name  => Logical,
         Physical_Name => Physical);

      Assert (Condition => DOM.Core.Elements.Get_Tag_Name
              (Elem => Node) = "event",
              Message   => "Event tag mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "id") = ID,
              Message   => "ID mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical") = Physical,
              Message   => "Physical name mismatch");
--  begin read only
   end Test_Create_Source_Event_Node;
--  end read only


--  begin read only
   procedure Test_Create_Target_Event_Node (Gnattest_T : in out Test);
   procedure Test_Create_Target_Event_Node_205897 (Gnattest_T : in out Test) renames Test_Create_Target_Event_Node;
--  id:2.2/2058979e74b10a92/Create_Target_Event_Node/1/0/
   procedure Test_Create_Target_Event_Node (Gnattest_T : in out Test) is
   --  expanders-xml_utils.ads:38:4:Create_Target_Event_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "log_name";
      Physical : constant String := "phys_name";
      Vect_Nr  : constant String := "48";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := Create_Target_Event_Node
        (Policy        => Policy,
         Logical_Name  => Logical,
         Physical_Name => Physical,
         Vector        => "");

      Assert (Condition => DOM.Core.Elements.Get_Tag_Name
              (Elem => Node) = "event",
              Message   => "Event tag mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical") = Physical,
              Message   => "Physical name mismatch");
      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Event action added");

       Node := Create_Target_Event_Node
        (Policy        => Policy,
         Logical_Name  => Logical,
         Physical_Name => Physical,
         Vector        => Vect_Nr);
      Assert (Condition => DOM.Core.Elements.Get_Tag_Name
              (Elem => DOM.Core.Nodes.First_Child (N => Node))
              = "inject_interrupt",
              Message   => "Inject interrupt action missing");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.First_Child (N => Node),
               Name => "vector") = Vect_Nr,
              Message   => "Vector number mismatch");
--  begin read only
   end Test_Create_Target_Event_Node;
--  end read only


--  begin read only
   procedure Test_Create_Logical_Device_Node (Gnattest_T : in out Test);
   procedure Test_Create_Logical_Device_Node_9b6840 (Gnattest_T : in out Test) renames Test_Create_Logical_Device_Node;
--  id:2.2/9b6840d9a3a104cc/Create_Logical_Device_Node/1/0/
   procedure Test_Create_Logical_Device_Node (Gnattest_T : in out Test) is
   --  expanders-xml_utils.ads:46:4:Create_Logical_Device_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "log_name";
      Physical : constant String := "phys_name";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := Create_Logical_Device_Node
        (Policy        => Policy,
         Logical_Name  => Logical,
         Physical_Name => Physical);

      Assert (Condition => DOM.Core.Elements.Get_Tag_Name
              (Elem => Node) = "device",
              Message   => "Device tag mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical") = Physical,
              Message   => "Physical name mismatch");
--  begin read only
   end Test_Create_Logical_Device_Node;
--  end read only


--  begin read only
   procedure Test_Calculate_PT_Size (Gnattest_T : in out Test);
   procedure Test_Calculate_PT_Size_310d80 (Gnattest_T : in out Test) renames Test_Calculate_PT_Size;
--  id:2.2/310d8086d72be4ec/Calculate_PT_Size/1/0/
   procedure Test_Calculate_PT_Size (Gnattest_T : in out Test) is
   --  expanders-xml_utils.ads:57:4:Calculate_PT_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/calculate_pt.xml");

      Assert
        (Condition => Calculate_PT_Size
           (Policy             => Policy,
            Paging_Levels      => 4,
            Large_Pages        => True,
            Dev_Virt_Mem_XPath => "/system/kernel/devices/device/memory",
            Virt_Mem_XPath     => "/system/kernel/memory/cpu[@id='0']/memory")
         = 16#6000#,
         Message   => "Size mismatch");
--  begin read only
   end Test_Calculate_PT_Size;
--  end read only


--  begin read only
   procedure Test_Calculate_Region_Address (Gnattest_T : in out Test);
   procedure Test_Calculate_Region_Address_c8560f (Gnattest_T : in out Test) renames Test_Calculate_Region_Address;
--  id:2.2/c8560fd59646ebdc/Calculate_Region_Address/1/0/
   procedure Test_Calculate_Region_Address (Gnattest_T : in out Test) is
   --  expanders-xml_utils.ads:68:4:Calculate_Region_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      declare
         use type Interfaces.Unsigned_64;

         Virt_Mem : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/subjects/subject[@name='subject1']/memory/"
              & "memory");
         Dev_Mem  :  constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/subjects/subject[@name='subject1']/devices/"
              & "device/memory");
      begin
         Assert (Condition => Calculate_Region_Address
                 (Policy             => Policy,
                  Fixed_Memory       => Virt_Mem,
                  Device_Memory      => Dev_Mem,
                  Address_Space_Size => Interfaces.Unsigned_64'Last,
                  Region_Size        => 16#1000#) = 16#0000#,
                 Message   => "Region address mismatch (1)");

         Assert (Condition => Calculate_Region_Address
                 (Policy             => Policy,
                  Fixed_Memory       => Virt_Mem,
                  Device_Memory      => Dev_Mem,
                  Address_Space_Size => Interfaces.Unsigned_64'Last,
                  Region_Size        => 16#2000#) = 16#2000#,
                 Message   => "Region address mismatch (2)");
      end;
--  begin read only
   end Test_Calculate_Region_Address;
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
end Expanders.XML_Utils.Test_Data.Tests;
