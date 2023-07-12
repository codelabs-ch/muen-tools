--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.APU_Devices.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;

with Mutools.Templates;

--  begin read only
--  end read only
package body DTS.APU_Devices.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_APU_Devices (Gnattest_T : in out Test);
   procedure Test_Add_APU_Devices_2ed404 (Gnattest_T : in out Test) renames Test_Add_APU_Devices;
--  id:2.2/2ed4040d9767c38a/Add_APU_Devices/1/0/
   procedure Test_Add_APU_Devices (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "    amba-apu@20000000 {" & ASCII.LF &
        "        compatible = ""simple-bus"";" & ASCII.LF &
        "        #address-cells = <0x2>;" & ASCII.LF &
        "        #size-cells = <0x2>;" & ASCII.LF &
        "        ranges = <0x00000000 0x00000000 0x00000000 0x00000000 " &
        "0x00000000 0x20002000>;" & ASCII.LF & ASCII.LF &
        "        interrupt-controller@20000000 {" & ASCII.LF &
        "            compatible = ""muensk,irq-v0"";" & ASCII.LF &
        "            #interrupt-cells = <0x3>;" & ASCII.LF &
        "            interrupt-controller;" & ASCII.LF &
        "            reg = <0x00000000 0x20000000 0x00000000 0x00002000>;" &
        ASCII.LF &
        "            interrupts = <GIC_PPI 0x9 (GIC_CPU_MASK_SIMPLE(4) | " &
        "IRQ_TYPE_LEVEL_HIGH)>;" & ASCII.LF &
        "            linux,phandle = <0x7>;" & ASCII.LF &
        "            phandle = <0x7>;" & ASCII.LF &
        "        };" & ASCII.LF & ASCII.LF & "    };";

      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content =>
             "    amba-apu@__amba_apu_base__ {" & ASCII.LF &
             "        compatible = ""simple-bus"";" & ASCII.LF &
             "        #address-cells = <0x2>;" & ASCII.LF &
             "        #size-cells = <0x2>;" & ASCII.LF &
             "        __amba_apu_ranges__" & ASCII.LF &
             "__amba_apu_devices__" & ASCII.LF &
             "    };");

      Policy : Muxml.XML_Data_Type;

      Subject : DOM.Core.Node_List;
   begin
      --  (1) parse test policy  --
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_light.xml");

      --  (2) extract linux subject directly  --
      Subject := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']");

      --  (3) test the device entry according to template --
      Add_APU_Devices (Template  => Template,
                       Policy    => Policy,
                       Subject   => DOM.Core.Nodes.Item
                         (List  => Subject,
                          Index => 0));

      Assert (Actual   => Mutools.Templates.To_String (Template => Template),
              Expected => Expected_Entry,
              Message  => "wrong node entry for APU devices test data");

--  begin read only
   end Test_Add_APU_Devices;
--  end read only


--  begin read only
   procedure Test_Generate_GIC_Node (Gnattest_T : in out Test);
   procedure Test_Generate_GIC_Node_9fc516 (Gnattest_T : in out Test) renames Test_Generate_GIC_Node;
--  id:2.2/9fc5166b355b72da/Generate_GIC_Node/1/0/
   procedure Test_Generate_GIC_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "interrupt-controller@20000000 {" & ASCII.LF &
        "    compatible = ""muensk,irq-v0"";" & ASCII.LF &
        "    #interrupt-cells = <0x3>;" & ASCII.LF &
        "    interrupt-controller;" & ASCII.LF &
        "    reg = <0x00000000 0x20000000 0x00000000 0x00002000>;" & ASCII.LF &
        "    interrupts = <GIC_PPI 0x9 (GIC_CPU_MASK_SIMPLE(4) | " &
        "IRQ_TYPE_LEVEL_HIGH)>;" & ASCII.LF &
        "    linux,phandle = <0x7>;" & ASCII.LF &
        "    phandle = <0x7>;" & ASCII.LF &
        "};"& ASCII.LF;

      Policy : Muxml.XML_Data_Type;

      GIC_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy  --
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_light.xml");

      --  (2) extract generic interrupt controller device directly  --
      GIC_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']/" &
           "devices/device[@physical='APU_GIC']");

      --  (3) test the device entry according to template --
      Generate_GIC_Node (Policy    => Policy,
                         Device    => DOM.Core.Nodes.Item
                           (List  => GIC_Dev,
                            Index => 0),
                         DTS_Entry => Actual_Entry,
                         DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected => Expected_Entry,
              Message  => "wrong device entry for GIC test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_2000_0000#,
              Message   => "wrong register range base for GIC test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_2000#,
              Message   => "wrong register range size for GIC test data");

--  begin read only
   end Test_Generate_GIC_Node;
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
end DTS.APU_Devices.Test_Data.Tests;
