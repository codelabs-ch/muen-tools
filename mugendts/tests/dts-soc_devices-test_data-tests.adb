--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.SoC_Devices.Test_Data.

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
package body DTS.SoC_Devices.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_SoC_Devices (Gnattest_T : in out Test);
   procedure Test_Add_SoC_Devices_437c5e (Gnattest_T : in out Test) renames Test_Add_SoC_Devices;
--  id:2.2/437c5ecec591888e/Add_SoC_Devices/1/0/
   procedure Test_Add_SoC_Devices (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "    amba-soc@21010000 {" & ASCII.LF &
        "        compatible = ""simple-bus"";" & ASCII.LF &
        "        #address-cells = <0x2>;" & ASCII.LF &
        "        #size-cells = <0x2>;" & ASCII.LF &
        "        ranges = <0x00000000 0x00000000 0x00000000 " &
        "0x00000000 0x00000000 0x21011000>;" & ASCII.LF & ASCII.LF &
        "        serial_0: uart1@21010000 {" & ASCII.LF &
        "            compatible = ""cdns,uart-r1p12"", ""xlnx,xuartps"";" &
        ASCII.LF &
        "            reg = <0x00000000 0x21010000 0x00000000 0x00001000>;" &
        ASCII.LF &
        "            interrupt-parent = <0x7>;" & ASCII.LF &
        "            interrupts = <GIC_SPI 0x0 IRQ_TYPE_LEVEL_HIGH>;" &
        ASCII.LF &
        "            clocks = <&clk100>, <&clk100>;" & ASCII.LF &
        "            clock-names = ""uart_clk"", ""pclk"";" & ASCII.LF &
        "            status = ""okay"";" & ASCII.LF &
        "        };" & ASCII.LF & ASCII.LF & "    };" & ASCII.LF;

      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content =>
             "    amba-soc@__amba_soc_base__ {" & ASCII.LF &
             "        compatible = ""simple-bus"";" & ASCII.LF &
             "        #address-cells = <0x2>;" & ASCII.LF &
             "        #size-cells = <0x2>;" & ASCII.LF &
             "        __amba_soc_ranges__" & ASCII.LF &
             "__amba_soc_devices__" & ASCII.LF &
             "    };" & ASCII.LF);

      Policy : Muxml.XML_Data_Type;

      Subject : DOM.Core.Node_List;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_light.xml");

      --  (2) extract linux subject directly
      Subject := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']");

      --  (3) test the devices entry according to template
      Add_SoC_Devices (Template  => Template,
                       Policy    => Policy,
                       Subject   => DOM.Core.Nodes.Item
                         (List  => Subject,
                          Index => 0));

      Assert (Actual   => Mutools.Templates.To_String (Template => Template),
              Expected => Expected_Entry,
              Message  => "wrong node entry for SoC devices test data");

--  begin read only
   end Test_Add_SoC_Devices;
--  end read only


--  begin read only
   procedure Test_Generate_Channel_Node (Gnattest_T : in out Test);
   procedure Test_Generate_Channel_Node_d97f7e (Gnattest_T : in out Test) renames Test_Generate_Channel_Node;
--  id:2.2/d97f7e83d27ac2f8/Generate_Channel_Node/1/0/
   procedure Test_Generate_Channel_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "log_channel_reader@31052000 {" & ASCII.LF &
        "    compatible = ""muen,communication-channel"";" & ASCII.LF &
        "    reg = <0x00000000 0x31052000 0x00000000 0x00001000>;" & ASCII.LF &
        "    interrupt-parent = <0x7>;" & ASCII.LF &
        "    interrupts = <GIC_SPI 0x8 IRQ_TYPE_LEVEL_HIGH>;" & ASCII.LF &
        "    type = <READONLY_CHANNEL>;" & ASCII.LF &
        "    status = ""okay"";" & ASCII.LF &
        "};" & ASCII.LF;

      Policy : Muxml.XML_Data_Type;

      Channel_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) extract channel ring logger memory node directly
      Channel_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']/" &
           "memory/memory[@physical='ringlogger_string_reverser']");

      --  (3) test the channel entry
      Generate_Channel_Node (Policy    => Policy,
                             Device    => DOM.Core.Nodes.Item
                               (List  => Channel_Dev,
                                Index => 0),
                             DTS_Entry => Actual_Entry,
                             DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected => Expected_Entry,
              Message  =>
                "wrong channel entry for ring logger test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_3105_2000#,
              Message   =>
                "wrong register range base for ring logger test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_1000#,
              Message   =>
                "wrong register range size for ring logger test data");

--  begin read only
   end Test_Generate_Channel_Node;
--  end read only


--  begin read only
   procedure Test_Generate_NIC_Node (Gnattest_T : in out Test);
   procedure Test_Generate_NIC_Node_c53706 (Gnattest_T : in out Test) renames Test_Generate_NIC_Node;
--  id:2.2/c53706b31664d42e/Generate_NIC_Node/1/0/
   procedure Test_Generate_NIC_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "gem3@31010000 {" & ASCII.LF &
        "    compatible = ""cdns,zynqmp-gem"", ""cdns,gem"";" & ASCII.LF &
        "    reg = <0x00000000 0x31010000 0x00000000 0x00001000>;" & ASCII.LF &
        "    interrupt-parent = <0x7>;" & ASCII.LF &
        "    interrupts = <GIC_SPI 0x2 IRQ_TYPE_LEVEL_HIGH>," & ASCII.LF &
        "        <GIC_SPI 0x2 IRQ_TYPE_LEVEL_HIGH>;" & ASCII.LF &
        "    clocks = <&clk125 &clk125 &clk125 &clk125 &clk125>;" & ASCII.LF &
        "    clock-names = ""pclk"", ""hclk"", ""tx_clk"", ""rx_clk"", ""tsu_clk"";" & ASCII.LF &
        "    status = ""okay"";" & ASCII.LF &
        "" & ASCII.LF &
        "    phy-handle = <0xf>;" & ASCII.LF &
        "    phy-mode = ""rgmii-id"";" & ASCII.LF &
        "    xlnx,ptp-enet-clock = <0x0>;" & ASCII.LF &
        "    local-mac-address = [00 0a 35 00 22 01];" & ASCII.LF & ASCII.LF &
        "    #address-cells = <0x1>;" & ASCII.LF &
        "    #size-cells = <0x0>;" & ASCII.LF & ASCII.LF &
        "    phy@c {" & ASCII.LF &
        "        reg = <0xc>;" & ASCII.LF &
        "        ti,rx-internal-delay = <0x8>;" & ASCII.LF &
        "        ti,tx-internal-delay = <0xa>;" & ASCII.LF &
        "        ti,fifo-depth = <0x1>;" & ASCII.LF &
        "        ti,dp83867-rxctrl-strap-quirk;" & ASCII.LF &
        "        phandle = <0xf>;" & ASCII.LF &
        "    };" & ASCII.LF &
        "};" & ASCII.LF;

      Policy : Muxml.XML_Data_Type;

      NIC_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) extract the GEM ethernet device node directly
      NIC_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='2']/" &
           "devices/device[@physical='GEM3']");

      --  (3) test the GEM ethernet device entry
      Generate_NIC_Node (Policy    => Policy,
                         Device    => DOM.Core.Nodes.Item
                           (List  => NIC_Dev,
                            Index => 0),
                         DTS_Entry => Actual_Entry,
                         DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected => Expected_Entry,
              Message  =>
                "wrong node entry for GEM device test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_3101_0000#,
              Message   =>
                "wrong register range base for GEM device test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_1000#,
              Message   =>
                "wrong register range size for GEM device test data");

--  begin read only
   end Test_Generate_NIC_Node;
--  end read only


--  begin read only
   procedure Test_Generate_UART_Node (Gnattest_T : in out Test);
   procedure Test_Generate_UART_Node_32c347 (Gnattest_T : in out Test) renames Test_Generate_UART_Node;
--  id:2.2/32c347019ba8a651/Generate_UART_Node/1/0/
   procedure Test_Generate_UART_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "serial_0: uart1@31000000 {" & ASCII.LF &
        "    compatible = ""cdns,uart-r1p12"", ""xlnx,xuartps"";" & ASCII.LF &
        "    reg = <0x00000000 0x31000000 0x00000000 0x00001000>;" & ASCII.LF &
        "    interrupt-parent = <0x7>;" & ASCII.LF &
        "    interrupts = <GIC_SPI 0x0 IRQ_TYPE_LEVEL_HIGH>;" & ASCII.LF &
        "    clocks = <&clk100>, <&clk100>;" & ASCII.LF &
        "    clock-names = ""uart_clk"", ""pclk"";" & ASCII.LF &
        "    status = ""okay"";" & ASCII.LF &
        "};" & ASCII.LF;

      Policy : Muxml.XML_Data_Type;

      UART_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) extract UART device node directly
      UART_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='2']/" &
           "devices/device[@physical='UART1']");

      --  (3) test the UART device entry
      Generate_UART_Node (Policy    => Policy,
                          Device    => DOM.Core.Nodes.Item
                            (List  => UART_Dev,
                             Index => 0),
                          DTS_Entry => Actual_Entry,
                          DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected => Expected_Entry,
              Message  =>
                "wrong node entry for UART test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_3100_0000#,
              Message   =>
                "wrong register range base for UART test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_1000#,
              Message   =>
                "wrong register range size for UART test data");

--  begin read only
   end Test_Generate_UART_Node;
--  end read only


--  begin read only
   procedure Test_Generate_USB_Node (Gnattest_T : in out Test);
   procedure Test_Generate_USB_Node_1f48d1 (Gnattest_T : in out Test) renames Test_Generate_USB_Node;
--  id:2.2/1f48d10e3ea3ce8d/Generate_USB_Node/1/0/
   procedure Test_Generate_USB_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "usb3_0_xhci@31001000 {" & ASCII.LF &
        "    #address-cells = <0x2>;" & ASCII.LF &
        "    #size-cells = <0x2>;" & ASCII.LF &
        "    compatible = ""xlnx,zynqmp-dwc3"";" & ASCII.LF &
        "    reg = <0x00000000 0x31001000 0x00000000 0x00001000>;" & ASCII.LF &
        "    status = ""okay"";" & ASCII.LF &
        "    ranges;" & ASCII.LF &
        ASCII.LF &
        "    usb3_0_xhci_dwc3@31010000 {" & ASCII.LF &
        "        compatible = ""snps,dwc3"";" & ASCII.LF &
        "        reg = <0x00000000 0x31010000 0x00000000 0x00040000>;" & ASCII.LF &
        "        interrupt-parent = <0x7>;" & ASCII.LF &
        "        interrupts = <GIC_SPI 0x2 IRQ_TYPE_LEVEL_HIGH>," & ASCII.LF &
        "            <GIC_SPI 0x3 IRQ_TYPE_LEVEL_HIGH>," & ASCII.LF &
        "            <GIC_SPI 0x4 IRQ_TYPE_LEVEL_HIGH>;" & ASCII.LF &
        "        clocks = <&clk250>, <&clk250>;" & ASCII.LF &
        "        clock-names = ""ref"", ""bus_early"";" & ASCII.LF &
        "        snps,quirk-frame-length-adjustment = <0x20>;" & ASCII.LF &
        "        snps,resume-hs-terminations;" & ASCII.LF &
        "        snps,usb2-lpm-disable;" & ASCII.LF &
        "        snps,dis_u3_susphy_quirk;" & ASCII.LF &
        "        snps,dis_u2_susphy_quirk;" & ASCII.LF &
        "        maximum-speed = ""super-speed"";" & ASCII.LF &
        "        dr_mode = ""host"";" & ASCII.LF &
        "        status = ""okay"";" & ASCII.LF &
        "    };" & ASCII.LF &
        "};" & ASCII.LF;

      Policy : Muxml.XML_Data_Type;

      USB_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) extract USB device node directly
      USB_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']/" &
           "devices/device[@physical='USB3_0_XHCI']");

      --  (3) test the USB device entry
      Generate_USB_Node (Policy    => Policy,
                          Device    => DOM.Core.Nodes.Item
                            (List  => USB_Dev,
                             Index => 0),
                          DTS_Entry => Actual_Entry,
                          DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected => Expected_Entry,
              Message  =>
                "wrong node entry for USB test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_3100_1000#,
              Message   =>
                "wrong register range base for USB test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0004_f000#,
              Message   =>
                "wrong register range size for USB test data");

--  begin read only
   end Test_Generate_USB_Node;
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
end DTS.SoC_Devices.Test_Data.Tests;
