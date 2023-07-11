--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.Test_Data.

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

with Node_Utils;
with Mulog;

--  begin read only
--  end read only
package body DTS.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Block_Indent (Gnattest_T : in out Test);
   procedure Test_Block_Indent_4021a6 (Gnattest_T : in out Test) renames Test_Block_Indent;
--  id:2.2/4021a6d7cd8f7dd3/Block_Indent/1/0/
   procedure Test_Block_Indent (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Multiline : constant String :=
        "first line without indent {" & ASCII.LF &
        "    second line has indent = ""4"";" & ASCII.LF &
        "    third line as well <0x0 0x0 0x2000 0x0000>;" & ASCII.LF &
        "" & ASCII.LF &
        "      indent equals 6 for line after empty line" & ASCII.LF &
        "};";

      Block : Unbounded_String;
   begin
      Block := To_Unbounded_String ("");
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected => "",
              Message  => "wrong block indent for empty block with default " &
                "N and unit size indent");

      Block := To_Unbounded_String ("");
      Block_Indent (Block => Block,
                    N     => 2);
      Assert (Actual   => To_String (Block),
              Expected => "",
              Message  => "wrong block indent for empty block with N " &
                "equals 2 and default unit size indent");

      Block := To_Unbounded_String ("");
      Block_Indent (Block     => Block,
                    N         => 3,
                    Unit_Size => 2);
      Assert (Actual   => To_String (Block),
              Expected => "",
              Message  => "wrong block indent for empty block with N " &
                "equals 3 and unit size equals 2 indent");

      Block := To_Unbounded_String ("one line");
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected => "    one line",
              Message  => "wrong block indent for one line block with " &
                "default N and unit size indent");

      Block := To_Unbounded_String ("one line");
      Block_Indent (Block => Block,
                    N     => 2);
      Assert (Actual   => To_String (Block),
              Expected => "        one line",
              Message  => "wrong block indent for one line block with " &
                "N equals 2 and default unit size indent");

      Block := To_Unbounded_String ("one line");
      Block_Indent (Block     => Block,
                    N         => 3,
                    Unit_Size => 3);
      Assert (Actual   => To_String (Block),
              Expected => "         one line",
              Message  => "wrong block indent for one line block with " &
                "N equals 3 and unit size equals 3 indent");

      Block := To_Unbounded_String ("first line" & ASCII.LF & "second line");
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected =>
                "    first line" & ASCII.LF &
                "    second line",
              Message  => "wrong block indent for two line block with " &
                "default N and unit size indent");

      Block := To_Unbounded_String ("first line" & ASCII.LF & "second line");
      Block_Indent (Block => Block,
                    N     => 3);
      Assert (Actual   => To_String (Block),
              Expected =>
                "            first line" & ASCII.LF &
                "            second line",
              Message  => "wrong block indent for two line block with " &
                "N equals 3 and default unit size indent");

      Block := To_Unbounded_String ("first line" & ASCII.LF & "second line");
      Block_Indent (Block     => Block,
                    N         => 5,
                    Unit_Size => 1);
      Assert (Actual   => To_String (Block),
              Expected =>
                "     first line" & ASCII.LF &
                "     second line",
              Message  => "wrong block indent for two line block with " &
                "N equals 5 and unit size equals 1 indent");

      Block := To_Unbounded_String (Multiline);
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected =>
                "    first line without indent {" &
                ASCII.LF &
                "        second line has indent = ""4"";" &
                ASCII.LF &
                "        third line as well <0x0 0x0 0x2000 0x0000>;" &
                ASCII.LF &
                "" &
                ASCII.LF &
                "          indent equals 6 for line after empty line" &
                ASCII.LF &
                "    };",
              Message  => "wrong block indent for multiline block with " &
                "default N and unit size indent");

      Block := To_Unbounded_String (ASCII.LF & Multiline & ASCII.LF);
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected =>
                "" &
                ASCII.LF &
                "    first line without indent {" &
                ASCII.LF &
                "        second line has indent = ""4"";" &
                ASCII.LF &
                "        third line as well <0x0 0x0 0x2000 0x0000>;" &
                ASCII.LF &
                "" &
                ASCII.LF &
                "          indent equals 6 for line after empty line" &
                ASCII.LF &
                "    };" &
                ASCII.LF &
                "",
              Message  => "wrong block indent for multiline block with " &
                "default N and unit size indent and single empty line");

      Block := To_Unbounded_String
        (ASCII.LF & ASCII.LF & Multiline & ASCII.LF & ASCII.LF);
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected =>
                "" &
                ASCII.LF &
                "" &
                ASCII.LF &
                "    first line without indent {" &
                ASCII.LF &
                "        second line has indent = ""4"";" &
                ASCII.LF &
                "        third line as well <0x0 0x0 0x2000 0x0000>;" &
                ASCII.LF &
                "" &
                ASCII.LF &
                "          indent equals 6 for line after empty line" &
                ASCII.LF &
                "    };" &
                ASCII.LF &
                "" &
                ASCII.LF &
                "",
              Message  => "wrong block indent for multiline block with " &
                "default N and unit size indent and multiple empty lines");

      Block := To_Unbounded_String
        ("" & ASCII.LF & "" & ASCII.LF & "" & ASCII.LF & "" & ASCII.LF);
      Block_Indent (Block => Block);
      Assert (Actual   => To_String (Block),
              Expected =>
                "" & ASCII.LF & "" & ASCII.LF & "" & ASCII.LF & "" & ASCII.LF,
              Message  => "wrong block indent for new line only block with " &
                "default N and unit size indent");

--  begin read only
   end Test_Block_Indent;
--  end read only


--  begin read only
   procedure Test_DTS_Register_Entry (Gnattest_T : in out Test);
   procedure Test_DTS_Register_Entry_1a3843 (Gnattest_T : in out Test) renames Test_DTS_Register_Entry;
--  id:2.2/1a38430ae77256c5/DTS_Register_Entry/1/0/
   procedure Test_DTS_Register_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Subject : DOM.Core.Node_List;

      Virtual_GIC_Dev  : DOM.Core.Node_List;
      Virtual_UART_Dev : DOM.Core.Node_List;

      Actual_Entry : Unbounded_String;
      Actual_Range : DTS_Range_Type;
   begin
      --  (1) parse test policy  --
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_light.xml");

      --  (2) extract linux subject directly  --
      Subject := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']");

      --  (3.a) test for a single virtual memory node with UART  --
      Virtual_UART_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => DOM.Core.Nodes.Item (List  => Subject,
                                       Index => 0),
         XPath => "devices/device[@physical='UART1']");

      DTS_Register_Entry (Policy    => Policy,
                          Device    => DOM.Core.Nodes.Item
                            (List  => Virtual_UART_Dev,
                             Index => 0),
                          DTS_Entry => Actual_Entry,
                          DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected =>
                "reg = <0x00000000 0x21010000 0x00000000 0x00001000>;",
              Message  => "wrong register entry for UART test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_2101_0000#,
              Message   => "wrong register range base for UART test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_1000#,
              Message   => "wrong register range base for UART test data");

      --  (3.b) reset actual values  --
      Actual_Entry := To_Unbounded_String ("");
      Actual_Range := (Base =>  16#0000_0000_0000_0000#,
                       Size =>  16#0000_0000_0000_0000#);

      --  (3.c) test for a multi virtual memory node with GIC  --
      Virtual_GIC_Dev := McKae.XML.XPath.XIA.XPath_Query
        (N     => DOM.Core.Nodes.Item (List  => Subject,
                                       Index => 0),
         XPath => "devices/device[@physical='APU_GIC']");

      DTS_Register_Entry (Policy    => Policy,
                          Device    => DOM.Core.Nodes.Item
                            (List  => Virtual_GIC_Dev,
                             Index => 0),
                          DTS_Entry => Actual_Entry,
                          DTS_Range => Actual_Range);

      Assert (Actual   => To_String (Actual_Entry),
              Expected =>
                "reg = <0x00000000 0x20000000 0x00000000 0x00001000>," &
                ASCII.LF & "        " &
                "<0x00000000 0x20001000 0x00000000 0x00001000>;",
              Message  => "wrong register entry for GIC test data");
      Assert (Condition => Actual_Range.Base = 16#0000_0000_2000_0000#,
              Message   => "wrong register range base for GIC test data");
      Assert (Condition => Actual_Range.Size = 16#0000_0000_0000_2000#,
              Message   => "wrong register range base for GIC test data");

--  begin read only
   end Test_DTS_Register_Entry;
--  end read only


--  begin read only
   procedure Test_To_DTS_Cell (Gnattest_T : in out Test);
   procedure Test_To_DTS_Cell_7c3700 (Gnattest_T : in out Test) renames Test_To_DTS_Cell;
--  id:2.2/7c37003497272501/To_DTS_Cell/1/0/
   procedure Test_To_DTS_Cell (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (Actual   => To_DTS_Cell (Value => 16#0000_0000_0000_0000#),
              Expected => "0x00000000 0x00000000",
              Message  => "wrong dts cell conversion for zero value");

      Assert (Actual   => To_DTS_Cell (Value => 16#0000_0000_5f00_4000#),
              Expected => "0x00000000 0x5f004000",
              Message  => "wrong dts cell conversion for 32-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#2f00_0020_0000_0000#),
              Expected => "0x2f000020 0x00000000",
              Message  => "wrong dts cell conversion for high 64-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#03d0_e020_1201_f000#),
              Expected => "0x03d0e020 0x1201f000",
              Message  => "wrong dts cell conversion for full 64-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#ffff_ffff_ffff_ffff#),
              Expected => "0xffffffff 0xffffffff",
              Message  => "wrong dts cell conversion for last value");

--  begin read only
   end Test_To_DTS_Cell;
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
end DTS.Test_Data.Tests;
