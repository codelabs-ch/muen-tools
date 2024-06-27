--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.Root.Test_Data.

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

with Test_Utils;

--  begin read only
--  end read only
package body DTS.Root.Test_Data.Tests is

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
   procedure Test_Write_194de8 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/194de815d188fd68/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

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

      --  (3) write device tree sources
      Write (Policy       => Policy,
             Subject      => DOM.Core.Nodes.Item
               (List  => Subject,
                Index => 0),
             Subject_Name => "linux",
             Filename     => "obj/devicetree_linux.dts");

      --  (4) test reference file
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/dts_light_linux.ref",
               Filename2 => "obj/devicetree_linux.dts"),
              Message   => "DTS mismatch for Linux (light test policy)");

--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Add_Aliases_Node (Gnattest_T : in out Test);
   procedure Test_Add_Aliases_Node_e37c23 (Gnattest_T : in out Test) renames Test_Add_Aliases_Node;
--  id:2.2/e37c23a946ad81a6/Add_Aliases_Node/1/0/
   procedure Test_Add_Aliases_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "    aliases {" & ASCII.LF &
        "        serial = &serial_0;" & ASCII.LF & "    };";

      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content =>
             "    aliases {"  & ASCII.LF &
             "        serial = &__serial_alias__;"  & ASCII.LF &
             "    };");

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

      --  (3) test the aliases node entry according to template
      Add_Aliases_Node (Template  => Template,
                        Policy    => Policy,
                        Subject   => DOM.Core.Nodes.Item
                          (List  => Subject,
                           Index => 0));

      Assert (Actual   => Mutools.Templates.To_String (Template => Template),
              Expected => Expected_Entry,
              Message  => "wrong root entry for aliases node test data");

--  begin read only
   end Test_Add_Aliases_Node;
--  end read only


--  begin read only
   procedure Test_Add_Chosen_Node (Gnattest_T : in out Test);
   procedure Test_Add_Chosen_Node_fbd7ca (Gnattest_T : in out Test) renames Test_Add_Chosen_Node;
--  id:2.2/fbd7ca0f0d5263f7/Add_Chosen_Node/1/0/
   procedure Test_Add_Chosen_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "    chosen {" & ASCII.LF &
        "        bootargs = ""hostname=lnx1 console=ttyPS0,115200 " &
        "root=/dev/ram0 earlycon initrd=0x1b000000,0x5000000"";" & ASCII.LF &
        "        stdout-path = ""serial:115200n8"";" & ASCII.LF &
        "    };";

      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content =>
             "    chosen {" & ASCII.LF &
             "        bootargs = ""__chosen_bootparams__"";" & ASCII.LF &
             "        stdout-path = ""serial:115200n8"";" & ASCII.LF &
             "    };");

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

      --  (3) test the chosen node entry according to template
      Add_Chosen_Node (Template  => Template,
                       Policy    => Policy,
                       Subject   => DOM.Core.Nodes.Item
                         (List  => Subject,
                          Index => 0));

      Assert (Actual   => Mutools.Templates.To_String (Template => Template),
              Expected => Expected_Entry,
              Message  => "wrong root entry for chosen node test data");

--  begin read only
   end Test_Add_Chosen_Node;
--  end read only


--  begin read only
   procedure Test_Add_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Node_090547 (Gnattest_T : in out Test) renames Test_Add_Memory_Node;
--  id:2.2/090547ccb151f6b1/Add_Memory_Node/1/0/
   procedure Test_Add_Memory_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Expected_Entry : constant String
        := "    memory@0 {" & ASCII.LF &
        "        device_type = ""memory"";" & ASCII.LF &
        "        reg = <0x00000000 0x00000000 0x00000000 0x00040000" &
        ASCII.LF &
        "               0x00000000 0x00080000 0x00000000 0x006dd000" &
        ASCII.LF &
        "               0x00000000 0x0075d000 0x00000000 0x1a8a3000" &
        ASCII.LF &
        "               0x00000000 0x1b000000 0x00000000 0x05000000>;" &
        ASCII.LF & "    };";

      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content =>
             "    memory@__memory_base__ {" & ASCII.LF &
             "        device_type = ""memory"";" & ASCII.LF &
             "        __memory_registers__" & ASCII.LF & "    };");

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

      --  (3) test the memory node entry according to template
      Add_Memory_Node (Template  => Template,
                       Policy    => Policy,
                       Subject   => DOM.Core.Nodes.Item
                         (List  => Subject,
                          Index => 0));

      Assert (Actual   => Mutools.Templates.To_String (Template => Template),
              Expected => Expected_Entry,
              Message  => "wrong root entry for memory node test data");

--  begin read only
   end Test_Add_Memory_Node;
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
end DTS.Root.Test_Data.Tests;
