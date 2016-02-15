--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.DSDT.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Acpi.DSDT.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_a7b092 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/a7b092122beb7bb7/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  acpi-dsdt.ads:28:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure DSDT_Generation
      is
         Policy : Muxml.XML_Data_Type;
         Fname  : constant String := "linux_dsdt_generation";
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
         begin
            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");

            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/linux_dsdt.dsl.ref",
                     Filename2 => "obj/" & Fname & ".dsl"),
                    Message   => "DSDT table source mismatch");
         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end DSDT_Generation;

      ----------------------------------------------------------------------

      procedure Single_PRT_Entry
      is
         Fname  : constant String := "linux_dsdt_one_serial";
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='ethernet']");
         begin

            --  Remove second IRQ resource.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");

            --  The iasl compilation step must not raise an exception.

         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end Single_PRT_Entry;

      ----------------------------------------------------------------------

      procedure Empty_PRT
      is
         Fname  : constant String := "linux_dsdt_empty_prt";
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='ethernet']");
         begin

            --  Remove all IRQ resources.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");
            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");

            --  The iasl compilation step must not raise an exception.

         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end Empty_PRT;

      ----------------------------------------------------------------------

      procedure Single_Serial_Port
      is
         Fname  : constant String := "linux_dsdt_one_port";
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='serial_1']");
         begin

            --  Remove second I/O port resource.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "ioPort");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");


            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/" & Fname & ".dsl.ref",
                     Filename2 => "obj/" & Fname & ".dsl"),
                    Message   => "DSDT table source mismatch (single port)");
         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end Single_Serial_Port;

      ----------------------------------------------------------------------

      procedure Single_Serial_Device
      is
         Fname  : constant String := "linux_dsdt_one_serial";
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='serial_1']");
         begin

            --  Remove one serial device reference.

            Dev := DOM.Core.Nodes.Remove_Child
              (N         => DOM.Core.Nodes.Parent_Node (N => Dev),
               Old_Child => Dev);

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");


            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/" & Fname & ".dsl.ref",
                     Filename2 => "obj/" & Fname & ".dsl"),
                    Message   => "DSDT table source mismatch (single serial)");
         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end Single_Serial_Device;

      ----------------------------------------------------------------------

      procedure No_Serial_Device
      is
         Fname  : constant String := "linux_dsdt_no_serial";
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='serial_1']");
         begin

            --  Remove all serial device references.

            Dev := DOM.Core.Nodes.Remove_Child
              (N         => DOM.Core.Nodes.Parent_Node (N => Dev),
               Old_Child => Dev);
            Dev := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='serial_2']");
            Dev := DOM.Core.Nodes.Remove_Child
              (N         => DOM.Core.Nodes.Parent_Node (N => Dev),
               Old_Child => Dev);

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/" & Fname & ".dsl");


            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/" & Fname & ".dsl.ref",
                     Filename2 => "obj/" & Fname & ".dsl"),
                    Message   => "DSDT table source mismatch (no serial)");
         end;

         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".dsl");
         Ada.Directories.Delete_File (Name => "obj/" & Fname & ".aml");
      end No_Serial_Device;
   begin
      DSDT_Generation;
      Single_PRT_Entry;
      Empty_PRT;
      Single_Serial_Port;
      Single_Serial_Device;
      No_Serial_Device;
--  begin read only
   end Test_Write;
--  end read only

end Acpi.DSDT.Test_Data.Tests;
