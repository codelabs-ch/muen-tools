--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Cmd_Line.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:27:4:Init
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Strings.Unbounded.Unbounded_String;

      ----------------------------------------------------------------------

      procedure Invalid_Switch
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-x"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when Invalid_Cmd_Line =>
               null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure Null_Argument
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("file"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when Invalid_Cmd_Line =>
               null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-a"),
               2 => new String'("additional_hardware.xml"),
               3 => new String'("-p"),
               4 => new String'("platform.xml"),
               5 => new String'("data/system_config.xml"),
               6 => new String'("merged.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Config_File = "data/system_config.xml",
                 Message   => "Config file mismatch");
         Assert (Condition => Platform_File = "platform.xml",
                 Message   => "Platform file mismatch");
         Assert (Condition => Additional_Hw_File = "additional_hardware.xml",
                 Message   => "Additional hardware file mismatch");
         Assert (Condition => Output_File = "merged.xml",
                 Message   => "Output file  mismatch");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Without_Additional_Hw
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("platform.xml"),
               3 => new String'("data/system_config.xml"),
               4 => new String'("merged.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Config_File = "data/system_config.xml",
                 Message   => "Config file mismatch");
         Assert (Condition => Platform_File = "platform.xml",
                 Message   => "Platform file mismatch");
         Assert (Condition => Additional_Hw_File = "",
                 Message   => "Additional hardware file set");
         Assert (Condition => Output_File = "merged.xml",
                 Message   => "Output file  mismatch");
      end Without_Additional_Hw;
   begin
      Invalid_Switch;
      Null_Argument;
      Without_Additional_Hw;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Config_File (Gnattest_T : in out Test);
   procedure Test_Get_Config_File_86576e (Gnattest_T : in out Test) renames Test_Get_Config_File;
--  id:2.2/86576e84aef15e99/Get_Config_File/1/0/
   procedure Test_Get_Config_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:30:4:Get_Config_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("system_config.xml");
   begin
      Config_File := Ref;
      Assert (Condition => Get_Config_File = Ref,
              Message   => "Config file mismatch");
--  begin read only
   end Test_Get_Config_File;
--  end read only


--  begin read only
   procedure Test_Get_Output_File (Gnattest_T : in out Test);
   procedure Test_Get_Output_File_762f34 (Gnattest_T : in out Test) renames Test_Get_Output_File;
--  id:2.2/762f34e807656cc2/Get_Output_File/1/0/
   procedure Test_Get_Output_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:33:4:Get_Output_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Output_File := Ref;
      Assert (Condition => Get_Output_File = Ref,
              Message   => "Output file mismatch");
--  begin read only
   end Test_Get_Output_File;
--  end read only


--  begin read only
   procedure Test_Get_Platform_File (Gnattest_T : in out Test);
   procedure Test_Get_Platform_File_632c68 (Gnattest_T : in out Test) renames Test_Get_Platform_File;
--  id:2.2/632c686b957c35ab/Get_Platform_File/1/0/
   procedure Test_Get_Platform_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:36:4:Get_Platform_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("platform.xml");
   begin
      Platform_File := Ref;
      Assert (Condition => Get_Platform_File = Ref,
              Message   => "Platform file mismatch");
--  begin read only
   end Test_Get_Platform_File;
--  end read only


--  begin read only
   procedure Test_Get_Additional_Hardware_File (Gnattest_T : in out Test);
   procedure Test_Get_Additional_Hardware_File_eb3075 (Gnattest_T : in out Test) renames Test_Get_Additional_Hardware_File;
--  id:2.2/eb3075ff1cc3ea9b/Get_Additional_Hardware_File/1/0/
   procedure Test_Get_Additional_Hardware_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:39:4:Get_Additional_Hardware_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("additional_hardware.xml");
   begin
      Additional_Hw_File := Ref;
      Assert (Condition => Get_Additional_Hardware_File = Ref,
              Message   => "Additional hardware file mismatch");
--  begin read only
   end Test_Get_Additional_Hardware_File;
--  end read only

end Merge.Cmd_Line.Test_Data.Tests;