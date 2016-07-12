--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Cspec.Cmd_Line.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:27:4:Init
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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure Invalid_Parameter
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("system.xml"),
               3 => new String'("-c"));
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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Parameter;

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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-c"),
               2 => new String'("my_cspec"),
               3 => new String'("obj/gen"));
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

         Assert (Condition => Output_Dir = "obj/gen",
                 Message   => "Output dir mismatch");
         Assert (Condition => Cspec_Path = "my_cspec",
                 Message   => "Cspec path mismatch");
      end Positive_Test;
   begin
      Invalid_Switch;
      Invalid_Parameter;
      Null_Argument;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Output_Dir_c3e9c8 (Gnattest_T : in out Test) renames Test_Get_Output_Dir;
--  id:2.2/c3e9c87208c742d1/Get_Output_Dir/1/0/
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:30:4:Get_Output_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Output_Dir := Ref;
      Assert (Condition => Get_Output_Dir = Ref,
              Message   => "Output dir mismatch");
--  begin read only
   end Test_Get_Output_Dir;
--  end read only


--  begin read only
   procedure Test_Get_Component_Spec (Gnattest_T : in out Test);
   procedure Test_Get_Component_Spec_40d06b (Gnattest_T : in out Test) renames Test_Get_Component_Spec;
--  id:2.2/40d06b45c5b08117/Get_Component_Spec/1/0/
   procedure Test_Get_Component_Spec (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:33:4:Get_Component_Spec
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("cspec.xml");
   begin
      Cspec_Path := Ref;
      Assert (Condition => Get_Component_Spec = Ref,
              Message   => "Cspec mismatch");
--  begin read only
   end Test_Get_Component_Spec;
--  end read only

end Cspec.Cmd_Line.Test_Data.Tests;
