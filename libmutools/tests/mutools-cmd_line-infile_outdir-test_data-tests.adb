--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Cmd_Line.Infile_Outdir.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.Cmd_Line.Infile_Outdir.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outdir.ads:28:4:Init
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
           := (1 => new String'("-o"),
               2 => new String'("obj"));
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
           := (1 => new String'("-o"),
               2 => new String'("obj"),
               3 => new String'("data/test_policy.xml"));
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

         Assert (Condition => Policy = "data/test_policy.xml",
                 Message   => "Policy mismatch");
         Assert (Condition => Output_Dir = "obj",
                 Message   => "Outdir mismatch");
      end Positive_Test;
   begin
      Invalid_Switch;
      Null_Argument;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_ae009b (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/ae009b814d7f9e88/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outdir.ads:33:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Counter : Positive                  := 1;
      Outdir  : constant Unbounded_String := To_Unbounded_String ("obj");

      --  Test process procedure.
      procedure Test_Process
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type);

      ----------------------------------------------------------------------

      procedure Test_Process
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
      begin
         Assert (Condition => Output_Dir = Outdir,
                 Message   => "Outdir mismatch");
         Counter := Counter + 1;
      end Test_Process;
   begin
      Policy     := To_Unbounded_String ("data/test_policy.xml");
      Output_Dir := Outdir;

      Run (Kind    => Muxml.Format_Src,
           Process => Test_Process'Access);
      Assert (Condition => Counter = 2,
              Message   => "Counter not 2");
--  begin read only
   end Test_Run;
--  end read only

end Mutools.Cmd_Line.Infile_Outdir.Test_Data.Tests;
