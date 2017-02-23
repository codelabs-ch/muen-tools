--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Cspec.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_674d69 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/674d6939a65f67a4/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  cspec.ads:27:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dir : constant String := "obj/outdir";
      P   : constant String := "_component";
   begin
      Component:
      declare
         C : constant String := "vt";
      begin
         Run (Input_Spec       => "data/component_vt.xml",
              Output_Directory => Dir,
              Include_Path     => "");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (1)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".adb",
                  Filename2 => "data/" & C & P & ".adb"),
                 Message   => C & P & ".adb mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory.ads",
                  Filename2 => "data/" & C & P & "-memory.ads"),
                 Message   => C & P & "-memory.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-channels.ads",
                  Filename2 => "data/" & C & P & "-channels.ads"),
                 Message   => C & P & "-channels.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-devices.ads",
                  Filename2 => "data/" & C & P & "-devices.ads"),
                 Message   => C & P & "-devices.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory_arrays.ads",
                  Filename2 => "data/" & C & P & "-memory_arrays.ads"),
                 Message   => C & P & "-memory_arrays.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-channel_arrays.ads",
                  Filename2 => "data/" & C & P & "-channel_arrays.ads"),
                 Message   => C & P & "-channel_arrays.ads mismatch");
      end Component;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Library:
      declare
         C : constant String := "libdebug";
      begin
         Run (Input_Spec       => "data/library_debug.xml",
              Output_Directory => Dir,
              Include_Path     => "");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (2)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory.ads",
                  Filename2 => "data/" & C & P & "-memory.ads"),
                 Message   => C & P & "-memory.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-devices.ads",
                  Filename2 => "data/" & C & P & "-devices.ads"),
                 Message   => C & P & "-devices.ads mismatch");
      end Library;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Includes:
      declare
         C : constant String := "inc";
      begin
         Run (Input_Spec       => "data/component_inc.xml",
              Output_Spec      => Dir & "/outspec.xml",
              Output_Directory => Dir,
              Include_Path     => "data/incdir");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (3)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/outspec.xml",
                  Filename2 => "data/outspec_inc.xml"),
                 Message   => "Output spec mismatch (1)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory.ads",
                  Filename2 => "data/" & C & P & "-memory.ads"),
                 Message   => C & P & "-memory.ads mismatch");
      end Includes;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Conditionals:
      declare
         C : constant String := "cond";
      begin
         Run (Input_Spec       => "data/component_cond.xml",
              Output_Spec      => Dir & "/outspec.xml",
              Output_Directory => Dir,
              Include_Path     => "");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (4)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/outspec.xml",
                  Filename2 => "data/outspec_cond.xml"),
                 Message   => "Output spec mismatch (2)");
      end Conditionals;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Substitutions :
      declare
         C : constant String := "subst";
      begin
         Run (Input_Spec       => "data/component_subst.xml",
              Output_Spec      => Dir & "/outspec.xml",
              Output_Directory => Dir,
              Include_Path     => "");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (5)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/outspec.xml",
                  Filename2 => "data/outspec_subst.xml"),
                 Message   => "Output spec mismatch (3)");
      end Substitutions;

      Ada.Directories.Delete_Tree (Directory => Dir);

      --  No resources found.

      Run (Input_Spec       => "data/component_nores.xml",
           Output_Directory => "obj",
           Include_Path     => "");
      Assert (Condition => not Ada.Directories.Exists (Name => Dir),
              Message   => "Out directory created");
--  begin read only
   end Test_Run;
--  end read only

end Cspec.Test_Data.Tests;
