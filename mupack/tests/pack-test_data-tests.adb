--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Pack.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack.ads:26:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Execute_Run
      is
      begin
         Run (Policy_File => "data/execute_run.xml",
              Input_Dir   => "data",
              Output_Dir  => "obj");

         Assert (Condition => Ada.Directories.Exists (Name => "obj/muen.img"),
                 Message   => "System image not found");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "obj/muen.img",
                  Filename2 => "data/execute_run.img"),
                 Message   => "Image file differs");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "obj/muen.img.manifest",
                  Filename2 => "data/execute_run.manifest"),
                 Message   => "Manifest file differs");

         Ada.Directories.Delete_File (Name => "obj/muen.img");
         Ada.Directories.Delete_File (Name => "obj/muen.img.manifest");
      end Execute_Run;

      ----------------------------------------------------------------------

      procedure Execute_Run_No_Content
      is
      begin
         Run (Policy_File => "data/test_policy.xml",
              Input_Dir   => "data",
              Output_Dir  => "obj");

      exception
         when E : Pack_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Image size is zero, no content to pack",
                    Message   => "Exception mismatch");
      end Execute_Run_No_Content;
   begin
      Execute_Run;
      Execute_Run_No_Content;
--  begin read only
   end Test_Run;
--  end read only

end Pack.Test_Data.Tests;
