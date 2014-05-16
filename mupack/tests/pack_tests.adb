--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Directories;
with Ada.Exceptions;

with Pack.Command_Line.Test;

with Test_Utils;

package body Pack_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Execute_Run
   is
   begin
      Command_Line.Test.Set_Input_Dir (Path => "data");

      Pack.Run (Policy_File => "data/execute_run.xml",
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

   -------------------------------------------------------------------------

   procedure Execute_Run_No_Content
   is
   begin
      Pack.Run (Policy_File => "data/test_policy.xml",
                Input_Dir   => "data",
                Output_Dir  => "obj");

   exception
      when E : Pack_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Image size is zero, no content to pack",
                 Message   => "Exception mismatch");
   end Execute_Run_No_Content;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Pack tests");
      T.Add_Test_Routine
        (Routine => Execute_Run'Access,
         Name    => "Run packaging process");
      T.Add_Test_Routine
        (Routine => Execute_Run_No_Content'Access,
         Name    => "Run packaging with no content");
   end Initialize;

end Pack_Tests;
