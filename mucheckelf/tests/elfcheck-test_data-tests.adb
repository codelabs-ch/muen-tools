--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Elfcheck.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;

with Mutools.Types;
--  begin read only
--  end read only
package body Elfcheck.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_fe87e3 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/fe87e30211bf9f21/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Positive test.

      Run (Policy_File => "data/test_policy-x86_64.xml",
           ELF_Binary  => "data/binary.x86_64");

      --  Unexpected section.

      begin
         Run (Policy_File => "data/test_policy-x86_64.xml",
              ELF_Binary  => "data/unexpected_section");
         Assert (Condition => True,
                 Message   => "Exception expected (1)");

      exception
         when E : ELF_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unexpected ELF section '.rela.dyn'",
                    Message   => "Exception expected (1)");
      end;

      --  Missing section.

      for M of Section_Map (Mutools.Types.X86_64) loop
         M.Present := False;
      end loop;

      begin
         Run (Policy_File => "data/test_policy-x86_64.xml",
              ELF_Binary  => "data/section_missing");
         Assert (Condition => True,
                 Message   => "Exception expected (2)");

      exception
         when E : ELF_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required section '.text' not present",
                    Message   => "Exception mismatch (2)");
      end;

      --  Positive test (arm64).

      Run (Policy_File => "data/test_policy-arm64.xml",
           ELF_Binary  => "data/binary.arm64");
--  begin read only
   end Test_Run;
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
end Elfcheck.Test_Data.Tests;
