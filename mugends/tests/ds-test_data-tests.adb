--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DS.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body DS.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test);
   procedure Test_Less_Than_5adc94 (Gnattest_T : in out Test) renames Test_Less_Than;
--  id:2.2/5adc941da2821314/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_A : Loadable_File
         := (Address => 16#0010_0000#,
             others  => <>);
      File_B : Loadable_File
         := (Address => 16#0020_0000#,
             others  => <>);
   begin
      Assert (Condition => File_A < File_B,
              Message   => "loadable file ordering incorrect (0x100000 < 0x200000)");

--  begin read only
   end Test_Less_Than;
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
end DS.Test_Data.Tests;
