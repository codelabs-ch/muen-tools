--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.Test_Data.

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
package body DTS.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_DTS_Cell (Gnattest_T : in out Test);
   procedure Test_To_DTS_Cell_7c3700 (Gnattest_T : in out Test) renames Test_To_DTS_Cell;
--  id:2.2/7c37003497272501/To_DTS_Cell/1/0/
   procedure Test_To_DTS_Cell (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (Actual   => To_DTS_Cell (Value => 16#0000_0000_0000_0000#),
              Expected => "0x00000000 0x00000000",
              Message  => "wrong dts cell conversion for zero value");

      Assert (Actual   => To_DTS_Cell (Value => 16#0000_0000_5f00_4000#),
              Expected => "0x00000000 0x5f004000",
              Message  => "wrong dts cell conversion for 32-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#2f00_0020_0000_0000#),
              Expected => "0x2f000020 0x00000000",
              Message  => "wrong dts cell conversion for high 64-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#03d0_e020_1201_f000#),
              Expected => "0x03d0e020 0x1201f000",
              Message  => "wrong dts cell conversion for full 64-bit value");

      Assert (Actual   => To_DTS_Cell (Value => 16#ffff_ffff_ffff_ffff#),
              Expected => "0xffffffff 0xffffffff",
              Message  => "wrong dts cell conversion for last value");

--  begin read only
   end Test_To_DTS_Cell;
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
end DTS.Test_Data.Tests;
