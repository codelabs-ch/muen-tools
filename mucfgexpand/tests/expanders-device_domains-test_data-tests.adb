--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Device_Domains.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Device_Domains.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test);
   procedure Test_Add_Section_Skeleton_797fa9 (Gnattest_T : in out Test) renames Test_Add_Section_Skeleton;
--  id:2.2/797fa93bd19d8580/Add_Section_Skeleton/1/0/
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:23:4:Add_Section_Skeleton
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/device_domains_skeleton.xml",
         Ref_Filename => "data/device_domains_skeleton.ref.xml",
         Expander     => Add_Section_Skeleton'Access);
--  begin read only
   end Test_Add_Section_Skeleton;
--  end read only

end Expanders.Device_Domains.Test_Data.Tests;
