--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Platform.Test_Data.Tests is


--  begin read only
   procedure Test_Add_PCI_Config_Space (Gnattest_T : in out Test);
   procedure Test_Add_PCI_Config_Space_138943 (Gnattest_T : in out Test) renames Test_Add_PCI_Config_Space;
--  id:2.2/1389432568272396/Add_PCI_Config_Space/1/0/
   procedure Test_Add_PCI_Config_Space (Gnattest_T : in out Test) is
   --  expanders-platform.ads:25:4:Add_PCI_Config_Space
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_pci_config_space.xml",
         Ref_Filename => "data/platform_pci_config_space.ref.xml",
         Expander     => Add_PCI_Config_Space'Access);
--  begin read only
   end Test_Add_PCI_Config_Space;
--  end read only

end Expanders.Platform.Test_Data.Tests;
