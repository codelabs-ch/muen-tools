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
         Pre          => Subjects.Add_Device_BDFs'Access,
         Expander     => Add_PCI_Config_Space'Access);
--  begin read only
   end Test_Add_PCI_Config_Space;
--  end read only


--  begin read only
   procedure Test_Add_IOMMU_Default_Caps (Gnattest_T : in out Test);
   procedure Test_Add_IOMMU_Default_Caps_44ac7d (Gnattest_T : in out Test) renames Test_Add_IOMMU_Default_Caps;
--  id:2.2/44ac7dcb3bcef9b3/Add_IOMMU_Default_Caps/1/0/
   procedure Test_Add_IOMMU_Default_Caps (Gnattest_T : in out Test) is
   --  expanders-platform.ads:28:4:Add_IOMMU_Default_Caps
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_iommu_caps.xml",
         Ref_Filename => "data/platform_iommu_caps.ref.xml",
         Expander     => Add_IOMMU_Default_Caps'Access);
--  begin read only
   end Test_Add_IOMMU_Default_Caps;
--  end read only


--  begin read only
   procedure Test_Add_Reserved_Memory_Blocks (Gnattest_T : in out Test);
   procedure Test_Add_Reserved_Memory_Blocks_ea03f6 (Gnattest_T : in out Test) renames Test_Add_Reserved_Memory_Blocks;
--  id:2.2/ea03f6c6ec01e163/Add_Reserved_Memory_Blocks/1/0/
   procedure Test_Add_Reserved_Memory_Blocks (Gnattest_T : in out Test) is
   --  expanders-platform.ads:31:4:Add_Reserved_Memory_Blocks
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_reserved_memory_blocks.xml",
         Ref_Filename => "data/platform_reserved_memory_blocks.ref.xml",
         Expander     => Add_Reserved_Memory_Blocks'Access);
--  begin read only
   end Test_Add_Reserved_Memory_Blocks;
--  end read only


--  begin read only
   procedure Test_Remove_Reserved_Mem_Regions (Gnattest_T : in out Test);
   procedure Test_Remove_Reserved_Mem_Regions_30c1ec (Gnattest_T : in out Test) renames Test_Remove_Reserved_Mem_Regions;
--  id:2.2/30c1ec4a7af39fe3/Remove_Reserved_Mem_Regions/1/0/
   procedure Test_Remove_Reserved_Mem_Regions (Gnattest_T : in out Test) is
   --  expanders-platform.ads:34:4:Remove_Reserved_Mem_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_reserved_memory.xml",
         Ref_Filename => "data/platform_reserved_memory.ref.xml",
         Expander     => Remove_Reserved_Mem_Regions'Access);
--  begin read only
   end Test_Remove_Reserved_Mem_Regions;
--  end read only


--  begin read only
   procedure Test_Remove_Reserved_Mem_References (Gnattest_T : in out Test);
   procedure Test_Remove_Reserved_Mem_References_6529e6 (Gnattest_T : in out Test) renames Test_Remove_Reserved_Mem_References;
--  id:2.2/6529e6a3b72406a9/Remove_Reserved_Mem_References/1/0/
   procedure Test_Remove_Reserved_Mem_References (Gnattest_T : in out Test) is
   --  expanders-platform.ads:37:4:Remove_Reserved_Mem_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_reserved_memory_references.xml",
         Ref_Filename => "data/platform_reserved_memory_references.ref.xml",
         Expander     => Remove_Reserved_Mem_References'Access);
--  begin read only
   end Test_Remove_Reserved_Mem_References;
--  end read only

end Expanders.Platform.Test_Data.Tests;
