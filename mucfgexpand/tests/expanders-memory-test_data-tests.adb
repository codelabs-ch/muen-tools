--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Memory.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Memory.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Kernel_Binary (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_Binary_289448 (Gnattest_T : in out Test) renames Test_Add_Kernel_Binary;
--  id:2.2/2894483fa0385d76/Add_Kernel_Binary/1/0/
   procedure Test_Add_Kernel_Binary (Gnattest_T : in out Test) is
   --  expanders-memory.ads:23:4:Add_Kernel_Binary
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_kernel_binary.xml",
         Ref_Filename => "data/memory_kernel_binary.ref.xml",
         Expander     => Add_Kernel_Binary'Access);
--  begin read only
   end Test_Add_Kernel_Binary;
--  end read only


--  begin read only
   procedure Test_Add_Kernel_PTs (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_PTs_046959 (Gnattest_T : in out Test) renames Test_Add_Kernel_PTs;
--  id:2.2/046959a8b3e4ac50/Add_Kernel_PTs/1/0/
   procedure Test_Add_Kernel_PTs (Gnattest_T : in out Test) is
   --  expanders-memory.ads:26:4:Add_Kernel_PTs
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Policy_Filename => "data/calculate_pt.xml",
         Policy_Format   => Muxml.Format_A,
         Filename        => "obj/memory_kernel_pts.xml",
         Ref_Filename    => "data/memory_kernel_pts.ref.xml",
         Expander        => Add_Kernel_PTs'Access);
--  begin read only
   end Test_Add_Kernel_PTs;
--  end read only


--  begin read only
   procedure Test_Add_Subject_PTs (Gnattest_T : in out Test);
   procedure Test_Add_Subject_PTs_a375d9 (Gnattest_T : in out Test) renames Test_Add_Subject_PTs;
--  id:2.2/a375d92e3b50d142/Add_Subject_PTs/1/0/
   procedure Test_Add_Subject_PTs (Gnattest_T : in out Test) is
   --  expanders-memory.ads:29:4:Add_Subject_PTs
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_subject_pts.xml",
         Ref_Filename => "data/memory_subject_pts.ref.xml",
         Pre          => Add_Missing_Attributes'Access,
         Expander     => Add_Subject_PTs'Access);
--  begin read only
   end Test_Add_Subject_PTs;
--  end read only


--  begin read only
   procedure Test_Add_Stack_Store (Gnattest_T : in out Test);
   procedure Test_Add_Stack_Store_5748e4 (Gnattest_T : in out Test) renames Test_Add_Stack_Store;
--  id:2.2/5748e4b90e252c05/Add_Stack_Store/1/0/
   procedure Test_Add_Stack_Store (Gnattest_T : in out Test) is
   --  expanders-memory.ads:32:4:Add_Stack_Store
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_stack_store.xml",
         Ref_Filename => "data/memory_stack_store.ref.xml",
         Expander     => Add_Stack_Store'Access);
--  begin read only
   end Test_Add_Stack_Store;
--  end read only


--  begin read only
   procedure Test_Add_Subject_States (Gnattest_T : in out Test);
   procedure Test_Add_Subject_States_9c50e6 (Gnattest_T : in out Test) renames Test_Add_Subject_States;
--  id:2.2/9c50e6215c6e4fd4/Add_Subject_States/1/0/
   procedure Test_Add_Subject_States (Gnattest_T : in out Test) is
   --  expanders-memory.ads:35:4:Add_Subject_States
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_subject_states.xml",
         Ref_Filename => "data/memory_subject_states.ref.xml",
         Expander     => Add_Subject_States'Access);
--  begin read only
   end Test_Add_Subject_States;
--  end read only


--  begin read only
   procedure Test_Add_Tau0_Interface (Gnattest_T : in out Test);
   procedure Test_Add_Tau0_Interface_6b67a0 (Gnattest_T : in out Test) renames Test_Add_Tau0_Interface;
--  id:2.2/6b67a0581135c397/Add_Tau0_Interface/1/0/
   procedure Test_Add_Tau0_Interface (Gnattest_T : in out Test) is
   --  expanders-memory.ads:38:4:Add_Tau0_Interface
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_tau0_iface.xml",
         Ref_Filename => "data/memory_tau0_iface.ref.xml",
         Expander     => Add_Tau0_Interface'Access);
--  begin read only
   end Test_Add_Tau0_Interface;
--  end read only


--  begin read only
   procedure Test_Add_AP_Trampoline (Gnattest_T : in out Test);
   procedure Test_Add_AP_Trampoline_2db30b (Gnattest_T : in out Test) renames Test_Add_AP_Trampoline;
--  id:2.2/2db30b099e003c2a/Add_AP_Trampoline/1/0/
   procedure Test_Add_AP_Trampoline (Gnattest_T : in out Test) is
   --  expanders-memory.ads:41:4:Add_AP_Trampoline
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_trampoline.xml",
         Ref_Filename => "data/memory_trampoline.ref.xml",
         Expander     => Add_AP_Trampoline'Access);
--  begin read only
   end Test_Add_AP_Trampoline;
--  end read only


--  begin read only
   procedure Test_Add_VMXON_Regions (Gnattest_T : in out Test);
   procedure Test_Add_VMXON_Regions_0233b6 (Gnattest_T : in out Test) renames Test_Add_VMXON_Regions;
--  id:2.2/0233b623f10e08f6/Add_VMXON_Regions/1/0/
   procedure Test_Add_VMXON_Regions (Gnattest_T : in out Test) is
   --  expanders-memory.ads:44:4:Add_VMXON_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_vmxon.xml",
         Ref_Filename => "data/memory_vmxon.ref.xml",
         Expander     => Add_VMXON_Regions'Access);
--  begin read only
   end Test_Add_VMXON_Regions;
--  end read only


--  begin read only
   procedure Test_Add_VMCS_Regions (Gnattest_T : in out Test);
   procedure Test_Add_VMCS_Regions_7b5dda (Gnattest_T : in out Test) renames Test_Add_VMCS_Regions;
--  id:2.2/7b5dda6237d4ef13/Add_VMCS_Regions/1/0/
   procedure Test_Add_VMCS_Regions (Gnattest_T : in out Test) is
   --  expanders-memory.ads:47:4:Add_VMCS_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_vmcs.xml",
         Ref_Filename => "data/memory_vmcs.ref.xml",
         Expander     => Add_VMCS_Regions'Access);
--  begin read only
   end Test_Add_VMCS_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Missing_Attributes (Gnattest_T : in out Test);
   procedure Test_Add_Missing_Attributes_8ead35 (Gnattest_T : in out Test) renames Test_Add_Missing_Attributes;
--  id:2.2/8ead35ae70aec256/Add_Missing_Attributes/1/0/
   procedure Test_Add_Missing_Attributes (Gnattest_T : in out Test) is
   --  expanders-memory.ads:50:4:Add_Missing_Attributes
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_attributes.xml",
         Ref_Filename => "data/memory_attributes.ref.xml",
         Expander     => Add_Missing_Attributes'Access);
--  begin read only
   end Test_Add_Missing_Attributes;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Bitmaps (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Bitmaps_4f1ecd (Gnattest_T : in out Test) renames Test_Add_Subject_Bitmaps;
--  id:2.2/4f1ecd09d13ab752/Add_Subject_Bitmaps/1/0/
   procedure Test_Add_Subject_Bitmaps (Gnattest_T : in out Test) is
   --  expanders-memory.ads:53:4:Add_Subject_Bitmaps
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_subject_bitmaps.xml",
         Ref_Filename => "data/memory_subject_bitmaps.ref.xml",
         Expander     => Add_Subject_Bitmaps'Access);
--  begin read only
   end Test_Add_Subject_Bitmaps;
--  end read only


--  begin read only
   procedure Test_Add_Subject_MSR_Store (Gnattest_T : in out Test);
   procedure Test_Add_Subject_MSR_Store_187092 (Gnattest_T : in out Test) renames Test_Add_Subject_MSR_Store;
--  id:2.2/187092daa53d49c3/Add_Subject_MSR_Store/1/0/
   procedure Test_Add_Subject_MSR_Store (Gnattest_T : in out Test) is
   --  expanders-memory.ads:56:4:Add_Subject_MSR_Store
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/memory_subject_msrstore.xml",
         Ref_Filename => "data/memory_subject_msrstore.ref.xml",
         Pre          => Subjects.Handle_Profile'Access,
         Expander     => Add_Subject_MSR_Store'Access);
--  begin read only
   end Test_Add_Subject_MSR_Store;
--  end read only

end Expanders.Memory.Test_Data.Tests;
