--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DS.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Directories;
with Ada.Exceptions;

with Test_Utils;

--  begin read only
--  end read only
package body DS.Generator.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      --  (1) parse test policy
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_full.xml");

      --  (2) write deploy scripts to obj directory
      Write (Output_Dir => "obj",
             Policy     => Policy);

      --  (3) test reference files
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootgen_config_full.ref",
               Filename2 => "obj/bootgen.config"),
              Message   => "bootgen.config mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootscript_cmd_full.ref",
               Filename2 => "obj/bootscript.cmd"),
              Message   => "bootscript.cmd mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/gdbinit_config_full.ref",
               Filename2 => "obj/gdbinit.config"),
              Message   => "gdbinit.config mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/xsct_cmd_full.ref",
               Filename2 => "obj/xsct.cmd"),
              Message   => "xsct.cmd mismatch");

      --  (4) text extracted/padded reference files
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_text.part.ref",
               Filename2 => "obj/kernel.bin-kernel_text.part"),
              Message   => "obj/kernel.bin-kernel_text.part mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_ro.part.ref",
               Filename2 => "obj/kernel.bin-kernel_ro.part"),
              Message   => "obj/kernel.bin-kernel_ro.part mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_data_0.part.ref",
               Filename2 => "obj/kernel.bin-kernel_data_0.part"),
              Message   => "obj/kernel.bin-kernel_data_0.part mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_global_data.part.ref",
               Filename2 => "obj/kernel.bin-kernel_global_data.part"),
              Message   => "obj/kernel.bin-kernel_global_data.part mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_global_rodata.part.ref",
               Filename2 => "obj/kernel.bin-kernel_global_rodata.part"),
              Message   => "obj/kernel.bin-kernel_global_rodata.part mismatch");

      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-core-0.pt-kernel_0_pt.pad"),
              Message   => "obj/xilinxzcu104-core-0.pt should not get padded");
      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-core-1.pt-kernel_1_pt.pad"),
              Message   => "obj/xilinxzcu104-core-1.pt should not get padded");
      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-lnx1.pt-lnx1_pt.pad"),
              Message   => "obj/xilinxzcu104-lnx1.pt should not get padded");
      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-string_reverser.pt-string_reverser_pt.pad"),
              Message   => "obj/xilinxzcu104-string_reverser.pt should not get padded");
      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-lnx2.pt-lnx2_pt.pad"),
              Message   => "obj/xilinxzcu104-lnx2.pt should not get padded");
      Assert (Condition => not Ada.Directories.Exists
              (Name => "obj/xilinxzcu104-caesar_cipher.pt-caesar_cipher_pt.pad"),
              Message   => "obj/xilinxzcu104-caesar_cipher.pt should not get padded");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/subject_one.bin-string_reverser_binary.pad.ref",
               Filename2 => "obj/subject_one.bin-string_reverser_binary.pad"),
              Message   => "obj/subject_one.bin-string_reverser_binary.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/devicetree_linux_one.dtb-lnx1_dtb.pad.ref",
               Filename2 => "obj/devicetree_linux_one.dtb-lnx1_dtb.pad"),
              Message   => "obj/devicetree_linux_one.dtb-lnx1_dtb.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/Image_linux.bin-lnx1_binary.pad.ref",
               Filename2 => "obj/Image_linux.bin-lnx1_binary.pad"),
              Message   => "obj/Image_linux.bin-lnx1_binary.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/rootfs_linux_one.cpio.gz-lnx1_rootfs.pad.ref",
               Filename2 => "obj/rootfs_linux_one.cpio.gz-lnx1_rootfs.pad"),
              Message   => "obj/rootfs_linux_one.cpio.gz-lnx1_rootfs.pad mismatch");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/subject_two.bin-caesar_cipher_binary.pad.ref",
               Filename2 => "obj/subject_two.bin-caesar_cipher_binary.pad"),
              Message   => "obj/subject_two.bin-caesar_cipher_binary.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/devicetree_linux_two.dtb-lnx2_dtb.pad.ref",
               Filename2 => "obj/devicetree_linux_two.dtb-lnx2_dtb.pad"),
              Message   => "obj/devicetree_linux_two.dtb-lnx2_dtb.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/Image_linux.bin-lnx2_binary.pad.ref",
               Filename2 => "obj/Image_linux.bin-lnx2_binary.pad"),
              Message   => "obj/Image_linux.bin-lnx2_binary.pad mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/rootfs_linux_two.cpio.gz-lnx2_rootfs.pad.ref",
               Filename2 => "obj/rootfs_linux_two.cpio.gz-lnx2_rootfs.pad"),
              Message   => "obj/rootfs_linux_two.cpio.gz-lnx2_rootfs.pad mismatch");

      --  (5) test fill reference files
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel_bss_0.fill.ref",
               Filename2 => "obj/kernel_bss_0.fill"),
              Message   => "kernel_bss_0.fill mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel_interrupt_stack_0.fill.ref",
               Filename2 => "obj/kernel_interrupt_stack_0.fill"),
              Message   => "kernel_interrupt_stack_0.fill mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel_stack_0.fill.ref",
               Filename2 => "obj/kernel_stack_0.fill"),
              Message   => "kernel_stack_0.fill mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel_stack_1.fill.ref",
               Filename2 => "obj/kernel_stack_1.fill"),
              Message   => "kernel_stack_1.fill mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/lnx1_lowmem.fill.ref",
               Filename2 => "obj/lnx1_lowmem.fill"),
              Message   => "lnx1_lowmem.fill mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/lnx2_lowmem.fill.ref",
               Filename2 => "obj/lnx2_lowmem.fill"),
              Message   => "lnx2_lowmem.fill mismatch");

      --  Test padding of file part extracted with offset
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_offset_padding.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootgen_config_offset_padding.ref",
               Filename2 => "obj/bootgen.config"),
              Message   => "bootgen.config mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bootscript_cmd_offset_padding.ref",
               Filename2 => "obj/bootscript.cmd"),
              Message   => "bootscript.cmd mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/gdbinit_config_offset_padding.ref",
               Filename2 => "obj/gdbinit.config"),
              Message   => "gdbinit.config mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/xsct_cmd_offset_padding.ref",
               Filename2 => "obj/xsct.cmd"),
              Message   => "xsct.cmd mismatch");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/kernel.bin-kernel_stack.part.pad.ref",
               Filename2 => "obj/kernel.bin-kernel_stack.part.pad"),
              Message   => "obj/kernel.bin-kernel_stack.part.pad mismatch");

      --  Test error case: Offset larger than file
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_offset_too_high.xml");

      begin
         Write (Output_Dir => "obj",
                Policy     => Policy);
         Assert (Condition => False,
                 Message   => "Offset larger than file unexpectedly accepted");
      exception
         when E : Generator_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                     "Offset into file 'obj/kernel.bin' referenced by physical "
                     & "memory region 'kernel_global_rodata' is larger than "
                     & "file size: 16#000c_0000# > 16#000b_0000#",
                    Message   => "Unexpected error message '"
                     & Ada.Exceptions.Exception_Message (X => E) & "' if "
                     & "offset is larger than file");
      end;

      --  Test error case: File larger than memory region
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_file_too_big.xml");

      begin
         Write (Output_Dir => "obj",
                Policy     => Policy);
         Assert (Condition => False,
                 Message   => "File that's too large unexpectedly accepted");
      exception
         when E : Generator_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                     "File 'obj/rootfs_linux_one.cpio.gz' is too large for "
                     & "physical memory region 'lnx1|rootfs': 16#01e1_043e# > "
                     & "16#0100_0000#",
                    Message   => "Unexpected error message '"
                     & Ada.Exceptions.Exception_Message (X => E) & "' if "
                     & "file is larger than memory region");
      end;

--  begin read only
   end Test_Write;
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
end DS.Generator.Test_Data.Tests;
