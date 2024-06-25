--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;

with Mutools.Strings;

with GNAT.OS_Lib;

package body DS.Generator.Test_Data is

   --  Generate a file with a given number of random or zero bytes.
   procedure Generate_File (Filename : String;
                            Size     : Natural;
                            Zero     : Boolean := False)
   is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      subtype Random_Range is Stream_Element range 1 .. Stream_Element'Last;

      package Rand is new Ada.Numerics.Discrete_Random (Random_Range);

      File      : File_Type;
      Generator : Rand.Generator;
      Pid       : constant GNAT.OS_Lib.Process_Id
        := GNAT.OS_Lib.Current_Process_Id;
      Buffer    : Stream_Element_Array (1 .. 2048)
        := (others => 0);
      Written   : Stream_Element_Offset := 0;
      Last      : Stream_Element_Offset;
   begin
      Rand.Reset (Gen       => Generator,
                  Initiator => GNAT.OS_Lib.Pid_To_Integer (Pid => Pid));

      Create (File => File, Mode => Out_File, Name => Filename);

      while Written < Stream_Element_Offset (Size) loop
         if not Zero then
            for I in 1 .. Buffer'Last loop
               Buffer (I) := Rand.Random (Gen => Generator);
            end loop;
         end if;
         Last := Stream_Element_Offset'Min
            (Buffer'Last, Stream_Element_Offset (Size) - Written);
         Write (File => File, Item => Buffer (1 .. Last));
         Written := Written + Last;
      end loop;

      Close (File => File);
   end Generate_File;

   --  Concatenate the given files (paths separated with :).
   procedure Concat_Files (Files_List : String;
                           Target     : String)
   is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use Ada.Strings.Unbounded;

      Files     : constant Mutools.Strings.String_Array
         := Mutools.Strings.Tokenize (Files_List);

      Source_FD : File_Type;
      Target_FD : File_Type;
      Buffer    : Stream_Element_Array (1 .. 2048);
      Last      : Stream_Element_Offset;
   begin
      Create (File => Target_FD, Mode => Out_File, Name => Target);

      for File of Files loop
         Open (File => Source_FD, Mode => In_File, Name => To_String (File));
         loop
            Read (File => Source_FD, Item => Buffer, Last => Last);
            Write (File => Target_FD, Item => Buffer (1 .. Last));
            exit when Last < Buffer'Last;
         end loop;
         Close (File => Source_FD);
      end loop;
      Close (File => Target_FD);
   end Concat_Files;

   --  Create a padded reference file from the given file.
   procedure Pad_File (Filename  : String;
                       Total     : Natural;
                       Reference : String)
   is
      Size    : Natural := Natural (Ada.Directories.Size (Filename));
      Padding : Natural := Total - Size;
   begin
      Generate_File ("obj/padding.tmp", Padding, True);
      Concat_Files (Filename & ":obj/padding.tmp", Reference);
   end Pad_File;

   --  Create a file that's eventually getting padded, so the reference file
   --  is generated accordingly.
   procedure Generate_Padded (Filename  : String;
                              Size      : Natural;
                              Total     : Natural;
                              Reference : String)
   is
   begin
      Generate_File (Filename, Size);
      Pad_File (Filename, Total, Reference);
   end Generate_Padded;

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      --  Create kernel.bin from its expected parts.
      Generate_File ("obj/kernel.bin-kernel_text.part.ref", 16#0005_8000#);
      Generate_File ("obj/kernel.bin-kernel_ro.part.ref", 16#0001_8000#);
      Generate_File ("obj/kernel.bin-kernel_data_0.part.ref", 16#2000#);
      Generate_File ("obj/kernel_bss_0.fill.ref", 16#2000#, True);
      Generate_File ("obj/kernel.bin-kernel_global_data.part.ref", 16#c000#);
      Generate_File ("obj/kernel.bin-kernel_global_rodata.part.ref", 16#0001_0000#);
      --  This part is not mapped in the regular policy but contained in the binary.
      Generate_File ("obj/kernel.bin-kernel_stack", 16#0002_0000#);
      Concat_Files ("obj/kernel.bin-kernel_text.part.ref:obj/kernel.bin-kernel_ro.part.ref:obj/kernel.bin-kernel_data_0.part.ref:obj/kernel_bss_0.fill.ref:obj/kernel.bin-kernel_global_data.part.ref:obj/kernel.bin-kernel_global_rodata.part.ref:obj/kernel.bin-kernel_stack",
                    "obj/kernel.bin");

      --  Generate a reference file for the last part with padding.
      Pad_File ("obj/kernel.bin-kernel_stack", 16#0003_0000#,
                "obj/kernel.bin-kernel_stack.part.ref");

      --  These are correctly sized, so no padding.
      Generate_File ("obj/xilinxzcu104-core-0.pt", 16#8000#);
      Generate_File ("obj/xilinxzcu104-core-1.pt", 16#8000#);
      Generate_File ("obj/xilinxzcu104-lnx1.pt", 16#6000#);
      Generate_File ("obj/xilinxzcu104-string_reverser.pt", 16#3000#);
      Generate_File ("obj/xilinxzcu104-lnx2.pt", 16#6000#);
      Generate_File ("obj/xilinxzcu104-caesar_cipher.pt", 16#3000#);

      --  These are smaller, so they get padded.
      Generate_Padded ("obj/subject_one.bin",
                       16#0002_0000#, 16#0004_0000#,
                       "obj/subject_one.bin-string_reverser_binary.pad.ref");
      Generate_Padded ("obj/devicetree_linux_one.dtb",
                       16#1048#, 16#0004_0000#,
                       "obj/devicetree_linux_one.dtb-lnx1_dtb.pad.ref");
      Generate_Padded ("obj/Image_linux_one.bin",
                       16#006d_c808#, 16#006d_d000#,
                       "obj/Image_linux_one.bin-lnx1_binary.pad.ref");
      Generate_Padded ("obj/rootfs_linux_one.cpio.gz",
                       16#01e1_043e#, 16#0200_0000#,
                       "obj/rootfs_linux_one.cpio.gz-lnx1_rootfs.pad.ref");

      Generate_Padded ("obj/subject_two.bin",
                       16#0002_0001#, 16#0004_0000#,
                       "obj/subject_two.bin-caesar_cipher_binary.pad.ref");
      Generate_Padded ("obj/devicetree_linux_two.dtb",
                       16#1049#, 16#0004_0000#,
                       "obj/devicetree_linux_two.dtb-lnx2_dtb.pad.ref");
      Generate_Padded ("obj/Image_linux_two.bin",
                       16#006d_c801#, 16#006d_d000#,
                       "obj/Image_linux_two.bin-lnx2_binary.pad.ref");
      Generate_Padded ("obj/rootfs_linux_two.cpio.gz",
                       16#01e1_0431#, 16#0200_0000#,
                       "obj/rootfs_linux_two.cpio.gz-lnx2_rootfs.pad.ref");

      --  Fill patterns.
      Generate_File ("obj/kernel_stack_0.fill.ref", 16#0002_0000#, True);
      Generate_File ("obj/kernel_stack_1.fill.ref", 16#0002_0000#, True);
      Generate_File ("obj/lnx1_lowmem.fill.ref", 16#0004_0000#, True);
      Generate_File ("obj/lnx2_lowmem.fill.ref", 16#0004_0000#, True);
      Generate_File ("obj/kernel_interrupt_stack_0.fill.ref", 16#1000#, True);
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

end DS.Generator.Test_Data;
