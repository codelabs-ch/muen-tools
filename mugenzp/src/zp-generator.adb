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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with System;

with Interfaces.C;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Files;
with Mutools.Utils;
with Muxml.Utils;

with bootparam_h;

with Zp.Utils;

package body Zp.Generator
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure C_Memset
     (S : System.Address;
      C : Interfaces.C.int;
      N : Interfaces.C.size_t);
   pragma Import (C, C_Memset, "memset");

   --  Write Linux bootparams structure with ramdisk information and specified
   --  command line to file given by filename. The size of the generated file
   --  is 4k + length (cmdl). The physical address argument designates the
   --  physical address of the zero-page in guest memory. The subject memory
   --  nodes describe the virtual address space and are used to generate the
   --  e820 map.
   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Physical_Address : Interfaces.Unsigned_64;
      Ramdisk_Address  : Interfaces.Unsigned_64;
      Ramdisk_Size     : Interfaces.Unsigned_64;
      Subject_Memory   : DOM.Core.Node_List);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Zps      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_zeropage']/file");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Zps)'Img
                 & " zero-page file(s)");

      for I in 1 .. DOM.Core.Nodes.Length (List => Zps) loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Zps,
                                      Index => I - 1);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
            Memname  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "name");
            Subj_Name : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Memname);
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subjects,
                 Ref_Attr  => "name",
                 Ref_Value => Subj_Name);
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Physaddr : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "memory/memory[@physical='" & Memname & "']",
                 Name  => "virtualAddress");
            Bootparams : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Subj_Node,
                 XPath => "bootparams");
            Initramfs_Name : constant String := Subj_Name & "|initramfs";
            Initramfs_Addr_Str : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "memory/memory[@physical='" & Initramfs_Name & "']",
                 Name  => "virtualAddress");

            Initramfs_Address, Initramfs_Size : Interfaces.Unsigned_64 := 0;
         begin
            Mulog.Log (Msg => "Guest-physical address of '" & Memname
                       & "' zero-page is " & Physaddr);

            if Initramfs_Addr_Str'Length > 0 then
               Initramfs_Address := Interfaces.Unsigned_64'Value
                 (Initramfs_Addr_Str);
               Initramfs_Size := Interfaces.Unsigned_64'Value
                 (Muxml.Utils.Get_Attribute
                    (Nodes     => Phys_Mem,
                     Refs      => ((Name  => U ("type"),
                                    Value => U ("subject_initrd")),
                                   (Name  => U ("name"),
                                    Value => U (Initramfs_Name))),
                     Attr_Name => "size"));
               Mulog.Log (Msg => "Declaring ramdisk of size "
                          & Mutools.Utils.To_Hex (Number => Initramfs_Size)
                          & " at address " & Initramfs_Addr_Str
                          & " in zero-page");
            end if;

            Write_ZP_File
              (Filename         => Output_Dir & "/" & Filename,
               Cmdline          => Bootparams,
               Physical_Address => Interfaces.Unsigned_64'Value (Physaddr),
               Ramdisk_Address  => Initramfs_Address,
               Ramdisk_Size     => Initramfs_Size,
               Subject_Memory   => Subj_Memory);
         end;
      end loop;

   end Write;

   -------------------------------------------------------------------------

   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Physical_Address : Interfaces.Unsigned_64;
      Ramdisk_Address  : Interfaces.Unsigned_64;
      Ramdisk_Size     : Interfaces.Unsigned_64;
      Subject_Memory   : DOM.Core.Node_List)
   is
      use Ada.Streams.Stream_IO;
      use type Interfaces.C.size_t;
      use type Interfaces.C.unsigned;

      File   : Ada.Streams.Stream_IO.File_Type;
      Params : bootparam_h.boot_params;
   begin
      Mulog.Log (Msg   => "Generating '" & Filename & "' with command line '"
                 & Cmdline & "'");

      C_Memset (S => Params'Address,
                C => 0,
                N => bootparam_h.boot_params'Object_Size / 8);

      Params.hdr.type_of_loader := 16#ff#;

      Params.e820_entries := Interfaces.C.unsigned_char
        (DOM.Core.Nodes.Length (List => Subject_Memory));
      Params.e820_map := Utils.Create_e820_Map (Memory => Subject_Memory);

      --  Initramfs

      Params.hdr.ramdisk_image := Interfaces.C.unsigned (Ramdisk_Address);
      Params.hdr.ramdisk_size  := Interfaces.C.unsigned (Ramdisk_Size);

      Params.hdr.cmdline_size     := 16#0000_0fff#;
      Params.hdr.kernel_alignment := 16#0100_0000#;
      Params.hdr.cmd_line_ptr     := Interfaces.C.unsigned
        (Physical_Address) + 16#1000#;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      bootparam_h.boot_params'Write (Stream (File => File), Params);

      --  Write command line

      String'Write (Stream (File => File), Cmdline);
      Character'Write (Stream (File => File), Character'Val (0));

      Close (File => File);
   end Write_ZP_File;

end Zp.Generator;
