--
--  Copyright (C) 2023-2024  Tobias Brunner <tobias@codelabs.ch>
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Interfaces;

with Mulog;
with Muxml.Utils;
with Mutools.Types;
with Mutools.Utils;

with DS.BIF;
with DS.GDB;
with DS.UBOOT;
with DS.XSCT;

package body DS.Generator
is

   use Ada.Strings.Unbounded;

   package File_Map_Package is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Loadable_File,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package String_Sets_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String);

   --  Generate a file that contains the pattern repeated size times.
   procedure Create_Fill_File
      (Filename : String;
       Size     : Interfaces.Unsigned_64;
       Pattern  : Interfaces.Unsigned_8);

   --  Extract a part of a given file and/or pad it to a specific size.
   procedure Extract_And_Pad
      (Source : String;
       Target : String;
       Offset : Interfaces.Unsigned_64;
       Size   : Interfaces.Unsigned_64);

   --  Register the files referenced by the given memory and page table nodes.
   procedure Register_Files
      (CPU                :        Natural;
       PT                 :        Unbounded_String;
       Nodes              :        DOM.Core.Node_List;
       File_Backed_Memory : in out File_Map_Package.Map;
       Used_Memory        : in out String_Sets_Package.Set;
       Kernels            : in out CPU_Kernel_Map_Package.Map);

   ------------------------------------------------------------------------

   procedure Create_Fill_File
      (Filename : String;
       Size     : Interfaces.Unsigned_64;
       Pattern  : Interfaces.Unsigned_8)
   is
      type Fill_Type is array (1 .. Size) of Interfaces.Unsigned_8;
      package SIO is new Ada.Sequential_IO
         (Element_Type => Fill_Type);

      Fill : constant Fill_Type
         := (others => Pattern);
      FD   : SIO.File_Type;
   begin
      SIO.Create (File => FD,
                  Name => Filename);
      SIO.Write (File => FD,
                 Item => Fill);
      SIO.Close (File => FD);
   end Create_Fill_File;

   -------------------------------------------------------------------------

   procedure Extract_And_Pad
      (Source : String;
       Target : String;
       Offset : Interfaces.Unsigned_64;
       Size   : Interfaces.Unsigned_64)
   is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      use type Interfaces.Unsigned_64;

      subtype Buffer_Range_Type is Stream_Element_Offset range 1 .. 2048;

      In_FD       : File_Type;
      Out_FD      : File_Type;
      Source_Size : Stream_IO.Count;
      Written     : Stream_Element_Offset := 0;
      Buffer      : Stream_Element_Array (Buffer_Range_Type);
      Last        : Stream_Element_Offset;

      --  Write the data in the given buffer to the target file, adjusting
      --  the total bytes written and returning true if all necessary data
      --  has been written.
      function Write_Buffer
         (Buf : Stream_Element_Array)
         return Boolean;

      ----------------------------------------------------------------------

      function Write_Buffer
         (Buf : Stream_Element_Array)
         return Boolean
      is
      begin
         Last := Stream_Element_Offset'Min
            (Last, Stream_Element_Offset (Size) - Written);
         Write (File => Out_FD, Item => Buf (1 .. Last));
         Written := Written + Last;
         return Written = Stream_Element_Offset (Size) or Last < Buf'Last;
      end Write_Buffer;
   begin
      Open (File => In_FD, Mode => In_File, Name => Source);
      Source_Size := Stream_IO.Size (File => In_FD);

      if Offset = 0 and Source_Size < Stream_IO.Count (Size) then
         Mulog.Log (Msg => "Pad '" & Source & "' from "
                            & Mutools.Utils.To_Hex (Number =>
                               Interfaces.Unsigned_64 (Source_Size))
                            & " to "
                            & Mutools.Utils.To_Hex (Number => Size)
                            & " to '" & Target & "'");
      else
         Mulog.Log (Msg => "Extracting part of '" & Source & "' at offset "
                            & Mutools.Utils.To_Hex (Number => Offset)
                            & " with size "
                            & Mutools.Utils.To_Hex (Number => Size)
                            & " to '" & Target & "'");
      end if;

      Set_Index (File => In_FD,
                 To   => Stream_IO.Count (Offset) + 1);
      Create (File => Out_FD, Mode => Out_File, Name => Target);
      loop
         Read (File => In_FD,
               Item => Buffer,
               Last => Last);
         exit when Write_Buffer (Buffer);
      end loop;
      Close (File => In_FD);

      if Written < Stream_Element_Offset (Size) then
         declare
            Padding : constant Stream_Element_Array (Buffer_Range_Type)
              := (others => 0);
         begin
            Last := Padding'Last;
            loop
               exit when Write_Buffer (Padding);
            end loop;
         end;
      end if;
      Close (File => Out_FD);
   end Extract_And_Pad;

   -------------------------------------------------------------------------

   procedure Register_Files
      (CPU                :        Natural;
       PT                 :        Unbounded_String;
       Nodes              :        DOM.Core.Node_List;
       File_Backed_Memory : in out File_Map_Package.Map;
       Used_Memory        : in out String_Sets_Package.Set;
       Kernels            : in out CPU_Kernel_Map_Package.Map)
   is
      --  Mark a single filename as being referenced. If it's a kernel image,
      --  keep track of that for the given CPU.
      procedure Register_File
         (CPU      : Natural;
          Virtual  : Unbounded_String;
          Physical : Unbounded_String);

      ----------------------------------------------------------------------

      procedure Register_File
         (CPU      : Natural;
          Virtual  : Unbounded_String;
          Physical : Unbounded_String)
      is
      begin
         if File_Backed_Memory.Contains (Physical) then
            declare
               File     : constant Loadable_File
                 := File_Backed_Memory (Physical);
               Filename : constant Unbounded_String
                 := File.Filename;
            begin
               Mulog.Log (Msg => "  memory '" & To_String (Virtual)
                                  & "' is backed by '" & To_String (Physical)
                                  & "' and the file '" & To_String (Filename)
                                  & "'");
               Used_Memory.Include (Physical);
               if File.Kernel then
                  Kernels.Include (CPU, File);
               end if;
            end;
         end if;
      end Register_File;
   begin
      Register_File (CPU, To_Unbounded_String ("pt"), PT);

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Logical  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "logical");
            Physical : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "physical");
         begin
            Register_File (CPU, To_Unbounded_String (Logical),
                           To_Unbounded_String (Physical));
         end;
      end loop;
   end Register_Files;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir  : String;
      Policy      : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      --  Map from physical memory name to loadable file.
      File_Backed_Memory : File_Map_Package.Map;
      --  Set of physical regions that are actually referenced.
      Used_Memory        : String_Sets_Package.Set;
      --  Map from CPU to kernel text files.
      Kernels            : CPU_Kernel_Map_Package.Map;

      File_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory/file");
      Fill_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[fill]");
      CPUs     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/kernel/memory/cpu");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Files    : File_Vector_Package.Vector;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => File_Mem) - 1 loop
         declare
            File_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => File_Mem,
                 Index => I);
            Mem_Node   : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => File_Node);
            Name       : constant Unbounded_String
              := To_Unbounded_String (DOM.Core.Elements.Get_Attribute
                (Elem => Mem_Node,
                 Name => "name"));
            Filename   : constant Unbounded_String
              := To_Unbounded_String (DOM.Core.Elements.Get_Attribute
                (Elem => File_Node,
                 Name => "filename"));
            Path       : constant String
              := Output_Dir & "/" & To_String (Filename);
            File_Size  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (Ada.Directories.Size (Path));
            Pad_Name   : constant Unbounded_String
              := Filename & "-" &
                Translate (To_Unbounded_String (Mutools.Utils.To_Ada_Identifier
                (Str => To_String (Name))),
                  Ada.Strings.Maps.Constants.Lower_Case_Map) & ".pad";
            Pad_Path   : constant String
              := Output_Dir & "/" & To_String (Pad_Name);
            Split_Name : constant Unbounded_String
              := Filename & "-" &
                Translate (To_Unbounded_String (Mutools.Utils.To_Ada_Identifier
                (Str => To_String (Name))),
                  Ada.Strings.Maps.Constants.Lower_Case_Map) & ".part";
            Split_Path : constant String
              := Output_Dir & "/" & To_String (Split_Name);
            Address    : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "physicalAddress"));
            Size       : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "size"));
            Offset_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => File_Node,
                 Name => "offset");
            Offset     : Interfaces.Unsigned_64 := 0;
            Kind       : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "type"));
            File       : Loadable_File
              := (Physical => Name,
                  Filename => Filename,
                  Address  => Address,
                  Kernel   => Kind in Mutools.Types.Kernel_Binary and then
                              Name = "kernel_text");
         begin
            Mulog.Log (Msg => "Found file-backed memory '"
                               & To_String (Name) & "' at "
                               & Mutools.Utils.To_Hex (Number => Address)
                               & " with file '"
                               & To_String (Filename) & "'");

            if Offset_Str = "none" then
               if File_Size > Size then
                  raise Generator_Error with "File '" & Path & "' is too large "
                     & "for physical memory region '" & To_String (Name) & "': "
                     & Mutools.Utils.To_Hex (Number => File_Size) & " > "
                     & Mutools.Utils.To_Hex (Number => Size);
               elsif File_Size < Size then
                  Extract_And_Pad
                     (Source => Path,
                      Target => Pad_Path,
                      Offset => 0,
                      Size   => Size);
                  File.Filename := Pad_Name;
               end if;
            else
               Offset := Interfaces.Unsigned_64'Value (Offset_Str);
               if Offset > File_Size then
                  raise Generator_Error with "Offset into file '" & Path
                     & "' referenced by physical memory region '"
                     & To_String (Name)
                     & "' is larger than file size: "
                     & Mutools.Utils.To_Hex (Number => Offset) & " > "
                     & Mutools.Utils.To_Hex (Number => File_Size);
               end if;
               Extract_And_Pad
                  (Source => Path,
                   Target => Split_Path,
                   Offset => Offset,
                   Size   => Size);
               File.Filename := Split_Name;
            end if;

            File_Backed_Memory.Include (Name, File);
            Files.Append (File);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Fill_Mem) - 1 loop
         declare
            Fill_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Fill_Mem,
                 Index => I);
            Name      : constant Unbounded_String
              := To_Unbounded_String (DOM.Core.Elements.Get_Attribute
                (Elem => Fill_Node,
                 Name => "name"));
            Filename  : constant Unbounded_String
              := Translate (To_Unbounded_String (Mutools.Utils.To_Ada_Identifier
                (Str => To_String (Name))),
                  Ada.Strings.Maps.Constants.Lower_Case_Map) & ".fill";
            Path      : constant String
              := Output_Dir & "/" & To_String (Filename);
            Address   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Fill_Node,
                    Name => "physicalAddress"));
            Size      : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Fill_Node,
                    Name => "size"));
            Pattern   : constant Interfaces.Unsigned_8
              := Interfaces.Unsigned_8'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   => Fill_Node,
                    XPath => "fill",
                    Name  => "pattern"));
            File      : constant Loadable_File
              := (Physical => Name,
                  Filename => Filename,
                  Address  => Address,
                  Kernel   => False);
         begin
            -- Generate a file with the given size and pattern and ..
            Create_Fill_File (Path, Size, Pattern);

            Mulog.Log (Msg => "Found filled memory '"
                      & To_String (Name) & "' at "
                      & Mutools.Utils.To_Hex (Number => Address)
                      & " of size "
                      & Mutools.Utils.To_Hex (Number => Size)
                      & " with pattern "
                      & Mutools.Utils.To_Hex
                        (Number => Interfaces.Unsigned_64 (Pattern))
                      & " written to "
                      & Path);

            -- ... treat this node like a file-backed memory region.
            File_Backed_Memory.Include (Name, File);
            Files.Append (File);
         end;
      end loop;

      File_Vector_Sorting_Package.Sort (Files);

      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            Kernel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPUs,
                 Index => I);
            CPU : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Kernel_Node,
                 Name => "id");
            CPU_Id : constant Natural
              := Natural'Value (CPU);

            Kernel_PT  : constant Unbounded_String
              := To_Unbounded_String ("kernel_" & CPU & "|pt");
            Kernel_Mem : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Kernel_Node,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Collect file-backed kernel memory for CPU "
                               & CPU);

            Register_Files (CPU_Id, Kernel_PT, Kernel_Mem,
                            File_Backed_Memory, Used_Memory, Kernels);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            CPU  : constant Natural
               := Natural'Value
                 (DOM.Core.Elements.Get_Attribute
                   (Elem => Subj_Node,
                    Name => "cpu"));
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");

            Subject_PT  : constant Unbounded_String
              := To_Unbounded_String (Name & "|pt");
            Subject_Mem : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
         begin
            Mulog.Log (Msg => "Collect file-backed subject memory for '"
                               & Name & "'");

            Register_Files (CPU, Subject_PT, Subject_Mem,
                            File_Backed_Memory, Used_Memory, Kernels);
         end;
      end loop;

      for I in reverse Files.First_Index .. Files.Last_Index loop
         if not Used_Memory.Contains (Files (I).Physical) then
            Mulog.Log (Msg => "Ignoring unused file '"
                               & To_String (Files (I).Filename) & "'");
            Files.Delete (I);
         end if;
      end loop;

      DS.BIF.Write (Kernels, Files, Output_Dir);

      DS.GDB.Write (Kernels, Files, Output_Dir);

      DS.UBOOT.Write (Kernels, Files, Output_Dir);

      DS.XSCT.Write (Kernels, Files, Output_Dir);

   end Write;

end DS.Generator;
