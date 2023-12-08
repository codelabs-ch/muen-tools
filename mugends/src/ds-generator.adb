--
--  Copyright (C) 2023  Tobias Brunner <tobias@codelabs.ch>
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

   package String_Map_Package is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unbounded_String,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package String_Sets_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String);

   --  Map of all files referenced by physical memory nodes
   File_Map           : File_Map_Package.Map;
   --  Map from physical memory name to filename
   File_Backed_Memory : String_Map_Package.Map;
   --  Set of filenames that are actually referenced by kernels/subjects
   Used_Files         : String_Sets_Package.Set;
   --  Map from CPU to kernel image file
   Kernels            : CPU_Kernel_Map_Package.Map;

   --  Register the files referenced by the given memory and page table nodes.
   procedure Register_Files
      (CPU      : Natural;
       PT       : Unbounded_String;
       Nodes    : DOM.Core.Node_List);

   -------------------------------------------------------------------------

   procedure Register_Files
      (CPU      : Natural;
       PT       : Unbounded_String;
       Nodes    : DOM.Core.Node_List)
   is
      --  Mark a single filename as being referenced. If it's a kernel image,
      --  keep track of that for the given CPU.
      procedure Register_File
         (CPU      : Natural;
          Virtual  : Unbounded_String;
          Physical : Unbounded_String);

      procedure Register_File
         (CPU      : Natural;
          Virtual  : Unbounded_String;
          Physical : Unbounded_String)
      is
      begin
         if File_Backed_Memory.Contains (Physical) then
            declare
               Filename : constant Unbounded_String
                 := File_Backed_Memory (Physical);
               File     : constant Loadable_File
                 := File_Map (Filename);
            begin
               Mulog.Log (Msg => "  memory '" & To_String (Virtual)
                                  & "' is backed by '" & To_String (Physical)
                                  & "' and the file '" & To_String (Filename)
                                  & "'");
               Used_Files.Include (Filename);
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

      File_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[file]");
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
            Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => File_Mem,
                 Index => I);
            Name     : constant Unbounded_String
              := To_Unbounded_String (DOM.Core.Elements.Get_Attribute
                (Elem => Mem_Node,
                 Name => "name"));
            Filename : constant Unbounded_String
              := To_Unbounded_String (Muxml.Utils.Get_Attribute
                (Doc   => Mem_Node,
                 XPath => "file",
                 Name  => "filename"));
            Address  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "physicalAddress"));
            Kind     : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "type"));
            File     : constant Loadable_File
              := (Filename => Filename,
                  Address  => Address,
                  Kernel   => Kind in Mutools.Types.Kernel_Binary);
         begin
            File_Backed_Memory.Include (Name, Filename);

            if File_Map.Contains (Filename) then
               Mulog.Log (Msg => "Merge file-backed memory '"
                                  & To_String (Name) & "' at "
                                  & Mutools.Utils.To_Hex (Number => Address)
                                  & " with file '" & To_String (Filename) & "'");
               declare
                  Existing : Loadable_File := File_Map (Filename);
               begin
                  if Existing.Address > File.Address then
                     Existing.Address := File.Address;
                  end if;
                  if not Existing.Kernel and File.Kernel then
                     Existing.Kernel := True;
                  end if;
               end;
            else
               Mulog.Log (Msg => "Found file-backed memory '"
                                  & To_String (Name) & "' at "
                                  & Mutools.Utils.To_Hex (Number => Address)
                                  & " with file '" & To_String (Filename) & "'");
               File_Map.Include (Filename, File);
            end if;
         end;
      end loop;

      for F in File_Map.Iterate loop
         Files.Append (File_Map (F));
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

            Register_Files (CPU_Id, Kernel_PT, Kernel_Mem);
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

            Register_Files (CPU, Subject_PT, Subject_Mem);
         end;
      end loop;

      for I in reverse Files.First_Index .. Files.Last_Index loop
         if not Used_Files.Contains (Files (I).Filename) then
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
