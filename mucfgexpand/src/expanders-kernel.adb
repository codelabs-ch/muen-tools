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

with Ada.Strings.Fixed;

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;

with Expand.XML_Utils;

package body Expanders.Kernel
is

   -------------------------------------------------------------------------

   procedure Add_Binary_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Kernel_Node  : DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
               (N     => Data.Doc,
                XPath => "/system/kernel"),
           Index => 0);
      CPU_Count    : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
               (Doc   => Data.Doc,
                XPath => "/system/platform/processor",
                Name  => "logicalCpus"));
      Section_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
   begin
      Mulog.Log (Msg => "Adding binary memory mappings for" & CPU_Count'Img
                 & " kernel(s)");

      --  TODO: Remove this after device expansion

      if Kernel_Node = null then
         declare
            Subjects_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                     (N     => Data.Doc,
                      XPath => "/system/subjects"),
                 Index => 0);
         begin
            Kernel_Node := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "kernel");
            Kernel_Node := DOM.Core.Nodes.Insert_Before
              (N         => DOM.Core.Nodes.Parent_Node (N => Subjects_Node),
               New_Child => Kernel_Node,
               Ref_Child => Subjects_Node);
         end;
      end if;

      Expand.XML_Utils.Append_Child (Node      => Kernel_Node,
                                     New_Child => Section_Node);

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => "cpu");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => CPU_Node,
               Name  => "id",
               Value => CPU_Str);
            Expand.XML_Utils.Append_Child
              (Node      => Section_Node,
               New_Child => CPU_Node);

            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "text",
                  Physical_Name => "kernel_text",
                  Address       => "16#0010_0000#",
                  Writable      => False,
                  Executable    => True));
            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "data",
                  Physical_Name => "kernel_data",
                  Address       => "16#0011_0000#",
                  Writable      => True,
                  Executable    => False));
            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "bss",
                  Physical_Name => "kernel_bss",
                  Address       => "16#0011_1000#",
                  Writable      => True,
                  Executable    => False));
            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "ro",
                  Physical_Name => "kernel_ro",
                  Address       => "16#0011_f000#",
                  Writable      => False,
                  Executable    => False));
            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "stack",
                  Physical_Name => "kernel_stack_" & CPU_Str,
                  Address       => "16#0011_3000#",
                  Writable      => True,
                  Executable    => False));
            Expand.XML_Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Expand.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "store",
                  Physical_Name => "kernel_store_" & CPU_Str,
                  Address       => "16#0011_6000#",
                  Writable      => True,
                  Executable    => False));
         end;
      end loop;
   end Add_Binary_Mappings;

end Expanders.Kernel;
