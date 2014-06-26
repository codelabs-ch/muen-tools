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

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.Layouts;

with Alloc.Map;

with Mutools.Constants;
with Muxml.Utils;

package body Expanders.XML_Utils
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Calculate_PT_Size
     (Policy             : Muxml.XML_Data_Type;
      Paging_Levels      : Paging.Paging_Level;
      Large_Pages        : Boolean;
      Dev_Virt_Mem_XPath : String;
      Virt_Mem_XPath     : String)
      return Interfaces.Unsigned_64
   is
      use type DOM.Core.Node;

      Layout       : Paging.Layouts.Memory_Layout_Type
        (Levels => Paging_Levels);
      Device_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Dev_Virt_Mem_XPath);
      Memory_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Virt_Mem_XPath);
   begin
      Paging.Layouts.Set_Large_Page_Support (Mem_Layout => Layout,
                                             State      => Large_Pages);

      for I in 0 .. DOM.Core.Nodes.Length (List => Memory_Nodes) - 1 loop
         declare
            Logical : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory_Nodes,
                 Index => I);
            Physical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "physical");
            Physical : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/memory/memory[@name='" & Physical_Name
                 & "']");
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "size"));
            Alignment : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "alignment"));
         begin
            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => Alignment,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Device_Nodes) - 1 loop
         declare
            Logical_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Device_Nodes,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Logical_Mem),
                 Name => "physical");
            Physical_Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "physical");
            Physical_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/platform/devices/device[@name='" & Dev_Name
                 & "']/memory[@name='" & Physical_Mem_Name & "']");
            Physical_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Mem,
                    Name => "physicalAddress"));
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Mem,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Mem,
                    Name => "size"));
         begin
            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => Physical_Address,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      declare
         use type Interfaces.Unsigned_64;

         Table_Counts : constant Paging.Layouts.Table_Count_Array
           := Paging.Layouts.Get_Table_Count (Mem_Layout => Layout);
         Count        : Natural := 0;
      begin
         for C of Table_Counts loop
            Count := Count + C;
         end loop;
         return Interfaces.Unsigned_64 (Count) * Mutools.Constants.Page_Size;
      end;
   end Calculate_PT_Size;

   -------------------------------------------------------------------------

   function Calculate_Region_Address
     (Policy             : Muxml.XML_Data_Type;
      Fixed_Memory       : DOM.Core.Node_List;
      Device_Memory      : DOM.Core.Node_List;
      Address_Space_Size : Interfaces.Unsigned_64;
      Region_Size        : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
   is
      Map : Alloc.Map.Map_Type;
   begin
      Map.Insert_Empty_Region (Name          => U ("Mem"),
                               Allocatable   => True,
                               First_Address => 0,
                               Last_Address  => Address_Space_Size);

      for I in 0 .. DOM.Core.Nodes.Length (List => Fixed_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Fixed_Memory,
                                      Index => I);
            Virt_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical");
            Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical");
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   =>  Policy.Doc,
                    XPath => "/system/memory/memory[@name='" & Phy_Name & "']",
                    Name  => "size"));
         begin
            Map.Allocate_Fixed (Name          => U (Virt_Name),
                                First_Address => Virt_Addr,
                                Last_Address  => Virt_Addr + Size - 1);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Device_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Device_Memory,
                                      Index => I);
            Virt_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical");
            Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical");
            Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
               Name => "physical");
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   =>  Policy.Doc,
                    XPath => "/system/platform/devices/device[@name='"
                    & Dev_Name & "']/memory[@name='" & Phy_Name & "']",
                    Name  => "size"));
         begin
            Map.Allocate_Fixed (Name          => U (Virt_Name),
                                First_Address => Virt_Addr,
                                Last_Address  => Virt_Addr + Size - 1);
         end;
      end loop;

      Map.Allocate_Variable (Name => U ("New_Region"),
                             Size => Region_Size);
      return Map.Get_Region (Name => "New_Region").First_Address;
   end Calculate_Region_Address;

   -------------------------------------------------------------------------

   function Create_Source_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      ID            :        String;
      Logical_Name  :        String;
      Physical_Name :        String;
      Action        :        String)
      return DOM.Core.Node
   is
      Event_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "event");
      Notify_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "notify");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "id",
         Value => ID);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "action",
         Value => Action);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Notify_Node,
         Name  => "physical",
         Value => Physical_Name);

      Muxml.Utils.Append_Child
        (Node      => Event_Node,
         New_Child => Notify_Node);

      return Event_Node;
   end Create_Source_Event_Node;

   -------------------------------------------------------------------------

   function Create_Target_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Vector        :        String)
      return DOM.Core.Node
   is
      Event_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "event");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "physical",
         Value => Physical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "vector",
         Value => Vector);

      return Event_Node;
   end Create_Target_Event_Node;

end Expanders.XML_Utils;
