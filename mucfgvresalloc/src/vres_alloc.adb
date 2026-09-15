--
--  Copyright (C) 2023 secunet Security Networks AG
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
with Ada.Directories;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.Utils;
with Mutools.Xmldebuglog;

with Mulog;

with Muxml;
with Muxml.Utils;

package body Vres_Alloc
is
   --  Return the size of the given channel or memory region via a lookup of
   --  the physical name.
   function Get_Size_From_Physical
     (Node : DOM.Core.Node)
     return String;

   ----------------------------------------------------------------------

   --  Given an 'array' node, add mappings of the form
   --  'logical name'-> ('virtual resource','size')
   --  for all elements of the array to Mapping.
   procedure Add_Array_Entries_To_Mapping
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   ----------------------------------------------------------------------

   --  Add a mapping from the key of the given node (Mapping_Key) to the
   --  specified Value to the given map. Raises Validation_Error if a mapping
   --  with a different value is already present or if a virtual address
   --  resource is not aligned.
   procedure Add_Mapping
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Node          :        DOM.Core.Node;
      Value         :        Address_And_Size_Type;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   ----------------------------------------------------------------------

   --  Return the virtual resource of the given resource kind declared by the
   --  specified Node. Raises Validation_Error if the node requests automatic
   --  allocation instead of declaring a fixed resource.
   function Get_Fixed_Resource_Value
     (Node          : DOM.Core.Node;
      Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type)
      return Interfaces.Unsigned_64;

   ----------------------------------------------------------------------

   --  Return the key for the virtual resource of the given Node which is used
   --  to store it in the value map. The logical name is prefixed with the
   --  resource kind to ensure that they do not clash, since logical names are
   --  only required to be unique per resource kind. Device memory regions are
   --  additionally prefixed with the logical device name as their names are
   --  only unique per device. Raises Validation_Error if the node tag is not
   --  a known resource element.
   function Mapping_Key (Node : DOM.Core.Node) return String;

   ----------------------------------------------------------------------

   --  Read targets within the given component declaration Comp_Node
   --  and use these entries to populate Components_Map (mapping logical
   --  names to virtual resources).
   procedure Init_Component_Elements
     (Comp_Node     : DOM.Core.Node;
      Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type);

   ----------------------------------------------------------------------

   --  Insert a new mapping into Components_Map mapping the 'name' of Comp_Node
   --  to its profile and empty mappings for virtual resources.
   procedure Init_Component_Head (Comp_Node : DOM.Core.Node);

   ----------------------------------------------------------------------

   --  Fill 'Components_Map' with information from the 'components'
   --  section of Policy.
   procedure Initialize_Components_Map
     (Policy : Muxml.XML_Data_Type);

   ----------------------------------------------------------------------

   --  Fill the maps 'Memory_Sizes' and 'Channel_Sizes' with entries of the
   --  form 'channelsname' -> 'channelsize'.
   procedure Initialize_Size_Maps
     (Policy : Muxml.XML_Data_Type);

   ----------------------------------------------------------------------

   --  For each element of Targets, add a mapping from 'logical' to
   --  a virtual resource (debpending on Resource_Kind) into Components_Map.
   procedure Put_Targets_In_Map
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Targets       :        DOM.Core.Node_List;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   ----------------------------------------------------------------------

   --  Iterate over target nodes:
   --    If the virtual resource (specified by Resource_Kind) of the target
   --    is set, subtract these resources from Available_Intervals.
   --    Otherwise:
   --       Set the resource if it is listed in Mapping.
   --       Otherwise: Put the target node into Todo_List.
   --  Raises an exception of some target lacks the requested resource and
   --  Read_Only is true.
   procedure Assign_Resources_If_Known
     (Targets             :        DOM.Core.Node_List;
      Todo_List           : in out Muxml.Utils.Node_List_Package.List;
      Available_Intervals : in out Mutools.Intervals.Interval_List_Type;
      Mapping             :        Logical_To_Interval_Package.Map;
      Read_Only           :        Boolean;
      Resource_Kind       :        Mutools.Vres_Alloc.Resource_Kind_Type);

   ----------------------------------------------------------------------

   --  Iterate over Todo_List and assign virtual resources from
   --  Available_Intervals.
   procedure Allocate_Todo_Nodes
     (Todo_List     :        Muxml.Utils.Node_List_Package.List;
      Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   -------------------------------------------------------------------------

   procedure Add_Array_Entries_To_Mapping
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Interfaces.Unsigned_64;
      use type DOM.Core.Node_Types;

      Base_U64   : constant Interfaces.Unsigned_64
        := Get_Fixed_Resource_Value
        (Node          => Node,
         Resource_Kind => Resource_Kind);
      Size       : constant Interfaces.Unsigned_64
        := Mutools.Vres_Alloc.Get_Resource_Size
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
      Child_List : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (N => Node);
      Child      : DOM.Core.Node;
      Count      : Interfaces.Unsigned_64 := 0;
   begin
      for I in 0 ..  DOM.Core.Nodes.Length (List => Child_List) - 1 loop
         Child := DOM.Core.Nodes.Item (List  => Child_List,
                                       Index => I);
         if DOM.Core.Nodes.Node_Type (N => Child) = DOM.Core.Element_Node
           and then Mutools.Vres_Alloc.Config.Array_Element_Names.Contains
             (DOM.Core.Nodes.Node_Name (N => Child))
         then
            Add_Mapping
              (Mapping       => Mapping,
               Node          => Child,
               Value         => (First_Address => Base_U64 + (Size * Count),
                                 Size          => Size),
               Resource_Kind => Resource_Kind);
            Count := Count + 1;
         end if;
      end loop;
   end Add_Array_Entries_To_Mapping;

   -------------------------------------------------------------------------

   procedure Add_Mapping
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Node          :        DOM.Core.Node;
      Value         :        Address_And_Size_Type;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Mutools.Vres_Alloc.Resource_Kind_Type;

      --  Return string representation of given address and size type.
      function To_String (I : Address_And_Size_Type) return String;

      ----------------------------------------------------------------------

      function To_String (I : Address_And_Size_Type) return String
      is
      begin
         return "(First_Address="
           & Mutools.Utils.To_Hex (Number => I.First_Address)
           & ", Size="
           & Mutools.Utils.To_Decimal (I.Size)
           & ")";
      end To_String;

      Cursor   : Logical_To_Interval_Package.Cursor;
      Inserted : Boolean;
   begin
      if Resource_Kind = Mutools.Vres_Alloc.Virtual_Addresses
        and then not Mutools.Vres_Alloc.Is_Aligned
          (Address => Value.First_Address,
           Size    => Value.Size)
      then
         Mulog.Log (Msg => "Error: Size or virtual address of node is not "
                      & "a multiple of 16#1000#. XPath: '"
                      & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                      & "', Size: '"
                      & Mutools.Utils.To_Hex (Number => Value.Size)
                      & "', Virtual address: '"
                      & Mutools.Utils.To_Hex (Number => Value.First_Address)
                      & "'");
         raise Validation_Error with "Virtual resource not aligned";
      end if;

      Logical_To_Interval_Package.Insert
        (Container => Mapping,
         Key       => Mapping_Key (Node => Node),
         New_Item  => Value,
         Position  => Cursor,
         Inserted  => Inserted);

      --  If the key exists, check if the value is equal.

      if not Inserted and then
        Logical_To_Interval_Package.Element (Position => Cursor) /= Value
      then
         Mulog.Log (Msg => "Error: Conflicting resource values for node at '"
                      & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                      & "'");
         raise Validation_Error with
           "Conflicting resource values for logical '"
           & DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical")
           & "': found '"
           & To_String (I => Logical_To_Interval_Package.Element
                          (Position => Cursor))
           & "' and '" & To_String (I => Value) & "'";
      end if;
   end Add_Mapping;

   -------------------------------------------------------------------------

   procedure Add_Resource_To_Mapping
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
   begin
      Add_Mapping
        (Mapping       => Mapping,
         Node          => Node,
         Value         =>
           (First_Address => Get_Fixed_Resource_Value
              (Node          => Node,
               Resource_Kind => Resource_Kind),
            Size          => Mutools.Vres_Alloc.Get_Resource_Size
              (Elem          => Node,
               Resource_Kind => Resource_Kind)),
         Resource_Kind => Resource_Kind);
   end Add_Resource_To_Mapping;

   -------------------------------------------------------------------------

   procedure Allocate_Todo_Nodes
     (Todo_List     :        Muxml.Utils.Node_List_Package.List;
      Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Muxml.Utils.Node_List_Package.Cursor;
      Curr      : Muxml.Utils.Node_List_Package.Cursor
        := Todo_List.First;
      Curr_Node : DOM.Core.Node;
      Size      : Interfaces.Unsigned_64;
   begin
      while Curr /= Muxml.Utils.Node_List_Package.No_Element loop
         Curr_Node :=  Muxml.Utils.Node_List_Package.Element (Curr);
         case Resource_Kind is
            when Mutools.Vres_Alloc.Virtual_Addresses =>
               Size := Interfaces.Unsigned_64'Value
                 (Get_Size_From_Physical (Node => Curr_Node));
            when Mutools.Vres_Alloc.Vector_Numbers
              | Mutools.Vres_Alloc.Event_Numbers =>
               Size := 1;
         end case;
         Mutools.Vres_Alloc.Allocate_And_Set_Single_Resource
           (Av_Ival       => Av_Ival,
            Node          => Curr_Node,
            Resource_Kind => Resource_Kind,
            Size          => Mutools.Utils.To_Decimal (Size));
         Muxml.Utils.Node_List_Package.Next (Curr);
      end loop;
   end Allocate_Todo_Nodes;

   -------------------------------------------------------------------------

   procedure Assign_Resources_If_Known
     (Targets             :        DOM.Core.Node_List;
      Todo_List           : in out Muxml.Utils.Node_List_Package.List;
      Available_Intervals : in out Mutools.Intervals.Interval_List_Type;
      Mapping             :        Logical_To_Interval_Package.Map;
      Read_Only           :        Boolean;
      Resource_Kind       :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Mutools.Vres_Alloc.Resource_Kind_Type;

      Curr_Node : DOM.Core.Node;
      Known     : Logical_To_Interval_Package.Cursor;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Targets) - 1 loop
         Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                           Index => I);
         declare
            Attr_Value : constant String
              := Mutools.Vres_Alloc.Get_Resource_Value
              (Elem          => Curr_Node,
               Resource_Kind => Resource_Kind);
            Size       : Interfaces.Unsigned_64;
         begin
            if Mutools.Vres_Alloc.Requests_Allocation (Value => Attr_Value)
            then
               if Read_Only then
                  Mulog.Log
                    (Msg => "Found read-only node which requested automatic "
                       & "allocation of virtual resource. Xpath: '"
                       & Mutools.Xmldebuglog.Get_Xpath (Node => Curr_Node)
                       & "', value: '"
                       & Attr_Value
                       & "'");
                  raise Validation_Error with "Invalid attribute value";
               end if;

               Known := Logical_To_Interval_Package.Find
                 (Container => Mapping,
                  Key       => Mapping_Key (Node => Curr_Node));
               if Logical_To_Interval_Package.Has_Element (Position => Known)
               then
                  Mutools.Vres_Alloc.Set_Virtual_Resource
                    (Node          => Curr_Node,
                     Resource_Kind => Resource_Kind,
                     Value         => Logical_To_Interval_Package.Element
                       (Position => Known).First_Address);
               else
                  Muxml.Utils.Node_List_Package.Append
                    (Container => Todo_List,
                     New_Item  => Curr_Node);
               end if;
            else
               case Resource_Kind is
                  when Mutools.Vres_Alloc.Virtual_Addresses =>
                     Size := Interfaces.Unsigned_64'Value
                       (Get_Size_From_Physical (Node => Curr_Node));
                  when Mutools.Vres_Alloc.Vector_Numbers
                    | Mutools.Vres_Alloc.Event_Numbers =>
                     Size := 1;
               end case;

               if Resource_Kind = Mutools.Vres_Alloc.Virtual_Addresses and then
                 not Mutools.Vres_Alloc.Is_Aligned
                 (Address => Interfaces.Unsigned_64'Value (Attr_Value),
                  Size    => Size)
               then
                  Mulog.Log (Msg => "Error: Size or virtual address of node is "
                               & "not a multiple of 16#1000#. XPath: '"
                               & Mutools.Xmldebuglog.Get_Xpath
                               (Node => Curr_Node)
                               & "', Size: '"
                               & Mutools.Utils.To_Hex (Number => Size)
                               & "', Virtual address: '"
                               & Attr_Value
                               & "'");
                  raise Validation_Error with "Virtual resource not aligned";
               end if;

               Mutools.Intervals.Subtract_Interval
                 (List          => Available_Intervals,
                  First_Element => Interfaces.Unsigned_64'Value (Attr_Value),
                  Size          => Size);
            end if;
         end;
      end loop;
   end Assign_Resources_If_Known;

   -------------------------------------------------------------------------

   function Get_Fixed_Resource_Value
     (Node          : DOM.Core.Node;
      Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type)
      return Interfaces.Unsigned_64
   is
      Attr_Value : constant String
        := Mutools.Vres_Alloc.Get_Resource_Value
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
   begin
      if Mutools.Vres_Alloc.Requests_Allocation (Value => Attr_Value) then
         raise Validation_Error with
           "Missing attribute value at '"
           & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
           & "'";
      end if;

      return Interfaces.Unsigned_64'Value (Attr_Value);
   end Get_Fixed_Resource_Value;

   -------------------------------------------------------------------------

   function Get_Size_From_Physical
     (Node : DOM.Core.Node)
     return String
   is
      Physical     : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                            Name => "physical");
      Tag_Name     : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Node);
   begin
      if Tag_Name = "memory" then
         if not Memory_Sizes.Contains (Physical) or else
           Memory_Sizes (Physical) = ""
         then
            raise Validation_Error with
              "Cannot find size of node at '"
              & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
              & "' with physical '"
              & Physical
              & "'";
         end if;
         return Memory_Sizes (Physical);
      else
         if not Channel_Sizes.Contains (Physical) or else
           Channel_Sizes (Physical) = ""
         then
            raise Validation_Error with
              "Cannot find size of node at '"
              & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
              & "' with physical '"
              & Physical
              & "'";
         end if;
         return Channel_Sizes (Physical);
      end if;
   end Get_Size_From_Physical;

   ----------------------------------------------------------------------

   procedure Init_Component_Elements
     (Comp_Node     : DOM.Core.Node;
      Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      type Map_Access_Type is access all Logical_To_Interval_Package.Map;
      package Config renames Mutools.Vres_Alloc.Config;

      Comp_Name                      : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "name");
      Targets_List_1, Targets_List_2 : Mutools.String_Vector.Vector;
      Targets                        : DOM.Core.Node_List;
      Mapping_Access                 : Map_Access_Type;

   begin
      case Resource_Kind is
         when Mutools.Vres_Alloc.Virtual_Addresses =>
            Targets_List_1 := Config.C_Va_Alloc_Read_Write_Targets;
            Targets_List_2 := Config.C_Va_Alloc_Read_Only_Targets;
            Mapping_Access := Components_Map (Comp_Name).Va_Map'Access;
         when Mutools.Vres_Alloc.Vector_Numbers =>
            Targets_List_1 := Config.C_Vector_Numbers_Read_Write_Targets;
            Targets_List_2 := Config.C_Vector_Numbers_Read_Only_Targets;
            Mapping_Access := Components_Map (Comp_Name).Vector_Numbers_Map'Access;
         when Mutools.Vres_Alloc.Event_Numbers =>
            Targets_List_1 := Config.C_Event_Numbers_Read_Write_Targets;
            Targets_List_2 := Config.C_Event_Numbers_Read_Only_Targets;
            Mapping_Access := Components_Map (Comp_Name).Event_Numbers_Map'Access;
      end case;

      Targets := McKae.XML.XPath.XIA.XPath_Query
        (N     => Comp_Node,
         XPath => Mutools.Vres_Alloc.Get_Target_String
           (Target_List => Targets_List_1,
            Prefix      => "./"));
      Put_Targets_In_Map (Mapping  => Mapping_Access.all,
                          Targets  => Targets,
                          Resource_Kind => Resource_Kind);

      Targets := McKae.XML.XPath.XIA.XPath_Query
        (N     => Comp_Node,
         XPath => Mutools.Vres_Alloc.Get_Target_String
           (Target_List => Targets_List_2,
            Prefix      => "./"));
      Put_Targets_In_Map (Mapping  => Mapping_Access.all,
                          Targets  => Targets,
                          Resource_Kind => Resource_Kind);
   end Init_Component_Elements;

   -------------------------------------------------------------------------

   procedure Init_Component_Head (Comp_Node : DOM.Core.Node)
   is
      Comp_Name    : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "name");
      Comp_Profile : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "profile");
   begin
      if not Component_To_Map_Package.Contains
        (Container => Components_Map,
         Key       => Comp_Name)
      then
         Component_To_Map_Package.Insert
           (Container => Components_Map,
            Key       => Comp_Name,
            New_Item  =>
              (Profile           => Mutools.String_Holder_Type.To_Holder
                 (Comp_Profile),
               Va_Map             => Logical_To_Interval_Package.Empty_Map,
               Vector_Numbers_Map => Logical_To_Interval_Package.Empty_Map,
               Event_Numbers_Map  => Logical_To_Interval_Package.Empty_Map));
      end if;
   end Init_Component_Head;

   -------------------------------------------------------------------------

   procedure Initialize_Components_Map
     (Policy : Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/components/component");
      Curr_Component : DOM.Core.Node;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         Curr_Component := DOM.Core.Nodes.Item (List  => Components,
                                                Index => I);
         Init_Component_Head (Comp_Node => Curr_Component);

         Init_Component_Elements
           (Comp_Node     => Curr_Component,
            Resource_Kind => Mutools.Vres_Alloc.Virtual_Addresses);
         Init_Component_Elements
           (Comp_Node     => Curr_Component,
            Resource_Kind => Mutools.Vres_Alloc.Event_Numbers);
         Init_Component_Elements
           (Comp_Node     => Curr_Component,
            Resource_Kind => Mutools.Vres_Alloc.Vector_Numbers);
      end loop;
   end Initialize_Components_Map;

   -------------------------------------------------------------------------

   procedure Initialize_Size_Maps
     (Policy : Muxml.XML_Data_Type)
   is
      Memory_Entries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory");
      Channel_Entries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/channels/channel");

      procedure Generic_Init
        (Targets  :        DOM.Core.Node_List;
         Hash_Map : in out Mutools.Expressions.Name_To_String_Hashed_Map.Map);

      procedure Generic_Init
        (Targets  :        DOM.Core.Node_List;
         Hash_Map : in out Mutools.Expressions.Name_To_String_Hashed_Map.Map)
      is
         Curr_Node : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Targets) - 1 loop
            Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                              Index => I);
            Hash_Map.Insert
              (Key      => DOM.Core.Elements.Get_Attribute (Elem => Curr_Node,
                                                            Name => "name"),
               New_Item => DOM.Core.Elements.Get_Attribute (Elem => Curr_Node,
                                                            Name => "size"));
         end loop;

      exception
         when Constraint_Error =>
            raise Validation_Error
              with "Duplicate physical "
              & DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node)
              & " with name '"
              & DOM.Core.Elements.Get_Attribute (Elem => Curr_Node,
                                                 Name => "name")
              & "'";
      end Generic_Init;

   begin
      Generic_Init (Targets  => Memory_Entries,
                    Hash_Map => Memory_Sizes);
      Generic_Init (Targets  => Channel_Entries,
                    Hash_Map => Channel_Sizes);
   end Initialize_Size_Maps;

   -------------------------------------------------------------------------

   function Mapping_Key (Node : DOM.Core.Node) return String
   is
      use type DOM.Core.Node;

      Tag_Name : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Node);
      Logical  : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                            Name => "logical");
      Parent   : constant DOM.Core.Node
        := DOM.Core.Nodes.Parent_Node (N => Node);
   begin
      if not Mutools.Vres_Alloc.Config.Array_Element_Names.Contains (Tag_Name)
      then
         raise Validation_Error with
           "Found unexpected node tag '"
           & Tag_Name
           & "' at '"
           & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
           & "'";
      end if;

      if Tag_Name = "memory"
        and then Parent /= null
        and then DOM.Core.Nodes.Node_Name (N => Parent) = "device"
      then
         return "device/"
           & DOM.Core.Elements.Get_Attribute (Elem => Parent,
                                              Name => "logical")
           & "/" & Logical;
      elsif Tag_Name = "reader" or Tag_Name = "writer" then
         return "channel/" & Logical;
      else
         return Tag_Name & "/" & Logical;
      end if;
   end Mapping_Key;

   -------------------------------------------------------------------------

   procedure Put_Targets_In_Map
     (Mapping       : in out Logical_To_Interval_Package.Map;
      Targets       :        DOM.Core.Node_List;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      Curr_Node : DOM.Core.Node;
   begin
      for I in 0 ..  DOM.Core.Nodes.Length (List => Targets) - 1 loop
         Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                           Index => I);

         if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) = "array"
         then
            Add_Array_Entries_To_Mapping
              (Mapping       => Mapping,
               Node          => Curr_Node,
               Resource_Kind => Resource_Kind);
         else
            Add_Resource_To_Mapping
              (Mapping       => Mapping,
               Node          => Curr_Node,
               Resource_Kind => Resource_Kind);
         end if;
      end loop;

   end Put_Targets_In_Map;

   ----------------------------------------------------------------------

   procedure Run
     (Policy_File_Name : String;
      Output_File_Name : String)
   is
      Policy : Muxml.XML_Data_Type;

      procedure Process_Subject
        (Subject        : DOM.Core.Node;
         Component_Info : Component_Info_Type;
         Resource_Kind  : Mutools.Vres_Alloc.Resource_Kind_Type);

      -------------------------------------------------------------------

      procedure Process_Subject
        (Subject        : DOM.Core.Node;
         Component_Info : Component_Info_Type;
         Resource_Kind  : Mutools.Vres_Alloc.Resource_Kind_Type)
      is
         use type Ada.Containers.Count_Type;
         type Map_Access_Type is access constant Logical_To_Interval_Package.Map;

         Available_Intervals : Mutools.Intervals.Interval_List_Type;
         Todo_List           : Muxml.Utils.Node_List_Package.List;
         Mapping_Access      : Map_Access_Type;
      begin
         --  Initialize free space for virtual resource.
         --  Note: Subtracting the resources reserved by the component
         --  specification (stored in Component_Info.*_Map)
         --  is delayed to avoid it when possible (i.e., in case all virtual
         --  resources of the subject are fixed by the component specification).
         case Resource_Kind is
            when Mutools.Vres_Alloc.Virtual_Addresses =>
               if not (Mutools.Vres_Alloc.Is_Aligned
                         (Address => Va_Space_Native.First_Element)
                  and Mutools.Vres_Alloc.Is_Aligned
                         (Address => Va_Space_Vm.First_Element))
               then
                  raise Validation_Error with
                    "Default values for virtual address domains are not "
                    & "aligned to page size.";
               end if;
               if Component_Info.Profile.Element = "native"
               then
                  Mutools.Intervals.Add_Interval
                    (List     => Available_Intervals,
                     Interval => Va_Space_Native);
               else
                  Mutools.Intervals.Add_Interval
                    (List     => Available_Intervals,
                     Interval => Va_Space_Vm);
               end if;
            when Mutools.Vres_Alloc.Event_Numbers =>
               Mutools.Intervals.Add_Interval
                 (List     => Available_Intervals,
                  Interval => Event_Numbers_Domain);
            when Mutools.Vres_Alloc.Vector_Numbers =>
               Mutools.Intervals.Add_Interval
                 (List     => Available_Intervals,
                  Interval => Vector_Numbers_Domain);
         end case;

         --  Choose targets and mapping depending on Resource_Kind
         declare
            package Config renames Mutools.Vres_Alloc.Config;

            Targets_List_R, Targets_List_R_W : Mutools.String_Vector.Vector;
            Targets                          : DOM.Core.Node_List;
         begin
            case Resource_Kind is
               when Mutools.Vres_Alloc.Virtual_Addresses =>
                  Targets_List_R_W := Config.Va_Alloc_Read_Write_Targets;
                  Targets_List_R   := Config.Va_Alloc_Read_Only_Targets;
                  Mapping_Access   := Component_Info.Va_Map'Access;
               when Mutools.Vres_Alloc.Vector_Numbers =>
                  Targets_List_R_W := Config.Vector_Numbers_Read_Write_Targets;
                  Targets_List_R   := Config.Vector_Numbers_Read_Only_Targets;
                  Mapping_Access   := Component_Info.Vector_Numbers_Map'Access;
               when Mutools.Vres_Alloc.Event_Numbers =>
                  Targets_List_R_W := Config.Event_Numbers_Read_Write_Targets;
                  Targets_List_R   := Config.Event_Numbers_Read_Only_Targets;
                  Mapping_Access   := Component_Info.Event_Numbers_Map'Access;
            end case;

            --  Put Targets within Subject on todo-list if resource
            --  is not given or fixed by component.
            --  Assign it, when fixed by referenced component or by an event.
            --  Read_Only targets must come first as they may contain new
            --  relevant virtual resources.
            Targets := McKae.XML.XPath.XIA.XPath_Query
              (N     => Subject,
               XPath => Mutools.Vres_Alloc.Get_Target_String
                 (Target_List => Targets_List_R,
                  Prefix      => "."));
            Assign_Resources_If_Known
              (Targets             => Targets,
               Todo_List           => Todo_List,
               Available_Intervals => Available_Intervals,
               Mapping             => Mapping_Access.all,
               Read_Only           => True,
               Resource_Kind       => Resource_Kind);

            Targets := McKae.XML.XPath.XIA.XPath_Query
              (N     => Subject,
               XPath => Mutools.Vres_Alloc.Get_Target_String
                 (Target_List => Targets_List_R_W,
                  Prefix      => "."));
            Assign_Resources_If_Known
              (Targets             => Targets,
               Todo_List           => Todo_List,
               Available_Intervals => Available_Intervals,
               Mapping             => Mapping_Access.all,
               Read_Only           => False,
               Resource_Kind       => Resource_Kind);
         end;

         --  allocate new resources for remaining nodes
         if Todo_List.Length > 0 then
            --  finish restriction of available intervals
            for Addr_And_Size of Mapping_Access.all loop
               Mutools.Intervals.Subtract_Interval
                 (List => Available_Intervals,
                  First_Element => Addr_And_Size.First_Address,
                  Size          => Addr_And_Size.Size);
            end loop;

            --  Iterate over todo-nodes and allocate.
            Mulog.Log (Msg => "Setting " & Resource_Kind'Image & " for"
                         & Todo_List.Length'Img
                         & " element(s) of subject '"
                         & DOM.Core.Elements.Get_Attribute
                         (Elem => Subject,
                          Name => "name")
                         & "'");
            Allocate_Todo_Nodes (Todo_List     => Todo_List,
                                 Av_Ival       => Available_Intervals,
                                 Resource_Kind => Resource_Kind);
         end if;
      end Process_Subject;
   begin
      Mulog.Log (Msg => "Processing joined policy");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src_Ext,
                   File => Policy_File_Name);

      --  Read global information about sizes of channels and memory
      --  regions (stored in hashmap for performance reasons).
      Initialize_Size_Maps (Policy => Policy);

      --  Initialize Components_Map with information about the targets in the
      --  'components' section.
      Initialize_Components_Map (Policy => Policy);

      --  Main loop: iterate over subjects.
      declare
         use type DOM.Core.Node;

         Subjects : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject");
         Curr_Subject : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
            Curr_Subject := DOM.Core.Nodes.Item (List  => Subjects,
                                                 Index => I);
            Mulog.Log (Msg => "Analyzing virtual resources for subject '"
                         & DOM.Core.Elements.Get_Attribute
                         (Elem => Curr_Subject,
                          Name => "name")
                         & "'");
            --  Get component info.
            declare
               Component_Name : constant String
                 := Muxml.Utils.Get_Attribute
                 (Doc   => Curr_Subject,
                  XPath => "component",
                  Name  => "ref");
               Sibling_Node  : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                 (Doc   => Curr_Subject,
                  XPath => "sibling");
            begin
               --  Virtual memory in siblings cannot be autoallocated at the moment
               --  (OK because siblings must not specify memory).
               if Sibling_Node = null then
                  if Component_To_Map_Package.Contains
                    (Container => Components_Map,
                     Key       => Component_Name)
                  then
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Resource_Kind  => Mutools.Vres_Alloc.Virtual_Addresses);
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Resource_Kind  => Mutools.Vres_Alloc.Vector_Numbers);
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Resource_Kind  => Mutools.Vres_Alloc.Event_Numbers);
                  else
                     raise Validation_Error with
                       "Cannot find component with name '"
                       & Component_Name
                       & "'";
                  end if;
               end if;
            end;
         end loop;

         --  Write output data.
         if not Ada.Directories.Exists
           (Ada.Directories.Containing_Directory (Output_File_Name))
         then
            Ada.Directories.Create_Path
              (Ada.Directories.Containing_Directory (Output_File_Name));
         end if;
         Muxml.Write
           (File => Output_File_Name,
            Kind => Muxml.Format_Src_Ext,
            Data => Policy);
         Mulog.Log (Msg => "Successfully wrote policy with virtual "
                      & "resources to '"
                      & Output_File_Name & "'");
      end;
   end Run;

   ----------------------------------------------------------------------

end Vres_Alloc;
