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

package Muxml.Utils
is

   --  Searches the element specified by an XPath in the given document and
   --  returns the attribute given by name as string. If no such attribute or
   --  element exists, an empty string is returned.
   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String;

   --  Set attribute 'Name' of element given by XPath to the specified value.
   --  If no such element exists, an exception is raised.
   procedure Set_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String;
      Value : String);

   --  Returns the element specified by an XPath in the given document. If no
   --  such element exists null is returned. The first match is returned if
   --  multiple elements are found.
   function Get_Element
     (Doc   : DOM.Core.Node;
      XPath : String)
      return DOM.Core.Node;

   --  Searches the element specified by an XPath in the given document and
   --  returns its value as string. If no such element exists, an empty string
   --  is returned.
   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String;

   --  Returns the element from the given node list with an attribute
   --  'Ref_Attr' that matches 'Ref_Value'. If no such element exists null is
   --  returned. The first match is returned if multiple elements are found.
   function Get_Element
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node;

   --  Returns all elements from the given node list with an attribute
   --  'Ref_Attr' that matches 'Ref_Value'. If no such element exists an empty
   --  node list is returned.
   function Get_Elements
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node_List;

   --  Returns the attribute 'Attr_Name' of the element from the given node
   --  list with an attribute 'Ref_Attr' that matches 'Ref_Value'. If no such
   --  element with the specified attribute exists an empty string is
   --  returned. The first match is returned if multiple elements are found.
   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String;
      Attr_Name : String)
      return String;

   type Ref_Attr_Type is record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Ref_Attrs_Type is array (Positive range <>) of Ref_Attr_Type;

   --  Returns the element from the given node list with a list of reference
   --  attributes (name, value pairs) that must all match. If no such element
   --  with the specified attribute exists null is returned. The first match is
   --  returned if multiple elements are found.
   function Get_Element
     (Nodes : DOM.Core.Node_List;
      Refs  : Ref_Attrs_Type)
      return DOM.Core.Node;

   --  Returns the attribute 'Attr_Name' of the element from the given node
   --  list with a list of reference attributes (name, value pairs) that must
   --  all match. If no such element with the specified attributes exists an
   --  empty string is returned. The first match is returned if multiple
   --  elements are found.
   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Refs      : Ref_Attrs_Type;
      Attr_Name : String)
      return String;

   --  Append all nodes of 'Right' to specified node list 'Left'.
   procedure Append
     (Left  : in out DOM.Core.Node_List;
      Right :        DOM.Core.Node_List);

   --  Append new child node to given node.
   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node);

   type Tags_Type is array (Positive range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   No_Tags : constant Tags_Type (1 .. 0) := (others => <>);

   --  Merge the right node incl. all its children into the left node. Values
   --  provided by the right node take precedence and replace existing data in
   --  the left node tree. Nothing is done if left and right do not have
   --  matching names. Child nodes matching one of the list tags are appended
   --  instead of merged into a single element.
   procedure Merge
     (Left      : DOM.Core.Node;
      Right     : DOM.Core.Node;
      List_Tags : Tags_Type := No_Tags);

   --  Return the ancestor at given level of the specified node.
   function Ancestor_Node
     (Node  : DOM.Core.Node;
      Level : Positive)
      return DOM.Core.Node;

   --  Remove child element node with given name. All children of the specified
   --  child node are removed as well. An exception is raised if no child with
   --  the given name exists.
   procedure Remove_Child
     (Node       : DOM.Core.Node;
      Child_Name : String);

   XML_Error : exception;

end Muxml.Utils;
