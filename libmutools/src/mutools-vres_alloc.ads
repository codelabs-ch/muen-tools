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

with Interfaces;

with DOM.Core;
use type DOM.Core.Node;

with Mutools.Intervals;

--  Procedures and functions for mucfgcvresalloc and mucfgvresalloc.
package Mutools.Vres_Alloc
is
   type Resource_Kind_Type is (Virtual_Addresses, Event_Numbers, Vector_Numbers);

   --  Depending on Resource_Kind, set the resource attribute of Node.
   --  A region of size Size is reserved in Av_Ival.
   --  If Size is not given, the 'size' attribute of Node is read instead.
   procedure Allocate_And_Set_Single_Resource
     (Av_Ival       : in out Intervals.Interval_List_Type;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Resource_Kind_Type;
      Size          :        String := "");

   --  Prefix the entries of Target_List and join them with "|" into an XPath.
   --  If Target_List is empty it returns an XPath that does not match any node.
   function Get_Target_String
     (Target_List : String_Vector.Vector;
      Prefix      : String)
     return String;

   --  For Resource_Kind=VIRTUAL_ADDRESS the size attribute of the memory region
   --  (or of one element of the array) is returned. Otherwise, 1 is returned.
   function Get_Resource_Size
     (Elem          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type)
     return Interfaces.Unsigned_64;

   --  Returns the value of "virtualAddress", "vector", "event",
   --  "virtualAddressBase", "vectorBase", "eventBase" or "id"
   --  attributes, depending on Resource_Kind and the tag of the node.
   --  If the needed attribute is not found, then "" is returned.
   --  Raises Validation_Error if the 'id' of a source event is set to 'auto',
   --  since allocation of source event IDs is requested by omission.
   function Get_Resource_Value
     (Elem          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type)
     return String;

   --  Depending on Resource_Kind and the tag of Node, set the attribute
   --  "virtualAddress", "event" (or "id" for source events) or "vector" of
   --  Node to Value. For arrays, the corresponding "virtualAddressBase",
   --  "eventBase" or "vectorBase" attribute is set.
   procedure Set_Virtual_Resource
     (Node          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type;
      Value         : Interfaces.Unsigned_64);

   --  Returns True if the given attribute value requests automatic allocation
   --  of a virtual resource.
   function Requests_Allocation (Value : String) return Boolean;

   --  Check if Address and Size is a multiple of 16#1000#
   --  (the size of one page) and that Size > 0 holds.
   function Is_Aligned
     (Address : Interfaces.Unsigned_64 := 16#1000#;
      Size    : Interfaces.Unsigned_64 := 16#1000#)
     return Boolean;

   Validation_Error : exception;

end Mutools.Vres_Alloc;
