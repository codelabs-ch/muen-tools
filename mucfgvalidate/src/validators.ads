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

with DOM.Core;

with Interfaces;

with Muxml;

package Validators
is

   --  Register all known validators.
   procedure Register_All;

   Validation_Error : exception;

private

   use type Interfaces.Unsigned_64;

   type Test_Function is not null access function
     (A, B : Interfaces.Unsigned_64) return Boolean;

   function Equals
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left = Right);

   function Not_Equals
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left /= Right);

   function Less_Than
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left < Right);

   function Less_Or_Equal
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left <= Right);

   function Mod_Equal_Zero
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left mod Right = 0);

   --  Check attribute value 'Attr' of given 'Node_Type' nodes using the
   --  specified test function and function parameter 'Right'. 'Name_Attr'
   --  defines the attribute used to query the name of a specific node. If the
   --  test fails, an exception with the given 'Error_Msg' string is raised.
   procedure Check_Attribute
     (Nodes     : DOM.Core.Node_List;
      Node_Type : String;
      Attr      : String;
      Name_Attr : String;
      Test      : Test_Function;
      Right     : Interfaces.Unsigned_64;
      Error_Msg : String);

   --  Check memory overlap of given 'Region_Type' nodes. 'Name_Attr' specifies
   --  the name and 'Address_Attr' the address attribute of a given region
   --  node. If two memory regions overlap, an exception is raised with the
   --  given 'Add_Msg' string appended to log and error messages.
   procedure Check_Memory_Overlap
     (Nodes        : DOM.Core.Node_List;
      Region_Type  : String;
      Address_Attr : String;
      Name_Attr    : String := "name";
      Add_Msg      : String := "");

   --  Compare all elements in a node list against each other using the
   --  provided comparator procedure.
   procedure Compare_All
     (Nodes      : DOM.Core.Node_List;
      Comparator : not null access procedure (Left, Right : DOM.Core.Node));

   --  For each element specified by 'Source_XPath', try to find a match in the
   --  nodes specified by 'Ref_XPath' using the given 'Match' function. The
   --  given message is appended to the log message. If no match is found, an
   --  exception with the message returned by the 'Error' function is raised.
   procedure For_Each_Match
     (XML_Data     : Muxml.XML_Data_Type;
      Source_XPath : String;
      Ref_XPath    : String;
      Log_Message  : String;
      Error        : not null access function
        (Node : DOM.Core.Node) return String;
      Match        : not null access function
        (Left, Right : DOM.Core.Node) return Boolean);

   --  Returns True if the left node's 'physical' attribute matches the 'name'
   --  attribute of the right node.
   function Is_Valid_Reference (Left, Right : DOM.Core.Node) return Boolean;

end Validators;
