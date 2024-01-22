--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Append_Node;
with DOM.Core.Elements;
with DOM.Core.Attrs;
with DOM.Core.Documents.Local;
with DOM.Core;

with McKae.XML.XPath.XIA;

with Muxml.Grammar_Tools;

package body Muxml.Utils
is

   use Ada.Strings.Unbounded;

   --  Return first child that matches one of the child names. Null is returned
   --  if no match is found.
   function Get_Child_Node
     (Parent      : DOM.Core.Node;
      Child_Names : Tags_Type)
      return DOM.Core.Node;

   -------------------------------------------------------------------------

   procedure Add_Child
     (Parent     : DOM.Core.Node;
      Child_Name : String;
      Ref_Names  : Tags_Type := No_Tags)
   is
      Child : constant DOM.Core.Node
        := Get_Element
          (Doc   => Parent,
           XPath => Child_Name);
   begin
      if Child /= null then
         return;
      end if;

      if Ref_Names = No_Tags then
         Append_Child
           (Node      => Parent,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => DOM.Core.Nodes.Owner_Document (N => Parent),
               Tag_Name => Child_Name));
         return;
      end if;

      declare
         Ref_Node : constant DOM.Core.Node
           := Get_Child_Node (Parent      => Parent,
                              Child_Names => Ref_Names);
      begin
         if Ref_Node /= null then
            declare
               Child_Node : DOM.Core.Node
                 := DOM.Core.Documents.Create_Element
                   (Doc      => DOM.Core.Nodes.Owner_Document (N => Parent),
                    Tag_Name => Child_Name);
            begin
               Child_Node := DOM.Core.Nodes.Insert_Before
                 (N         => Parent,
                  New_Child => Child_Node,
                  Ref_Child => Ref_Node);
            end;
         end if;
      end;
   end Add_Child;

   -------------------------------------------------------------------------

   function Ancestor_Node
     (Node  : DOM.Core.Node;
      Level : Positive)
      return DOM.Core.Node
   is
      Result : DOM.Core.Node := Node;
   begin
      for I in 1 .. Level loop
         exit when Result = null;
         Result := DOM.Core.Nodes.Parent_Node (N => Result);
      end loop;

      return Result;
   end Ancestor_Node;

   -------------------------------------------------------------------------

   procedure Append
     (Left  : in out DOM.Core.Node_List;
      Right :        DOM.Core.Node_List)
   is

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Right) - 1 loop
         DOM.Core.Append_Node
           (List => Left,
            N    => DOM.Core.Nodes.Item
              (List  => Right,
               Index => I));
      end loop;
   end Append;

   -------------------------------------------------------------------------

   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node)
   is
      Dummy : DOM.Core.Node;
      pragma Unreferenced (Dummy);
   begin
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => New_Child);
   end Append_Child;

   -------------------------------------------------------------------------

   function Contains
     (List : DOM.Core.Node_List;
      Node : DOM.Core.Node)
      return Boolean
   is
      Count : constant Natural := DOM.Core.Nodes.Length (List => List);
   begin
      if Node = null then
         return False;
      end if;

      for I in 0 .. Count - 1 loop
         if Node = DOM.Core.Nodes.Item
           (List  => List,
            Index => I)
         then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   ----------------------------------------------------------------------

   function Count_Element_Children
     (Node : DOM.Core.Node)
     return Natural
   is
      use type DOM.Core.Node_Types;

      Curr_Node  : DOM.Core.Node;
      Child_List : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (N => Node);
      Count      : Natural := 0;
   begin
      for I in 0 ..  DOM.Core.Nodes.Length (List => Child_List) - 1 loop
         Curr_Node := DOM.Core.Nodes.Item (List  => Child_List,
                                           Index => I);
         if DOM.Core.Nodes.Node_Type (N => Curr_Node) = DOM.Core.Element_Node then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Element_Children;

   -------------------------------------------------------------------------

   function Get_Ancestor_Names
     (Node : DOM.Core.Node)
     return  String_Vector.Vector
   is
      Output : String_Vector.Vector;
      Parent : DOM.Core.Node
        := DOM.Core.Nodes.Parent_Node (Node);
   begin
      Output.Append (DOM.Core.Nodes.Node_Name (N => Node));
      while Parent /= null loop
         Output.Append (DOM.Core.Nodes.Node_Name (N => Parent));
         Parent := DOM.Core.Nodes.Parent_Node (Parent);
      end loop;
      return Output;
   end Get_Ancestor_Names;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String
   is
      Node : constant DOM.Core.Node := Get_Element
        (Doc   => Doc,
         XPath => XPath);
   begin
      if Node = null then
         return "";
      else
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => Name);
      end if;
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String;
      Attr_Name : String)
      return String
   is
   begin
      return Get_Attribute
        (Nodes     => Nodes,
         Refs      => (1 => (Name  => To_Unbounded_String (Ref_Attr),
                             Value => To_Unbounded_String (Ref_Value))),
         Attr_Name => Attr_Name);
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Refs      : Ref_Attrs_Type;
      Attr_Name : String)
      return String
   is
      Node : constant DOM.Core.Node
        := Get_Element (Nodes => Nodes,
                        Refs  => Refs);
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => Attr_Name);
      else
         return "";
      end if;
   end Get_Attribute;

   -------------------------------------------------------------------------

   procedure Get_Bounds
     (Nodes     :     DOM.Core.Node_List;
      Attr_Name :     String;
      Lower     : out DOM.Core.Node;
      Upper     : out DOM.Core.Node)
   is
      use type Interfaces.Unsigned_64;

      Lower_Value : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
      Upper_Value : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'First;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Value : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name));
         begin
            if Value > Upper_Value then
               Upper       := Node;
               Upper_Value := Value;
            end if;
            if Value < Lower_Value then
               Lower       := Node;
               Lower_Value := Value;
            end if;
         end;
      end loop;
   end Get_Bounds;

   -------------------------------------------------------------------------

   procedure Get_Bounds
     (Nodes     :     DOM.Core.Node_List;
      Attr_Name :     String;
      Lower     : out Interfaces.Unsigned_64;
      Upper     : out Interfaces.Unsigned_64)
   is
      Lower_Node, Upper_Node : DOM.Core.Node;
   begin
      Get_Bounds (Nodes     => Nodes,
                  Attr_Name => Attr_Name,
                  Lower     => Lower_Node,
                  Upper     => Upper_Node);
      Lower := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Lower_Node,
            Name => Attr_Name));
      Upper := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Upper_Node,
            Name => Attr_Name));
   end Get_Bounds;

   -------------------------------------------------------------------------

   function Get_Child_Node
     (Parent      : DOM.Core.Node;
      Child_Names : Tags_Type)
      return DOM.Core.Node
   is
      Node : DOM.Core.Node := null;
   begin
      for Child of Child_Names loop
         Node := Get_Element
           (Doc   => Parent,
            XPath => Ada.Strings.Unbounded.To_String (Child));
         exit when Node /= null;
      end loop;

      return Node;
   end Get_Child_Node;

   -------------------------------------------------------------------------

   function Get_Element
     (Doc   : DOM.Core.Node;
      XPath : String)
      return DOM.Core.Node
   is
      Element : DOM.Core.Node := null;
   begin
      if XPath'Length = 0 then
         raise XML_Error with "No XPath given";
      end if;

      if Doc /= null then
         Element := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Doc,
               XPath => XPath),
            Index => 0);
      end if;

      return Element;
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node
   is
   begin
      return Get_Element
        (Nodes => Nodes,
         Refs  => (1 => (Name  => To_Unbounded_String (Ref_Attr),
                         Value => To_Unbounded_String (Ref_Value))));
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element
     (Nodes : DOM.Core.Node_List;
      Refs  : Ref_Attrs_Type)
      return DOM.Core.Node
   is
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      for I in 0 .. Count - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Match : Boolean;
         begin
            for Ref of Refs loop
               Match := To_String (Ref.Value) = DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => To_String (Ref.Name));

               exit when not Match;
            end loop;

            if Match then
               return Node;
            end if;
         end;
      end loop;

      return null;
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String
   is
      Node : constant DOM.Core.Node
        := Get_Element
          (Doc   => Doc,
           XPath => XPath & "/text()");
   begin
      if Node = null then
         return "";
      else
         return DOM.Core.Nodes.Node_Value (N => Node);
      end if;
   end Get_Element_Value;

   -------------------------------------------------------------------------

   function Get_Elements
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node_List
   is
      Count  : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
      Result : DOM.Core.Node_List;
   begin
      for I in 0 .. Count - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
         begin
            if DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Ref_Attr) = Ref_Value
            then
               DOM.Core.Append_Node (List => Result,
                                     N    => Node);
            end if;
         end;
      end loop;

      return Result;
   end Get_Elements;

   -------------------------------------------------------------------------

   function Get_Matching
     (Left_Nodes     : DOM.Core.Node_List;
      Right_Nodes    : DOM.Core.Node_List;
      Match_Multiple : Boolean := False;
      Match          : not null access function
        (Left, Right : DOM.Core.Node) return Boolean)
      return Matching_Pairs_Type
   is
      Result : Matching_Pairs_Type;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Left_Nodes) - 1 loop
         declare
            Left_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Left_Nodes,
                 Index => I);
            Right_Node : DOM.Core.Node;
         begin
            Find_Match :
            for J in 0 .. DOM.Core.Nodes.Length (List => Right_Nodes) - 1 loop
               Right_Node := DOM.Core.Nodes.Item
                 (List  => Right_Nodes,
                  Index => J);

               if Match
                 (Left  => Left_Node,
                  Right => Right_Node)
               then
                  DOM.Core.Append_Node (List => Result.Left,
                                        N    => Left_Node);
                  DOM.Core.Append_Node (List => Result.Right,
                                        N    => Right_Node);
                  exit Find_Match when not Match_Multiple;
               end if;
            end loop Find_Match;
         end;
      end loop;

      return Result;
   end Get_Matching;

   -------------------------------------------------------------------------

   function Get_Matching
     (XML_Data       : XML_Data_Type;
      Left_XPath     : String;
      Right_XPath    : String;
      Match_Multiple : Boolean := False;
      Match          : not null access function
        (Left, Right : DOM.Core.Node) return Boolean)
      return Matching_Pairs_Type
   is
      L : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Left_XPath);
      R : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Right_XPath);
   begin
      return Get_Matching (Left_Nodes     => L,
                           Right_Nodes    => R,
                           Match_Multiple => Match_Multiple,
                           Match          => Match);
   end Get_Matching;

   -------------------------------------------------------------------------

   function Get_Unique_Element_Child
     (Parent     : DOM.Core.Node;
      Child_Name : String)
      return DOM.Core.Node
   is
      use all type DOM.Core.Node_Types;
      use all type DOM.Core.Node;

      Node : DOM.Core.Node
         := DOM.Core.Nodes.First_Child (N => Parent);
      Result : DOM.Core.Node
         := null;
   begin
      while Node /= null loop
         if DOM.Core.Nodes.Node_Type (N => Node) =  Element_Node
            and then DOM.Core.Nodes.Node_Name (N => Node) = Child_Name
         then
            if Result = null then
               Result := Node;
            else
               return null;
            end if;
         end if;
         Node := DOM.Core.Nodes.Next_Sibling (N => Node);
      end loop;
      return Result;
   end Get_Unique_Element_Child;

   -------------------------------------------------------------------------

   function Has_Attribute
     (Node      : DOM.Core.Node;
      Attr_Name : String)
      return Boolean
   is
      use type DOM.Core.Node;
   begin
      return null /= DOM.Core.Nodes.Get_Named_Item
         (Map  => DOM.Core.Nodes.Attributes (N => Node),
          Name => Attr_Name);
   end Has_Attribute;

   -------------------------------------------------------------------------

   procedure Insert_Before
     (Parent    : DOM.Core.Node;
      New_Child : DOM.Core.Node;
      Ref_Child : String)
   is
   begin
      Insert_Before
        (Parent    => Parent,
         New_Child => New_Child,
         Ref_Names => (1 => To_Unbounded_String (Ref_Child)));
   end Insert_Before;

   -------------------------------------------------------------------------

   procedure Insert_Before
     (Parent    : DOM.Core.Node;
      New_Child : DOM.Core.Node;
      Ref_Names : Tags_Type)
   is
      use type DOM.Core.Node;

      Ref_Child_Node : DOM.Core.Node
        := Get_Child_Node (Parent      => Parent,
                           Child_Names => Ref_Names);

      Dummy : DOM.Core.Node;
   begin
      declare
         Ref_Children : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Parent,
              XPath => DOM.Core.Nodes.Node_Name (N => New_Child));
         Count : constant Natural
           := DOM.Core.Nodes.Length (List => Ref_Children);
      begin
         if Count > 0 then
            Ref_Child_Node := DOM.Core.Nodes.Next_Sibling
              (N => DOM.Core.Nodes.Item
                 (List  => Ref_Children,
                  Index => Count - 1));
         end if;
      end;

      if Ref_Child_Node /= null then
         Dummy := DOM.Core.Nodes.Insert_Before
           (N         => Parent,
            New_Child => New_Child,
            Ref_Child => Ref_Child_Node);
      else
         Append_Child (Node      => Parent,
                       New_Child => New_Child);
      end if;
   end Insert_Before;

   -------------------------------------------------------------------------

   procedure Insert_Child
     (Parent      : DOM.Core.Node;
      New_Child   : DOM.Core.Node;
      Clone_Child : Boolean := False)
   is
      Siblings_Names  : String_Vector.Vector;
      Siblings_Nodes  : Node_Vector.Vector;
      Insertion_Index : Natural;
   begin
      Insert_Child
        (Parent          => Parent,
         New_Child       => New_Child,
         Clone_Child     => Clone_Child,
         Siblings_Names  => Siblings_Names,
         Siblings_Nodes  => Siblings_Nodes,
         Ancestors       => Get_Ancestor_Names (Node => Parent),
         Insertion_Index => Insertion_Index);
   end Insert_Child;

   -------------------------------------------------------------------------

   procedure Insert_Child
     (Parent          :        DOM.Core.Node;
      New_Child       :        DOM.Core.Node;
      Clone_Child     :        Boolean := False;
      Siblings_Names  : in out String_Vector.Vector;
      Siblings_Nodes  : in out Node_Vector.Vector;
      Ancestors       :        String_Vector.Vector;
      Insertion_Index :    out Natural)
   is
      use type String_Vector.Vector;
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      use type Grammar_Tools.Insert_Query_Result_Type;

      Query_Result             : Grammar_Tools.Insert_Query_Result_Type;
      Ref_Child, Inserted_Node : DOM.Core.Node;
   begin
      --  Initialize missing information
      if Siblings_Names = String_Vector.Empty_Vector then
         declare
            Parent_Child : DOM.Core.Node
              := DOM.Core.Nodes.First_Child (N => Parent);
         begin
            while Parent_Child /= null loop
               if DOM.Core.Nodes.Node_Type (N => Parent_Child)
                 = DOM.Core.Element_Node
               then
                  Siblings_Nodes.Append (Parent_Child);
                  Siblings_Names.Append (DOM.Core.Nodes.Node_Name
                                           (N => Parent_Child));
               end if;
               Parent_Child := DOM.Core.Nodes.Next_Sibling
                 (N => Parent_Child);
            end loop;
         end;
      end if;

      --  Determine insertion reference node
      Query_Result := Muxml.Grammar_Tools.Get_Insert_Index
        (Ancestors => Ancestors,
         New_Child => DOM.Core.Nodes.Node_Name (N => New_Child),
         Siblings  => Siblings_Names);

      if Query_Result = Muxml.Grammar_Tools.No_Legal_Index then
         raise Muxml.Validation_Error with
           "Could not find valid place to insert '"
           & DOM.Core.Nodes.Node_Name (N => New_Child)
           & "' into node with name '"
           & DOM.Core.Nodes.Node_Name (N => Parent)
           & "'";
      elsif Query_Result = Muxml.Grammar_Tools.No_Unique_Index then
         raise Muxml.Validation_Error with
           "Insufficient information to insert '"
           & DOM.Core.Nodes.Node_Name (N => New_Child)
           & "' into node with name '"
           & DOM.Core.Nodes.Node_Name (N => Parent)
           & "'";
      elsif Query_Result = Muxml.Grammar_Tools.
        Insert_Query_Result_Type (Siblings_Names.Length)
      then
         --  This triggers Insert_Before to append the new child at the end.
         Ref_Child := null;
         Insertion_Index := Natural (Query_Result);
      else
         Ref_Child := Siblings_Nodes (Natural (Query_Result));
         Insertion_Index := Natural (Query_Result);
      end if;

      --  Do the insertion
      if Clone_Child then
         Inserted_Node := DOM.Core.Nodes.Insert_Before
           (N         => Parent,
            New_Child => DOM.Core.Documents.Local.Clone_Node
              (N    => New_Child,
               Deep => True),
            Ref_Child => Ref_Child);
      else
         Inserted_Node := DOM.Core.Nodes.Insert_Before
           (N         => Parent,
            New_Child => New_Child,
            Ref_Child => Ref_Child);
      end if;

      --  Update siblings (for potential reuse lateron)
      Siblings_Names.Insert (Before   => Insertion_Index,
                             New_Item => DOM.Core.Nodes.Node_Name
                               (N => Inserted_Node));
      Siblings_Nodes.Insert (Before   => Insertion_Index,
                             New_Item => Inserted_Node);
   end Insert_Child;

   -------------------------------------------------------------------------

   procedure Merge
     (Left      : DOM.Core.Node;
      Right     : DOM.Core.Node;
      List_Tags : Tags_Type := No_Tags)
   is
      use type DOM.Core.Node;

      Left_Doc : constant DOM.Core.Document
        := DOM.Core.Nodes.Owner_Document (N => Left);

      --  Returns True if the given name matches a list tag.
      function Is_List_Tag (Name : String) return Boolean;

      ----------------------------------------------------------------------

      function Is_List_Tag (Name : String) return Boolean
      is
      begin
         for Tag of List_Tags loop
            if Name = Ada.Strings.Unbounded.To_String (Source => Tag) then
               return True;
            end if;
         end loop;
         return False;
      end Is_List_Tag;
   begin
      if DOM.Core.Nodes.Node_Name (N => Left)
        /= DOM.Core.Nodes.Node_Name (N => Right)
      then
         return;
      end if;

      declare
         R_Child : DOM.Core.Node := DOM.Core.Nodes.First_Child (N => Right);
      begin
         while R_Child /= null loop
            declare
               L_Child : DOM.Core.Node := DOM.Core.Nodes.Last_Child
                 (N => Left);
            begin

               --  Find matching children.

               while L_Child /= null and then
                 DOM.Core.Nodes.Node_Name (N => L_Child)
                 /= DOM.Core.Nodes.Node_Name (N => R_Child)
               loop
                  L_Child := DOM.Core.Nodes.Previous_Sibling (N => L_Child);
               end loop;

               if L_Child = null then

                  --  No match or list found, attach right child incl. all
                  --  children to left.

                  Append_Child
                    (Node      => Left,
                     New_Child => DOM.Core.Documents.Local.Adopt_Node
                       (Doc    => Left_Doc,
                        Source => DOM.Core.Documents.Local.Clone_Node
                          (N    => R_Child,
                           Deep => True)));
               elsif Is_List_Tag
                 (Name => DOM.Core.Nodes.Node_Name (N => L_Child))
               then
                  declare
                     Dummy : DOM.Core.Node;
                  begin
                     Dummy := DOM.Core.Nodes.Insert_Before
                       (N         => Left,
                        New_Child => DOM.Core.Documents.Local.Adopt_Node
                          (Doc    => Left_Doc,
                           Source => DOM.Core.Documents.Local.Clone_Node
                             (N    => R_Child,
                              Deep => True)),
                        Ref_Child => DOM.Core.Nodes.Next_Sibling
                          (N => L_Child));
                  end;
               else
                  Merge (Left      => L_Child,
                         Right     => R_Child,
                         List_Tags => List_Tags);
               end if;
            end;

            R_Child := DOM.Core.Nodes.Next_Sibling (N => R_Child);
         end loop;
      end;

      DOM.Core.Nodes.Set_Node_Value
        (N     => Left,
         Value => DOM.Core.Nodes.Node_Value (N => Right));

      declare
         Attrs : constant DOM.Core.Named_Node_Map
           := DOM.Core.Nodes.Attributes (N => Right);
         Node  : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (Map => Attrs) - 1 loop
            Node := DOM.Core.Nodes.Item (Map   => Attrs,
                                         Index => I);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Left,
               Name  => DOM.Core.Attrs.Name (Att => Node),
               Value => DOM.Core.Attrs.Value (Att => Node));
         end loop;
      end;
   end Merge;

   -------------------------------------------------------------------------

   function Next_Node
     (Current_Node       : DOM.Core.Node;
      Only_Element_Nodes : Boolean       := False;
      Stop_Node          : DOM.Core.Node := null)
      return DOM.Core.Node
   is
      use type DOM.Core.Node;
      use all type DOM.Core.Node_Types;

      --  Recursively go to the parent until some parent has a next sibling
      --  and return that sibling.
      --  Returns null if there is no such node.
      function Backtrack_Until_Sibling
         (Node : DOM.Core.Node)
         return DOM.Core.Node;

      ----------------------------------------------------------------------

      function Backtrack_Until_Sibling
        (Node : DOM.Core.Node)
         return DOM.Core.Node
      is
      begin
         if Node = Stop_Node then
            return null;
         end if;

         if DOM.Core.Nodes.Next_Sibling (N => Node) /= null then
            return DOM.Core.Nodes.Next_Sibling (N => Node);
         elsif DOM.Core.Nodes.Parent_Node (N => Node) /= null then
            return Backtrack_Until_Sibling
               (Node => DOM.Core.Nodes.Parent_Node (N => Node));
         else
            return null;
         end if;
      end Backtrack_Until_Sibling;

      ----------------------------------------------------------------------

      --  Recurse until an element-node is found (if that is required).
      function Recurse_Until_Element
        (Node : DOM.Core.Node)
         return DOM.Core.Node;

      ----------------------------------------------------------------------

      function Recurse_Until_Element
        (Node : DOM.Core.Node)
         return DOM.Core.Node
      is
      begin
         if Node = Stop_Node then
            return null;
         end if;

         if Only_Element_Nodes then
            if Node /= null and then
               DOM.Core.Nodes.Node_Type (N => Node) /= Element_Node
            then
               return Next_Node (Current_Node       => Node,
                                 Only_Element_Nodes => Only_Element_Nodes,
                                 Stop_Node          => Stop_Node);
            else
               return Node;
            end if;
         else
            return Node;
         end if;
      end Recurse_Until_Element;

   begin
      if Current_Node /= null then
         if DOM.Core.Nodes.Has_Child_Nodes (N => Current_Node) then
            return Recurse_Until_Element
               (Node => DOM.Core.Nodes.First_Child (N => Current_Node));
         elsif DOM.Core.Nodes.Next_Sibling (N => Current_Node) /= null then
            return Recurse_Until_Element
               (Node => DOM.Core.Nodes.Next_Sibling (N => Current_Node));
         elsif DOM.Core.Nodes.Parent_Node (N => Current_Node) /= null then
            return Recurse_Until_Element
               (Node => Backtrack_Until_Sibling
                   (Node => DOM.Core.Nodes.Parent_Node
                       (N => Current_Node)));
         else
            return null;
         end if;
      else
         return null;
      end if;
   end Next_Node;

   -------------------------------------------------------------------------

   function Next_Node_In_Subtree
     (Root_Node          : DOM.Core.Node;
      Current_Node       : DOM.Core.Node;
      Only_Element_Nodes : Boolean := False)
      return DOM.Core.Node
   is
      use all type DOM.Core.Node;
   begin
      if Current_Node /= Root_Node or
         DOM.Core.Nodes.Has_Child_Nodes (N => Current_Node)
      then
         return Next_Node
            (Current_Node       => Current_Node,
             Only_Element_Nodes => Only_Element_Nodes,
             Stop_Node          => Root_Node);
      else
         return null;
      end if;
   end Next_Node_In_Subtree;

   -------------------------------------------------------------------------

   procedure Remove_Child
     (Node       : DOM.Core.Node;
      Child_Name : String)
   is
      Children : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (N => Node);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Children) - 1 loop
         declare
            Child : DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Children,
               Index => I);
         begin
            if DOM.Core.Nodes.Node_Name (N => Child) = Child_Name then
               Child := DOM.Core.Nodes.Remove_Child
                 (N         => Node,
                  Old_Child => Child);
               DOM.Core.Nodes.Free (N => Child);
               return;
            end if;
         end;
      end loop;

      raise XML_Error with "Unable to remove child '" & Child_Name
        & "' from node '" & DOM.Core.Nodes.Node_Name (N => Node) & "'";
   end Remove_Child;

   -------------------------------------------------------------------------

   procedure Remove_Elements
     (Doc   : DOM.Core.Node;
      XPath : String)
   is
      use type DOM.Core.Node;
   begin
      if Doc /= null then
         declare
            Nodes : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Doc,
                 XPath => XPath);
         begin
            for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
               declare
                  Node : DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Nodes,
                       Index => I);
               begin
                  Node := DOM.Core.Nodes.Remove_Child
                    (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                     Old_Child => Node);
                  DOM.Core.Nodes.Free (N => Node);
               end;
            end loop;
         end;
      end if;
   end Remove_Elements;

   -------------------------------------------------------------------------

   procedure Set_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String;
      Value : String)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Doc,
           XPath => XPath);
      Count : constant Natural := DOM.Core.Nodes.Length (Nodes);
   begin
      if Count = 0 then
         raise XML_Error with "Unable to set attribute '" & Name & "' to "
           & "value '" & Value & "' - No element found at XPath '" & XPath
           & "'";
      end if;

      for I in 0 .. Count - 1 loop
         DOM.Core.Elements.Set_Attribute
           (Elem  => DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I),
            Name  => Name,
            Value => Value);
      end loop;
   end Set_Attribute;

   -------------------------------------------------------------------------

   procedure Set_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String;
      Value : String)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Doc,
           XPath => XPath);
      Count : constant Natural := DOM.Core.Nodes.Length (Nodes);
   begin
      if Count = 0 then
         raise XML_Error with "Unable to set element value to '"
           & Value & "' - No element found at XPath '" & XPath & "'";
      end if;

      for I in 0 .. Count - 1 loop
         declare
            use type DOM.Core.Node;
            use type DOM.Core.Node_Types;

            Element : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Text_Node_Found : Boolean := False;
            Child : DOM.Core.Node := DOM.Core.Nodes.First_Child (N => Element);
         begin
            Set_Text_Loop :
            while Child /= null loop
               if DOM.Core.Nodes.Node_Type (N => Child) = DOM.Core.Text_Node
               then
                  DOM.Core.Nodes.Set_Node_Value
                    (N     => Child,
                     Value => Value);
                  Text_Node_Found := True;
                  exit Set_Text_Loop;
               end if;
               Child := DOM.Core.Nodes.Next_Sibling (N => Child);
            end loop Set_Text_Loop;

            if not Text_Node_Found then
               Append_Child
                 (Node      => Element,
                  New_Child => DOM.Core.Documents.Create_Text_Node
                    (Doc  => DOM.Core.Nodes.Owner_Document (N => Doc),
                     Data => Value));
            end if;
         end;
      end loop;
   end Set_Element_Value;

   -------------------------------------------------------------------------

   function Sum
     (Nodes  : DOM.Core.Node_List;
      Getter : not null access function (N : DOM.Core.Node) return String)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Node : DOM.Core.Node;
      Sum  : Interfaces.Unsigned_64 := 0;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         Node := DOM.Core.Nodes.Item
           (List  => Nodes,
            Index => I);
         Sum := Sum + Interfaces.Unsigned_64'Value
           (Getter (N => Node));
      end loop;

      return Sum;
   end Sum;

end Muxml.Utils;
