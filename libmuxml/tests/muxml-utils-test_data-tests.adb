--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Muxml.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Muxml.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Attribute (Gnattest_T : in out Test);
   procedure Test_Get_Attribute_7606a9 (Gnattest_T : in out Test) renames Test_Get_Attribute;
--  id:2.2/7606a922e00da111/Get_Attribute/1/0/
   procedure Test_Get_Attribute (Gnattest_T : in out Test) is
   --  muxml-utils.ads:27:4:Get_Attribute
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "parentAttr",
         Value => "parent_attribute");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "childAttr",
         Value => "child_attribute");

      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "parentAttr") = "parent_attribute",
              Message   => "Attribute mismatch (1)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/parent/child",
               Name  => "childAttr") = "child_attribute",
              Message   => "Attribute mismatch (2)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (3)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "//grandchild",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (4)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "nonexistent",
               Name  => "someAttribute") = "",
              Message   => "Attribute mismatch (5)");
--  begin read only
   end Test_Get_Attribute;
--  end read only


--  begin read only
   procedure Test_Set_Attribute (Gnattest_T : in out Test);
   procedure Test_Set_Attribute_9ffe3d (Gnattest_T : in out Test) renames Test_Set_Attribute;
--  id:2.2/9ffe3df58c900cd0/Set_Attribute/1/0/
   procedure Test_Set_Attribute (Gnattest_T : in out Test) is
   --  muxml-utils.ads:35:4:Set_Attribute
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "elem");
      Append_Child (Node      => Data.Doc,
                    New_Child => Node);

      Set_Attribute (Doc   => Data.Doc,
                     XPath => "/elem",
                     Name  => "test",
                     Value => "value");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "test") = "value",
              Message   => "Attribute value mismatch");

      begin
         Set_Attribute (Doc   => Data.Doc,
                        XPath => "/nonexistent",
                        Name  => "foo",
                        Value => "bar");
      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to set attribute 'foo' to value 'bar' - No "
                    & "element found at XPath '/nonexistent'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Set_Attribute;
--  end read only


--  begin read only
   procedure Test_1_Get_Element (Gnattest_T : in out Test);
   procedure Test_Get_Element_d0ef1a (Gnattest_T : in out Test) renames Test_1_Get_Element;
--  id:2.2/d0ef1aa70f2c2b3a/Get_Element/1/0/
   procedure Test_1_Get_Element (Gnattest_T : in out Test) is
   --  muxml-utils.ads:44:4:Get_Element
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "parent")) = "parent",
              Message   => "Element mismatch (1)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "/parent/child")) = "child",
              Message   => "Element mismatch (2)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "//grandchild")) = "grandchild",
              Message   => "Element mismatch (3)");
      Assert (Condition => Get_Element
              (Doc   => Data.Doc,
               XPath => "nonexistent") = null,
              Message   => "Element mismatch (4)");
--  begin read only
   end Test_1_Get_Element;
--  end read only


--  begin read only
   procedure Test_Get_Element_Value (Gnattest_T : in out Test);
   procedure Test_Get_Element_Value_69bad0 (Gnattest_T : in out Test) renames Test_Get_Element_Value;
--  id:2.2/69bad06be5a13405/Get_Element_Value/1/0/
   procedure Test_Get_Element_Value (Gnattest_T : in out Test) is
   --  muxml-utils.ads:52:4:Get_Element_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "parent text"));
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "child text"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent") = "parent text",
              Message   => "Element value mismatch (1)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent/child") = "child text",
              Message   => "Element value mismatch (2)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "//grandchild") = "",
              Message   => "Element value mismatch (3)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "nonexistent") = "",
              Message   => "Element value mismatch (4)");
--  begin read only
   end Test_Get_Element_Value;
--  end read only


--  begin read only
   procedure Test_2_Get_Element (Gnattest_T : in out Test);
   procedure Test_Get_Element_710ca6 (Gnattest_T : in out Test) renames Test_2_Get_Element;
--  id:2.2/710ca65d3e87fd3c/Get_Element/0/0/
   procedure Test_2_Get_Element (Gnattest_T : in out Test) is
   --  muxml-utils.ads:60:4:Get_Element
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = null,
              Message   => "Element in empty list");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = null,
              Message   => "Element in 'bar' list");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = Node,
              Message   => "Element not found");
--  begin read only
   end Test_2_Get_Element;
--  end read only


--  begin read only
   procedure Test_Append (Gnattest_T : in out Test);
   procedure Test_Append_6bcf00 (Gnattest_T : in out Test) renames Test_Append;
--  id:2.2/6bcf005e971aed10/Append/1/0/
   procedure Test_Append (Gnattest_T : in out Test) is
   --  muxml-utils.ads:67:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl        : DOM.Core.DOM_Implementation;
      Data        : XML_Data_Type;
      Node        : DOM.Core.Node;
      Left, Right : DOM.Core.Node_List;
   begin
      Append (Left  => Left,
              Right => Right);
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 0,
              Message   => "Length not zero");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);

      Left := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 1,
              Message   => "Left length not 1:" & DOM.Core.Nodes.Length
                (List => Left)'Img);

      Right := Left;

      Append (Left  => Left,
              Right => Right);
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 2,
              Message   => "Length not 2:" & DOM.Core.Nodes.Length
                (List => Left)'Img);
--  begin read only
   end Test_Append;
--  end read only


--  begin read only
   procedure Test_Append_Child (Gnattest_T : in out Test);
   procedure Test_Append_Child_0b6f31 (Gnattest_T : in out Test) renames Test_Append_Child;
--  id:2.2/0b6f317beeb588d9/Append_Child/1/0/
   procedure Test_Append_Child (Gnattest_T : in out Test) is
   --  muxml-utils.ads:72:4:Append_Child
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "node");
      Child := DOM.Core.Documents.Create_Element (Doc     => Data.Doc,
                                                  Tag_Name => "child");
      Append_Child (Node      => Node,
                    New_Child => Child);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Error appending child");
      Assert (Condition => DOM.Core.Nodes.First_Child (N => Node) = Child,
              Message   => "Child mismatch");
--  begin read only
   end Test_Append_Child;
--  end read only


--  begin read only
   procedure Test_Merge (Gnattest_T : in out Test);
   procedure Test_Merge_64a439 (Gnattest_T : in out Test) renames Test_Merge;
--  id:2.2/64a439c9547e9313/Merge/1/0/
   procedure Test_Merge (Gnattest_T : in out Test) is
   --  muxml-utils.ads:86:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive
      is
         Data : Muxml.XML_Data_Type;
         Impl : DOM.Core.DOM_Implementation;
         Doc  : constant DOM.Core.Document
           := DOM.Core.Create_Document (Implementation => Impl);
         Node, Tmp : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.VCPU_Profile,
                      File => "data/vcpu_profile.xml");

         --  Construct the following XML structure:
         --  <vcpu><segments><cs selector="16#ffff#>text</cs></segments></vcpu>

         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "cs");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "access",
            Value => "16#cafe#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "selector",
            Value => "16#ffff#");
         Append_Child
           (Node      => Node,
            New_Child => DOM.Core.Documents.Create_Text_Node
              (Doc  => Doc,
               Data => "text"));
         Tmp := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "segments");
         Append_Child (Node      => Tmp,
                       New_Child => Node);
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "vcpu");
         Append_Child (Node      => Node,
                       New_Child => Tmp);
         Append_Child
           (Node      => Doc,
            New_Child => Node);

         Assert
           (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "selector") = "16#0008#",
            Message   => "Unexpected cs selector attribute in vcpu policy");

         Merge (Left  => Data.Doc,
                Right => Doc);

         Assert (Condition => Get_Attribute
                 (Doc   => Data.Doc,
                  XPath => "/vcpu/segments/cs",
                  Name  => "access") = "16#cafe#",
                 Message   => "Error merging XML nodes: cs access");
         Assert (Condition => Get_Attribute
                 (Doc   => Data.Doc,
                  XPath => "/vcpu/segments/cs",
                  Name  => "selector") = "16#ffff#",
                 Message   => "Error merging XML nodes: cs selector");
      end Positive;

      ----------------------------------------------------------------------

      procedure Nodes_Name_Mismatch
      is
         Impl : DOM.Core.DOM_Implementation;
         Doc  : constant DOM.Core.Document
           := DOM.Core.Create_Document (Implementation => Impl);
         Node_A, Node_B : DOM.Core.Node;
      begin
         Node_A := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "A");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node_A,
            Name  => "attr",
            Value => "foobar");
         Append_Child (Node      => Doc,
                       New_Child => Node_A);

         Node_B := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "B");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node_B,
            Name  => "attr",
            Value => "16#cafe#");

         Merge (Left  => Node_A,
                Right => Node_B);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Node_A,
                  Name => "attr") = "foobar",
                 Message   => "Node B merged into Node A");
      end Nodes_Name_Mismatch;

      ----------------------------------------------------------------------

      procedure Nodes_With_List
      is
         use Ada.Strings.Unbounded;

         Data : XML_Data_Type;
         Impl : DOM.Core.DOM_Implementation;
         Doc  : constant DOM.Core.Document
           := DOM.Core.Create_Document (Implementation => Impl);
         Node, Tmp, MSRs_Node : DOM.Core.Node;
      begin
         Parse (Data => Data,
                Kind => VCPU_Profile,
                File => "data/vcpu_profile.xml");

         MSRs_Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/vcpu/registers/msrs"),
            Index => 0);

         --  Construct the following XML structure:
         --  <msrs><msr start="16#0174#"/></msrs>

         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "msr");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#0174#");
         Tmp := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "msrs");
         Append_Child (Node      => Tmp,
                       New_Child => Node);

         Merge (Left      => MSRs_Node,
                Right     => Tmp,
                List_Tags => (1 => To_Unbounded_String ("msr")));

         declare
            MSR_Count : constant Natural := DOM.Core.Nodes.Length
              (List => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "/vcpu/registers/msrs/msr"));
         begin
            Assert (Condition => MSR_Count = 3,
                    Message   => "Error merging child element list");
         end;
      end Nodes_With_List;
   begin
      Positive;
      Nodes_Name_Mismatch;
      Nodes_With_List;
--  begin read only
   end Test_Merge;
--  end read only


--  begin read only
   procedure Test_Ancestor_Node (Gnattest_T : in out Test);
   procedure Test_Ancestor_Node_314569 (Gnattest_T : in out Test) renames Test_Ancestor_Node;
--  id:2.2/3145695d1e1d2313/Ancestor_Node/1/0/
   procedure Test_Ancestor_Node (Gnattest_T : in out Test) is
   --  muxml-utils.ads:92:4:Ancestor_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "top");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child1"));
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child2"));

      declare
         use type DOM.Core.Node;

         Ancestor : DOM.Core.Node
           := Ancestor_Node
             (Node  => Node,
              Level => 2);
      begin
         Assert (Condition => DOM.Core.Nodes.Node_Name (N => Ancestor) = "top",
                 Message   => "Ancestor mismatch (1)");

         Ancestor := Ancestor_Node (Node  => Ancestor,
                                    Level => 1);
         Assert (Condition => Ancestor = null,
                 Message   => "Ancestor mismatch (2)");
      end;
--  begin read only
   end Test_Ancestor_Node;
--  end read only


--  begin read only
   procedure Test_Remove_Child (Gnattest_T : in out Test);
   procedure Test_Remove_Child_540ca0 (Gnattest_T : in out Test) renames Test_Remove_Child;
--  id:2.2/540ca0eb2b0d8bd4/Remove_Child/1/0/
   procedure Test_Remove_Child (Gnattest_T : in out Test) is
   --  muxml-utils.ads:100:4:Remove_Child
--  end read only

      pragma Unreferenced (Gnattest_T);

      Node     : DOM.Core.Node;
      Dom_Impl : DOM.Core.DOM_Implementation;
      Doc      : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Dom_Impl);
   begin
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "elem");

      Append_Child (Node      => Doc,
                    New_Child => Node);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Unable to add child to document");

      Remove_Child (Node       => Doc,
                    Child_Name => "elem");

      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Error removing child node");

      begin
         Remove_Child (Node       => Doc,
                       Child_Name => "elem");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to remove child 'elem' from node '#document'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Remove_Child;
--  end read only

end Muxml.Utils.Test_Data.Tests;
