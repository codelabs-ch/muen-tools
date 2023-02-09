--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Conditionals.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;
with McKae.XML.XPath.XIA;
with DOM.Core.Nodes;
with Mutools.Expressions;
--  begin read only
--  end read only
package body Mutools.Conditionals.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Expand (Gnattest_T : in out Test);
   procedure Test_Expand_4a19b8 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/4a19b878eb07fa84/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Conditionals
      is
         Output    : constant String := "obj/config_no_conditionals.xml";
         Data      : Muxml.XML_Data_Type;
         Node      : DOM.Core.Node;
         Node_List :  DOM.Core.Node_List;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         -- remove all IF-nodes
         -- the loop is necessary because nested IFs prevent usage of
         -- functions such as Muxml.Utils.Remove_Elements
         loop
            Node_List := McKae.XML.XPath.XIA.XPath_Query
               (N     => Data.Doc,
                XPath => "//if");
            exit when DOM.Core.Nodes.Length (List => Node_List) = 0;

            Node := DOM.Core.Nodes.Item
               (List  => Node_List,
                Index => 0);
            Node := DOM.Core.Nodes.Remove_Child
               (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                Old_Child => Node);
            DOM.Core.Nodes.Free (N => Node);
         end loop;

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/config_no_conditionals.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Conditionals;

      ----------------------------------------------------------------------

      procedure Positive_Test (Debug_Active : Boolean)
      is
         Output : constant String := "obj/output_test_policy_src_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src_conditionals.xml");

         Expand (Policy => Data, Debug_Active => Debug_Active);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_test_policy_src_conditionals.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Not_A_Boolean
      is
         Output : constant String := "obj/output_test_policy_src_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src_conditionals.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/memory/if[@variable='feature_enabled']",
             Name  => "value",
             Value => "1");
         Expand (Policy => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Expressions.Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Cannot compare value '1' to variable 'feature_enabled'"
                       & " which is a Boolean (cast failed)",
                    Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
      end Not_A_Boolean;

      ----------------------------------------------------------------------

      procedure Not_An_Integer
      is
         Output : constant String := "obj/output_test_policy_src_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src_conditionals.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/memory/if[@variable='session_count']",
             Name  => "value",
             Value => "one");
         Expand (Policy => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Expressions.Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Cannot compare value 'one' to variable 'session_count'"
                       & " which is an Integer (cast failed)",
                    Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
      end Not_An_Integer;

      ----------------------------------------------------------------------

      procedure Var_Not_Found
      is
         Output : constant String := "obj/output_test_policy_src_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src_conditionals.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/memory/if[@variable='session_count']",
             Name  => "variable",
             Value => "no_such_var");
         Expand (Policy => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Expressions.Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Cannot find variable with name 'no_such_var'"
                       & " in configuration",
                    Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
      end Var_Not_Found;
   begin
      Positive_Test (Debug_Active => False);
      Positive_Test (Debug_Active => True);
      No_Conditionals;
      Not_A_Boolean;
      Not_An_Integer;
      Var_Not_Found;

--  begin read only
   end Test_Expand;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutools.Conditionals.Test_Data.Tests;
