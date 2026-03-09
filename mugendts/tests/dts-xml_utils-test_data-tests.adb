--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DTS.XML_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Muxml.Utils;
--  begin read only
--  end read only
package body DTS.XML_Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_UART_Count (Gnattest_T : in out Test);
   procedure Test_Get_UART_Count_398d51 (Gnattest_T : in out Test) renames Test_Get_UART_Count;
--  id:2.2/398d51006f46c861/Get_UART_Count/1/0/
   procedure Test_Get_UART_Count (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy  : Muxml.XML_Data_Type;
      Subject : DOM.Core.Node;
      Count   : Natural;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_light.xml");

      Subject := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@globalId='0']");

      Count := Get_UART_Count 
        (Policy  => Policy,
         Subject => Subject);
      Assert (Condition => Count = 1,
              Message   => "UART count mismatch (1):" & Count'Img);

      Muxml.Utils.Remove_Child
        (Node       => Subject,
         Child_Name => "devices");
      Count := Get_UART_Count 
        (Policy  => Policy,
         Subject => Subject);
      Assert (Condition => Count = 0,
              Message   => "UART count mismatch (2):" & Count'Img);
--  begin read only
   end Test_Get_UART_Count;
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
end DTS.XML_Utils.Test_Data.Tests;
