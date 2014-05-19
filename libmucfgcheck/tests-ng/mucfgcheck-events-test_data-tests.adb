--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Events.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Events.Test_Data.Tests is


--  begin read only
   procedure Test_Source_Targets (Gnattest_T : in out Test);
   procedure Test_Source_Targets_dd485f (Gnattest_T : in out Test) renames Test_Source_Targets;
--  id:2.2/dd485fd3b78efbdb/Source_Targets/1/0/
   procedure Test_Source_Targets (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:26:4:Source_Targets
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/target/"
            & "event[@physical='trap_to_sm']/..");
      begin
         Muxml.Utils.Remove_Child
           (Node       => Node,
            Child_Name => "event");

         Source_Targets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of targets for event 'trap_to_sm': 0",
                    Message   => "Exception mismatch (target)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "name",
         Value => "new_event");

      begin
         Source_Targets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of sources for event 'new_event': 0",
                    Message   => "Exception mismatch (source)");
      end;
--  begin read only
   end Test_Source_Targets;
--  end read only


--  begin read only
   procedure Test_Subject_Event_References (Gnattest_T : in out Test);
   procedure Test_Subject_Event_References_0768ea (Gnattest_T : in out Test) renames Test_Subject_Event_References;
--  id:2.2/0768eab62525b03d/Subject_Event_References/1/0/
   procedure Test_Subject_Event_References (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:29:4:Subject_Event_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/"
         & "event[@physical='trap_to_sm']",
         Name  => "physical",
         Value => "nonexistent_dst");

      begin
         Subject_Event_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'nonexistent_dst' referenced by subject 'sm' does"
                    & " not exist",
                    Message   => "Exception mismatch (target)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event/notify[@physical='resume_linux']",
         Name  => "physical",
         Value => "nonexistent_src");

      begin
         Subject_Event_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'nonexistent_src' referenced by subject 'sm' does"
                    & " not exist",
                    Message   => "Exception mismatch (source)");
      end;
--  begin read only
   end Test_Subject_Event_References;
--  end read only


--  begin read only
   procedure Test_Self_References (Gnattest_T : in out Test);
   procedure Test_Self_References_af5859 (Gnattest_T : in out Test) renames Test_Self_References;
--  id:2.2/af5859813505ea74/Self_References/1/0/
   procedure Test_Self_References (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:33:4:Self_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='linux_console']",
         Name  => "physical",
         Value => "linux_keyboard");

      begin
         Self_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to self in event 'linux_keyboard' of subject "
                    & "'vt'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Self_References;
--  end read only


--  begin read only
   procedure Test_Switch_Same_Core (Gnattest_T : in out Test);
   procedure Test_Switch_Same_Core_9bc636 (Gnattest_T : in out Test) renames Test_Switch_Same_Core;
--  id:2.2/9bc636b0bd4cd54e/Switch_Same_Core/1/0/
   procedure Test_Switch_Same_Core (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:37:4:Switch_Same_Core
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event/"
         & "notify[@physical='linux_keyboard']",
         Name  => "physical",
         Value => "resume_linux");

      begin
         Switch_Same_Core (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 1) in subject's 'vt' "
                    & "(CPU 0) switch notification 'resume_linux' invalid - "
                    & "must run on the same CPU",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Switch_Same_Core;
--  end read only


--  begin read only
   procedure Test_IPI_Different_Core (Gnattest_T : in out Test);
   procedure Test_IPI_Different_Core_c8a75b (Gnattest_T : in out Test) renames Test_IPI_Different_Core;
--  id:2.2/c8a75bb306ee763d/IPI_Different_Core/1/0/
   procedure Test_IPI_Different_Core (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:41:4:IPI_Different_Core
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "mode",
         Value => "ipi");

      begin
         IPI_Different_Core (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'sm' (CPU 1) in subject's 'linux' "
                    & "(CPU 1) ipi notification 'trap_to_sm' invalid - no IPI"
                    & " allowed",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IPI_Different_Core;
--  end read only


--  begin read only
   procedure Test_Source_Group_Event_ID_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Source_Group_Event_ID_Uniqueness_0d6e56 (Gnattest_T : in out Test) renames Test_Source_Group_Event_ID_Uniqueness;
--  id:2.2/0d6e56c19519f6f3/Source_Group_Event_ID_Uniqueness/1/0/
   procedure Test_Source_Group_Event_ID_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:44:4:Source_Group_Event_ID_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event"
         & "[@logical='resume_linux']",
         Name  => "id",
         Value => "1");

      begin
         Source_Group_Event_ID_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'sm' source events 'resume_linux' and "
                    & "'channel_event_sm_console' share ID 1",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Source_Group_Event_ID_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Source_Group_Event_ID_Validity (Gnattest_T : in out Test);
   procedure Test_Source_Group_Event_ID_Validity_ed9d9b (Gnattest_T : in out Test) renames Test_Source_Group_Event_ID_Validity;
--  id:2.2/ed9d9bbe36269c5a/Source_Group_Event_ID_Validity/1/0/
   procedure Test_Source_Group_Event_ID_Validity (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:47:4:Source_Group_Event_ID_Validity
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Source_Group_Event_ID_Validity;
--  end read only


--  begin read only
   procedure Test_Get_Max_ID (Gnattest_T : in out Test);
   procedure Test_Get_Max_ID_a65afa (Gnattest_T : in out Test) renames Test_Get_Max_ID;
--  id:2.2/a65afae2a79d6438/Get_Max_ID/1/0/
   procedure Test_Get_Max_ID (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:50:4:Get_Max_ID
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Max_ID;
--  end read only


--  begin read only
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Event_ID_2d339d (Gnattest_T : in out Test) renames Test_Is_Valid_Event_ID;
--  id:2.2/2d339dda9942d861/Is_Valid_Event_ID/1/0/
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test) is
   --  mucfgcheck-events.ads:54:4:Is_Valid_Event_ID
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Is_Valid_Event_ID;
--  end read only

end Mucfgcheck.Events.Test_Data.Tests;
