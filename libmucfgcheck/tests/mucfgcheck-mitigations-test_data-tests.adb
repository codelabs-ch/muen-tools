--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Mitigations.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Muxml;
with Muxml.Utils;
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Mucfgcheck.Mitigations.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Check_Implementation (Gnattest_T : in out Test);
   procedure Test_Check_Implementation_40f9d0 (Gnattest_T : in out Test) renames Test_Check_Implementation;
--  id:2.2/40f9d0c37265402f/Check_Implementation/1/0/
   procedure Test_Check_Implementation (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test: the mitigation policy is disabled (enable="false"),
      --  so nothing is checked or enforced.

      Check_Implementation (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Enabled mitigation sequences are not implemented by the kernel.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/x86_64/mitigations",
         Name  => "enable",
         Value => "true");

      Check_Implementation (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Mitigation 'eibrs' is enabled but not implemented "
               & "for the x86_64 target kernel"),
              Message   => "Exception mismatch (eibrs)");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Mitigation 'ibpb' is enabled but not implemented "
               & "for the x86_64 target kernel"),
              Message   => "Exception mismatch (ibpb)");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Mitigation 'rsbStuffing' (mode 'singleCall') is "
               & "enabled but not implemented for the x86_64 target kernel"),
              Message   => "Exception mismatch (rsbStuffing)");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Mitigation 'bhbLoop' is enabled but not implemented "
               & "for the x86_64 target kernel"),
              Message   => "Exception mismatch (bhbLoop)");

      --  Re-disabling the policy clears the errors.

      Validation_Errors.Clear;
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/x86_64/mitigations",
         Name  => "enable",
         Value => "false");

      Check_Implementation (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error after re-disabling policy");
--  begin read only
   end Test_Check_Implementation;
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
end Mucfgcheck.Mitigations.Test_Data.Tests;
