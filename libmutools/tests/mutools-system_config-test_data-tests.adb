--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.System_Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.System_Config.Test_Data.Tests is


--  begin read only
   procedure Test_Has_Boolean (Gnattest_T : in out Test);
   procedure Test_Has_Boolean_89c551 (Gnattest_T : in out Test) renames Test_Has_Boolean;
--  id:2.2/89c551e2c6b63d90/Has_Boolean/1/0/
   procedure Test_Has_Boolean (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:25:4:Has_Boolean
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Boolean (Data => Policy,
                                        Name => "iommu_enabled"),
              Message   => "Boolean config variable not present");
      Assert (Condition => not Has_Boolean (Data => Policy,
                                            Name => "nonexistent"),
              Message   => "Boolean config variable present (non-existent)");
      Assert (Condition => not Has_Boolean (Data => Policy,
                                            Name => "session_count"),
              Message   => "Boolean config variable present (type mismatch)");
--  begin read only
   end Test_Has_Boolean;
--  end read only


--  begin read only
   procedure Test_Has_Integer (Gnattest_T : in out Test);
   procedure Test_Has_Integer_0bbd6e (Gnattest_T : in out Test) renames Test_Has_Integer;
--  id:2.2/0bbd6e7b6d7c7489/Has_Integer/1/0/
   procedure Test_Has_Integer (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:31:4:Has_Integer
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Integer (Data => Policy,
                                        Name => "session_count"),
              Message   => "Integer config variable not present");
      Assert (Condition => not Has_Integer (Data => Policy,
                                            Name => "nonexistent"),
              Message   => "Integer config variable present (non-existent)");
      Assert (Condition => not Has_Integer (Data => Policy,
                                            Name => "iommu_enabled"),
              Message   => "Integer config variable present (type mismatch)");
--  begin read only
   end Test_Has_Integer;
--  end read only


--  begin read only
   procedure Test_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_d13e21 (Gnattest_T : in out Test) renames Test_Get_Value;
--  id:2.2/d13e2143a0c1f788/Get_Value/1/0/
   procedure Test_Get_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:38:4:Get_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "iommu_enabled"),
              Message   => "Boolean config value false");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/config/boolean[@name=""iommu_enabled""]",
         Name  => "value",
         Value => "false");
      Assert (Condition => not Get_Value (Data => Policy,
                                      Name => "iommu_enabled"),
              Message   => "Boolean config value true");

      begin
         declare
            Dummy : constant Boolean := Get_Value (Data => Policy,
                                                   Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No boolean config option 'nonexistent' found",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Get_Value;
--  end read only

end Mutools.System_Config.Test_Data.Tests;