--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Subject.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Subject.Test_Data.Tests is


--  begin read only
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Name_Uniqueness_7f1559 (Gnattest_T : in out Test) renames Test_Name_Uniqueness;
--  id:2.2/7f15594730cfb6f0/Name_Uniqueness/1/0/
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:25:4:Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']",
         Name  => "name",
         Value => "linux");

      begin
         Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects with id 1 and 4 have identical name 'linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_CPU_ID (Gnattest_T : in out Test);
   procedure Test_CPU_ID_40ceb3 (Gnattest_T : in out Test) renames Test_CPU_ID;
--  id:2.2/40ceb37195f33af8/CPU_ID/1/0/
   procedure Test_CPU_ID (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:28:4:CPU_ID
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "7");

      begin
         CPU_ID (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'cpu => 7' of 'linux' subject element not in "
                    & "valid range 0 .. 3",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_ID;
--  end read only


--  begin read only
   procedure Test_Memory_Types (Gnattest_T : in out Test);
   procedure Test_Memory_Types_0a6ddc (Gnattest_T : in out Test) renames Test_Memory_Types;
--  id:2.2/0a6ddce5b8b8256a/Memory_Types/1/0/
   procedure Test_Memory_Types (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:31:4:Memory_Types
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "system_pt");

      begin
         Memory_Types (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical memory region 'binary' of subject 'vt' mapping "
                    & "physical region 'vt|bin' has invalid type system_pt",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Types;
--  end read only


--  begin read only
   procedure Test_No_IOMMU_Device_References (Gnattest_T : in out Test);
   procedure Test_No_IOMMU_Device_References_c1578b (Gnattest_T : in out Test) renames Test_No_IOMMU_Device_References;
--  id:2.2/c1578b1a998ee62a/No_IOMMU_Device_References/1/0/
   procedure Test_No_IOMMU_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:34:4:No_IOMMU_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='wireless']",
         Name  => "physical",
         Value => "iommu_1");

      begin
         No_IOMMU_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device referenced by subject 'vt'",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='ethernet']",
         Name  => "physical",
         Value => "iommu_2");

      begin
         No_IOMMU_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device referenced by subjects 'vt', 'linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_No_IOMMU_Device_References;
--  end read only

end Mucfgcheck.Subject.Test_Data.Tests;
