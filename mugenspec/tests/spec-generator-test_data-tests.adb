--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-generator.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sched_Spec  : constant String := "obj/skp-scheduling.ads";
      Intr_Spec   : constant String := "obj/skp-interrupts.ads";
      Kernel_Spec : constant String := "obj/skp-kernel.ads";
      Kernel_H    : constant String := "obj/policy.h";
      Subj_Spec_H : constant String := "obj/skp-subjects.ads";
      Subj_Spec_B : constant String := "obj/skp-subjects.adb";
      Skp_Spec    : constant String := "obj/skp.ads";
      HW_Spec     : constant String := "obj/skp-hardware.ads";
      IOMMU_Spec  : constant String := "obj/skp-iommu.ads";
      Policy_GPR  : constant String := "obj/policy.gpr";

      ----------------------------------------------------------------------

      procedure Write_Specs
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => Ada.Directories.Exists (Name => Sched_Spec),
                 Message   => "Scheduling spec missing");
         Ada.Directories.Delete_File (Name => Sched_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => Intr_Spec),
                 Message   => "Interrupt spec missing");
         Ada.Directories.Delete_File (Name => Intr_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => Kernel_Spec),
                 Message   => "Kernel spec missing");
         Ada.Directories.Delete_File (Name => Kernel_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => Kernel_H),
                 Message   => "Kernel policy.h missing");
         Ada.Directories.Delete_File (Name => Kernel_H);

         Assert (Condition => Ada.Directories.Exists (Name => Subj_Spec_H),
                 Message   => "Subject spec header missing");
         Ada.Directories.Delete_File (Name => Subj_Spec_H);
         Assert (Condition => Ada.Directories.Exists (Name => Subj_Spec_B),
                 Message   => "Subject spec body missing");
         Ada.Directories.Delete_File (Name => Subj_Spec_B);

         Assert (Condition => Ada.Directories.Exists (Name => Skp_Spec),
                 Message   => "Top-level spec missing");
         Ada.Directories.Delete_File (Name => Skp_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => HW_Spec),
                 Message   => "Hardware spec missing");
         Ada.Directories.Delete_File (Name => HW_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => IOMMU_Spec),
                 Message   => "IOMMU spec missing");
         Ada.Directories.Delete_File (Name => IOMMU_Spec);

         Assert (Condition => Ada.Directories.Exists (Name => Policy_GPR),
                 Message   => "Policy GPR missing");
         Ada.Directories.Delete_File (Name => Policy_GPR);
      end Write_Specs;

      ----------------------------------------------------------------------

      procedure Write_No_IOMMUs
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/features/iommu",
            Name  => "enabled",
            Value => "false");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Ada.Directories.Delete_File (Name => Sched_Spec);
         Ada.Directories.Delete_File (Name => Intr_Spec);
         Ada.Directories.Delete_File (Name => Kernel_Spec);
         Ada.Directories.Delete_File (Name => Kernel_H);
         Ada.Directories.Delete_File (Name => Subj_Spec_H);
         Ada.Directories.Delete_File (Name => Subj_Spec_B);
         Ada.Directories.Delete_File (Name => Skp_Spec);
         Ada.Directories.Delete_File (Name => HW_Spec);
         Ada.Directories.Delete_File (Name => Policy_GPR);

         Assert (Condition => not Ada.Directories.Exists (Name => IOMMU_Spec),
                 Message   => "IOMMU spec exists");
      end Write_No_IOMMUs;
   begin
      Write_Specs;
      Write_No_IOMMUs;
--  begin read only
   end Test_Write;
--  end read only

end Spec.Generator.Test_Data.Tests;
