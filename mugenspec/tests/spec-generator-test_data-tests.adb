--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Spec.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-generator.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sched_Spec  : constant String := "skp-scheduling.ads";
      Intr_Spec   : constant String := "skp-interrupts.ads";
      Kernel_Spec : constant String := "skp-kernel.ads";
      Kernel_H    : constant String := "policy.h";
      Subj_Spec_H : constant String := "skp-subjects.ads";
      Subj_Spec_B : constant String := "skp-subjects.adb";
      Skp_Spec    : constant String := "skp.ads";
      HW_Spec     : constant String := "skp-hardware.ads";
      IOMMU_Spec  : constant String := "skp-iommu.ads";
      Evts_Spec_H : constant String := "skp-events.ads";
      Evts_Spec_B : constant String := "skp-events.adb";
      Policy_GPR  : constant String := "policy.gpr";

      ----------------------------------------------------------------------

      procedure Write_Specs
      is
         Policy     : Muxml.XML_Data_Type;
         Output_Dir : constant String := "obj/test-generator-write-specs/";
      begin
         if not Ada.Directories.Exists (Name => Output_Dir) then
            Ada.Directories.Create_Directory (New_Directory => Output_Dir);
         end if;

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);

         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Sched_Spec),
                 Message   => "Scheduling spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Intr_Spec),
                 Message   => "Interrupt spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Kernel_Spec),
                 Message   => "Kernel spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Kernel_H),
                 Message   => "Kernel policy.h missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Subj_Spec_H),
                 Message   => "Subject spec header missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Subj_Spec_B),
                 Message   => "Subject spec body missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Skp_Spec),
                 Message   => "Top-level spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & HW_Spec),
                 Message   => "Hardware spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & IOMMU_Spec),
                 Message   => "IOMMU spec missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Evts_Spec_H),
                 Message   => "Events spec header missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Evts_Spec_B),
                 Message   => "Events spec body missing");
         Assert (Condition => Ada.Directories.Exists
                 (Name => Output_Dir & Policy_GPR),
                 Message   => "Policy GPR missing");

         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end Write_Specs;

      ----------------------------------------------------------------------

      procedure Write_No_IOMMUs
      is
         Policy     : Muxml.XML_Data_Type;
         Output_Dir : constant String := "obj/test-generator-write-no-iommus/";
      begin
         if not Ada.Directories.Exists (Name => Output_Dir) then
            Ada.Directories.Create_Directory (New_Directory => Output_Dir);
         end if;

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/config/boolean[@name='iommu_enabled']",
            Name  => "value",
            Value => "false");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Output_Dir & IOMMU_Spec,
                  Filename2 => "data/skp-iommu_noiommus_ads.ref"),
                 Message   => "Empty IOMMU spec mismatch");

         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end Write_No_IOMMUs;
   begin
      Write_Specs;
      Write_No_IOMMUs;
--  begin read only
   end Test_Write;
--  end read only

end Spec.Generator.Test_Data.Tests;
