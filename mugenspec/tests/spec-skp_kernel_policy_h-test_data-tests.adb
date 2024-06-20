--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Kernel_Policy_H.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Spec.Skp_Kernel_Policy_H.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Kernel_Spec : constant String := "obj/skp-kernel.ads";
   begin
      Write_X86_64 :
      declare
         Policy     : Muxml.XML_Data_Type;
         Kernel_Hdr : constant String := "obj/policy.h";
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/skp-kernel.ads",
                  Filename2 => Kernel_Spec),
                 Message   => "Skp.Kernel spec mismatch (x86/64)");
         Ada.Directories.Delete_File (Name => Kernel_Spec);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/policy.h.ref",
                  Filename2 => Kernel_Hdr),
                 Message   => "policy.h mismatch (x86/64)");
         Ada.Directories.Delete_File (Name => Kernel_Hdr);
      end Write_X86_64;

      Write_ARMv8a :
      declare
         Policy     : Muxml.XML_Data_Type;
         Kernel_Hdr : constant String := "obj/policy.S";
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy-armv8a.xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/skp-kernel-armv8a.ref",
                  Filename2 => Kernel_Spec),
                 Message   => "Skp.Kernel spec mismatch (ARMv8a)");
         Ada.Directories.Delete_File (Name => Kernel_Spec);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/policy-armv8a.S.ref",
                  Filename2 => Kernel_Hdr),
                 Message   => "policy.S mismatch (ARMv8a)");
         Ada.Directories.Delete_File (Name => Kernel_Hdr);
      end Write_ARMv8a;

--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Write_ARMv8a (Gnattest_T : in out Test);
   procedure Test_Write_ARMv8a_aa89b6 (Gnattest_T : in out Test) renames Test_Write_ARMv8a;
--  id:2.2/aa89b6b5d2a505b7/Write_ARMv8a/1/0/
   procedure Test_Write_ARMv8a (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy      : Muxml.XML_Data_Type;
      Kernel_Spec : constant String := "obj/skp-kernel.ads";
      Kernel_Hdr  : constant String := "obj/policy.S";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy-armv8a.xml");

      Write_ARMv8a (Output_Dir => "obj",
                    Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/skp-kernel-armv8a.ref",
               Filename2 => Kernel_Spec),
              Message   => "Skp.Kernel spec mismatch (ARMv8a)");
      Ada.Directories.Delete_File (Name => Kernel_Spec);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/policy-armv8a.S.ref",
               Filename2 => Kernel_Hdr),
              Message   => "policy.S mismatch (ARMv8a)");
      Ada.Directories.Delete_File (Name => Kernel_Hdr);
--  begin read only
   end Test_Write_ARMv8a;
--  end read only


--  begin read only
   procedure Test_Write_X86_64 (Gnattest_T : in out Test);
   procedure Test_Write_X86_64_93a87b (Gnattest_T : in out Test) renames Test_Write_X86_64;
--  id:2.2/93a87b7e3333f8da/Write_X86_64/1/0/
   procedure Test_Write_X86_64 (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy      : Muxml.XML_Data_Type;
      Kernel_Spec : constant String := "obj/skp-kernel.ads";
      Kernel_Hdr  : constant String := "obj/policy.h";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write_X86_64 (Output_Dir => "obj",
                    Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/skp-kernel.ads",
               Filename2 => Kernel_Spec),
              Message   => "Skp.Kernel spec mismatch (x86/64)");
      Ada.Directories.Delete_File (Name => Kernel_Spec);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/policy.h.ref",
               Filename2 => Kernel_Hdr),
              Message   => "policy.h mismatch (x86/64)");
      Ada.Directories.Delete_File (Name => Kernel_Hdr);
--  begin read only
   end Test_Write_X86_64;
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
end Spec.Skp_Kernel_Policy_H.Test_Data.Tests;
