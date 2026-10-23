--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Mitigations.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;
with Ada.Strings.Fixed;
with Muxml;
with Muxml.Utils;
with Test_Utils;
--  begin read only
--  end read only
package body Spec.Skp_Mitigations.Test_Data.Tests is

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

      Policy     : Muxml.XML_Data_Type;
      Output_Dir : constant String := "obj/test-mitigations-spec-write";
      Path       : constant String := Output_Dir & "/skp-mitigations.ads";
   begin
      if not Ada.Directories.Exists (Name => Output_Dir) then
         Ada.Directories.Create_Directory (New_Directory => Output_Dir);
      end if;

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      declare
         Content : constant String := Test_Utils.Read_File (Filename => Path);
      begin
         Assert
           (Condition => Ada.Strings.Fixed.Index
            (Source  => Content,
             Pattern => "Min_MCU_Revision : constant := 16#14#;") > 0,
            Message   => "Min revision wrong or missing");
         Assert
           (Condition => Ada.Strings.Fixed.Index
            (Source  => Content,
             Pattern => "Arch_Cap_Mask     : constant := 16#0001#;") > 0,
            Message   => "Arch cap mask wrong or missing");
         Assert
           (Condition => Ada.Strings.Fixed.Index
            (Source  => Content,
             Pattern => "Arch_Cap_Expected : constant := 16#0001#;") > 0,
            Message   => "Arch cap expected wrong or missing");
      end;

      --  A disabled mitigation policy does not emit the spec.

      Ada.Directories.Delete_File (Name => Path);
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/processor/x86_64/mitigations",
         Name  => "enable",
         Value => "false");
      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      Assert (Condition => not Ada.Directories.Exists (Name => Path),
              Message   => "Mitigation spec written for disabled policy");

      Ada.Directories.Delete_Tree (Directory => Output_Dir);
--  begin read only
   end Test_Write;
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
end Spec.Skp_Mitigations.Test_Data.Tests;
