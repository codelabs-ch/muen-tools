--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Hardware.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Spec.Skp_Hardware.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-skp_hardware.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy     : Muxml.XML_Data_Type;
      Output_Dir : constant String := "obj";
      Spec       : constant String := Output_Dir & "/skp-hardware.ads";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec,
               Filename2 => "data/skp-hardware.ads"),
              Message   => "Hardware spec mismatch");
      Ada.Directories.Delete_File (Name => Spec);
--  begin read only
   end Test_Write;
--  end read only

end Spec.Skp_Hardware.Test_Data.Tests;