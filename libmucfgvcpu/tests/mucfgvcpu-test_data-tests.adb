--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgvcpu.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mucfgvcpu.Test_Data.Tests is


--  begin read only
   procedure Test_Set_VCPU_Profile (Gnattest_T : in out Test);
   procedure Test_Set_VCPU_Profile_d4ed23 (Gnattest_T : in out Test) renames Test_Set_VCPU_Profile;
--  id:2.2/d4ed23397aebc638/Set_VCPU_Profile/1/0/
   procedure Test_Set_VCPU_Profile (Gnattest_T : in out Test) is
   --  mucfgvcpu.ads:27:4:Set_VCPU_Profile
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_User_VCPU_Profile
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/user_profile.xml");
         Set_VCPU_Profile
           (Profile => VM,
            Node    => DOM.Core.Documents.Get_Element (Doc => Data.Doc));

         Muxml.Write (Data => Data,
                      Kind => Muxml.VCPU_Profile,
                      File => "obj/merged_user_profile.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/merged_user_profile.xml",
                  Filename2 => "obj/merged_user_profile.xml"),
                 Message   => "Merged VCPU profile differs");
         Ada.Directories.Delete_File (Name => "obj/merged_user_profile.xml");
      end Merge_User_VCPU_Profile;

      ----------------------------------------------------------------------

      procedure Set_VCPU_Profile
      is
         Data : Muxml.XML_Data_Type;
         Impl : DOM.Core.DOM_Implementation;
         Node : DOM.Core.Node;
      begin
         Data.Doc := DOM.Core.Create_Document
           (Implementation => Impl);
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "vcpu");
         Muxml.Utils.Append_Child
           (Node      => Data.Doc,
            New_Child => Node);
         Set_VCPU_Profile
           (Profile => VM,
            Node    => Node);

         Muxml.Write (Data => Data,
                      Kind => Muxml.VCPU_Profile,
                      File => "obj/vcpu_profile_vm.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/vcpu_profile_vm.xml",
                  Filename2 => "obj/vcpu_profile_vm.xml"),
                 Message   => "VCPU profile differs");
         Ada.Directories.Delete_File (Name => "obj/vcpu_profile_vm.xml");
      end Set_VCPU_Profile;
   begin
      Set_VCPU_Profile;
      Merge_User_VCPU_Profile;
--  begin read only
   end Test_Set_VCPU_Profile;
--  end read only

end Mucfgvcpu.Test_Data.Tests;
