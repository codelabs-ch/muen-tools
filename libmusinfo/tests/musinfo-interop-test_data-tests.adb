--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Interop.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Musinfo.Interop.Test_Data.Tests is


--  begin read only
   procedure Test_Name_To_C (Gnattest_T : in out Test);
   procedure Test_Name_To_C_52e1d1 (Gnattest_T : in out Test) renames Test_Name_To_C;
--  id:2.2/52e1d1e1b9ecb40f/Name_To_C/1/0/
   procedure Test_Name_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:24:4:Name_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Name
              (Name => Utils.Create_Name (Str => Ref_Str)'Address) = 1,
              Message   => "C name mismatch");
--  begin read only
   end Test_Name_To_C;
--  end read only


--  begin read only
   procedure Test_Channel_To_C (Gnattest_T : in out Test);
   procedure Test_Channel_To_C_f10945 (Gnattest_T : in out Test) renames Test_Channel_To_C;
--  id:2.2/f10945751dad53f6/Channel_To_C/1/0/
   procedure Test_Channel_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:27:4:Channel_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Channel
              (Channel => Utils.Create_Channel
               (Name       => Utils.Create_Name (Str => Ref_Str),
                Address    => 16#dead_beef_cafe_feed#,
                Size       => 16#8080_abab_cdcd_9090#,
                Writable   => True,
                Has_Event  => True,
                Has_Vector => True,
                Event      => 128,
                Vector     => 255)'Address) = 1,
              Message   => "C channel mismatch");
--  begin read only
   end Test_Channel_To_C;
--  end read only


--  begin read only
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test);
   procedure Test_Subject_Info_To_C_3471da (Gnattest_T : in out Test) renames Test_Subject_Info_To_C;
--  id:2.2/3471dabca4420d92/Subject_Info_To_C/1/0/
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:30:4:Subject_Info_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
      Channel : constant Channel_Type             := Utils.Create_Channel
        (Name       => Utils.Create_Name (Str => Ref_Str),
         Address    => 16#dead_beef_cafe_feed#,
         Size       => 16#8080_abab_cdcd_9090#,
         Writable   => True,
         Has_Event  => True,
         Has_Vector => True,
         Event      => 128,
         Vector     => 255);
      Info    : Subject_Info_Type := Null_Subject_Info;
   begin
      for I in Channel_Index_Type loop
         Utils.Append_Channel (Info    => Info,
                               Channel => Channel);
      end loop;

      Assert (Condition => C_Imports.C_Assert_Subject_Info
              (Info => Info'Address) = 1,
              Message   => "C subject info mismatch");
--  begin read only
   end Test_Subject_Info_To_C;
--  end read only


--  begin read only
   procedure Test_Check_Name_Type (Gnattest_T : in out Test);
   procedure Test_Check_Name_Type_3e54f1 (Gnattest_T : in out Test) renames Test_Check_Name_Type;
--  id:2.2/3e54f1454c3de673/Check_Name_Type/1/0/
   procedure Test_Check_Name_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:33:4:Check_Name_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Dummy : Name_Type;
   begin

      Assert (Condition => C_Imports.C_Assert_Name_Type
              (Size          => Name_Type'Size / 8,
               Alignment     => Name_Type'Alignment,
               Length_Offset => Dummy.Length'Bit_Position / 8,
               Data_Offset   => Dummy.Data'Bit_Position / 8) = 1,
              Message   => "C name type mismatch");

--  begin read only
   end Test_Check_Name_Type;
--  end read only


--  begin read only
   procedure Test_Check_Channel_Type (Gnattest_T : in out Test);
   procedure Test_Check_Channel_Type_451002 (Gnattest_T : in out Test) renames Test_Check_Channel_Type;
--  id:2.2/451002ebfab2ca67/Check_Channel_Type/1/0/
   procedure Test_Check_Channel_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:36:4:Check_Channel_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Dummy : Channel_Type;
   begin

      Assert (Condition => C_Imports.C_Assert_Channel_Type
              (Size           => Channel_Type'Size / 8,
               Alignment      => Channel_Type'Alignment,
               Name_Offset    => Dummy.Name'Bit_Position / 8,
               Address_Offset => Dummy.Address'Bit_Position / 8,
               Size_Offset    => Dummy.Size'Bit_Position / 8,
               Flags_Offset   => Dummy.Flags'Bit_Position / 8,
               Event_Offset   => Dummy.Event'Bit_Position / 8,
               Vector_Offset  => Dummy.Vector'Bit_Position / 8) = 1,
              Message   => "C channel type mismatch");

--  begin read only
   end Test_Check_Channel_Type;
--  end read only


--  begin read only
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test);
   procedure Test_Check_Subject_Info_Type_659906 (Gnattest_T : in out Test) renames Test_Check_Subject_Info_Type;
--  id:2.2/659906a031093bd7/Check_Subject_Info_Type/1/0/
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:39:4:Check_Subject_Info_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Dummy : Subject_Info_Type;
   begin

      Assert (Condition => C_Imports.C_Assert_Subject_Info_Type
              (Size                 => Subject_Info_Type'Size / 8,
               Alignment            => Subject_Info_Type'Alignment,
               Magic_Offset         => Dummy.Magic'Bit_Position / 8,
               Channel_Count_Offset => Dummy.Channel_Count'Bit_Position / 8,
               Channels_Offset      => Dummy.Channels'Bit_Position / 8) = 1,
              Message   => "C subject info type mismatch");


--  begin read only
   end Test_Check_Subject_Info_Type;
--  end read only

end Musinfo.Interop.Test_Data.Tests;
