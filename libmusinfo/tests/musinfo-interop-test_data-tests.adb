--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Interop.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Musinfo.Interop.Test_Data.Tests is


--  begin read only
   procedure Test_Name_To_C (Gnattest_T : in out Test);
   procedure Test_Name_To_C_52e1d1 (Gnattest_T : in out Test) renames Test_Name_To_C;
--  id:2.2/52e1d1e1b9ecb40f/Name_To_C/1/0/
   procedure Test_Name_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:34:4:Name_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Name
              (Name => Utils.Create_Name (Str => Ref_Str)'Address) = 1,
              Message   => "C name mismatch");
--  begin read only
   end Test_Name_To_C;
--  end read only


--  begin read only
   procedure Test_Memregion_To_C (Gnattest_T : in out Test);
   procedure Test_Memregion_To_C_0eef56 (Gnattest_T : in out Test) renames Test_Memregion_To_C;
--  id:2.2/0eef565aa0cc57a2/Memregion_To_C/1/0/
   procedure Test_Memregion_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:37:4:Memregion_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => C_Imports.C_Assert_Memregion
              (Memregion => Utils.Create_Memregion
               (Address    => 16#dead_beef_cafe_feed#,
                Size       => 16#8080_abab_cdcd_9090#,
                Writable   => True,
                Executable => True)'Address) = 1,
              Message   => "C memregion mismatch");
--  begin read only
   end Test_Memregion_To_C;
--  end read only


--  begin read only
   procedure Test_Channel_To_C (Gnattest_T : in out Test);
   procedure Test_Channel_To_C_f10945 (Gnattest_T : in out Test) renames Test_Channel_To_C;
--  id:2.2/f10945751dad53f6/Channel_To_C/1/0/
   procedure Test_Channel_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:40:4:Channel_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Channel_Info
              (Channel_Info => Utils.Create_Channel_Info
               (Has_Event  => True,
                Has_Vector => True,
                Event      => 128,
                Vector     => 255)'Address) = 1,
              Message   => "C channel mismatch");
--  begin read only
   end Test_Channel_To_C;
--  end read only


--  begin read only
   procedure Test_Resource_To_C (Gnattest_T : in out Test);
   procedure Test_Resource_To_C_c62103 (Gnattest_T : in out Test) renames Test_Resource_To_C;
--  id:2.2/c6210377fb6885ae/Resource_To_C/1/0/
   procedure Test_Resource_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:43:4:Resource_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Resource
              (Resource => Utils.Create_Resource
               (Name               => Utils.Create_Name (Str => Ref_Str),
                Memregion_Index    => 23,
                Channel_Info_Index => 42)'Address) = 1,
              Message   => "C resource mismatch");
--  begin read only
   end Test_Resource_To_C;
--  end read only


--  begin read only
   procedure Test_Dev_Info_To_C (Gnattest_T : in out Test);
   procedure Test_Dev_Info_To_C_2390e3 (Gnattest_T : in out Test) renames Test_Dev_Info_To_C;
--  id:2.2/2390e3308a4a8a5b/Dev_Info_To_C/1/0/
   procedure Test_Dev_Info_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:46:4:Dev_Info_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => C_Imports.C_Assert_Dev_Info
              (Dev_Info => Utils.Create_Dev_Info
               (SID         => 16#abcd#,
                IRTE_Start  => 200,
                IRQ_Start   => 12,
                IR_Count    => 22,
                MSI_Capable => True)'Address) = 1,
              Message   => "C dev info mismatch");
--  begin read only
   end Test_Dev_Info_To_C;
--  end read only


--  begin read only
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test);
   procedure Test_Subject_Info_To_C_3471da (Gnattest_T : in out Test) renames Test_Subject_Info_To_C;
--  id:2.2/3471dabca4420d92/Subject_Info_To_C/1/0/
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:49:4:Subject_Info_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Str : constant String (Name_Index_Type) := (others => 'a');
      Info    : Subject_Info_Type                 := Null_Subject_Info;
   begin
      for I in Resource_Index_Type loop
         Utils.Append_Channel
           (Info       => Info,
            Name       => Utils.Create_Name (Str => Ref_Str),
            Address    => 16#dead_beef_cafe_feed#,
            Size       => 16#8080_abab_cdcd_9090#,
            Writable   => True,
            Has_Event  => True,
            Has_Vector => True,
            Event      => 128,
            Vector     => 255);
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
   --  musinfo-interop.ads:52:4:Check_Name_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

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
   procedure Test_Check_Memregion_Type (Gnattest_T : in out Test);
   procedure Test_Check_Memregion_Type_0f8f2f (Gnattest_T : in out Test) renames Test_Check_Memregion_Type;
--  id:2.2/0f8f2f7565e4b586/Check_Memregion_Type/1/0/
   procedure Test_Check_Memregion_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:55:4:Check_Memregion_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Memregion_Type;
   begin
      Assert (Condition => C_Imports.C_Assert_Memregion_Type
              (Size           => Memregion_Type'Size / 8,
               Alignment      => Memregion_Type'Alignment,
               Address_Offset => Dummy.Address'Bit_Position / 8,
               Size_Offset    => Dummy.Size'Bit_Position / 8,
               Flags_Offset   => Dummy.Flags'Bit_Position / 8) = 1,
              Message   => "C memregion type mismatch");
--  begin read only
   end Test_Check_Memregion_Type;
--  end read only


--  begin read only
   procedure Test_Check_Channel_Type (Gnattest_T : in out Test);
   procedure Test_Check_Channel_Type_451002 (Gnattest_T : in out Test) renames Test_Check_Channel_Type;
--  id:2.2/451002ebfab2ca67/Check_Channel_Type/1/0/
   procedure Test_Check_Channel_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:58:4:Check_Channel_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Channel_Info_Type;
   begin

      Assert (Condition => C_Imports.C_Assert_Channel_Info_Type
              (Size          => Channel_Info_Type'Size / 8,
               Alignment     => Channel_Info_Type'Alignment,
               Flags_Offset  => Dummy.Flags'Bit_Position / 8,
               Event_Offset  => Dummy.Event'Bit_Position / 8,
               Vector_Offset => Dummy.Vector'Bit_Position / 8) = 1,
              Message   => "C channel type mismatch");
--  begin read only
   end Test_Check_Channel_Type;
--  end read only


--  begin read only
   procedure Test_Check_Resource_Type (Gnattest_T : in out Test);
   procedure Test_Check_Resource_Type_35d4af (Gnattest_T : in out Test) renames Test_Check_Resource_Type;
--  id:2.2/35d4afdd5e3ed28f/Check_Resource_Type/1/0/
   procedure Test_Check_Resource_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:61:4:Check_Resource_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Resource_Type;
   begin
      Assert
        (Condition => C_Imports.C_Assert_Resource_Type
           (Size                    => Resource_Type'Size / 8,
            Alignment               => Resource_Type'Alignment,
            Name_Offset             => Dummy.Name'Bit_Position / 8,
            Memregion_Idx_Offset    => Dummy.Memregion_Idx'Bit_Position / 8,
            Channel_Info_Idx_Offset => Dummy.Channel_Info_Idx'Bit_Position / 8)
         = 1,
        Message   => "C resource type mismatch");
--  begin read only
   end Test_Check_Resource_Type;
--  end read only


--  begin read only
   procedure Test_Check_Dev_Info_Type (Gnattest_T : in out Test);
   procedure Test_Check_Dev_Info_Type_ad50ac (Gnattest_T : in out Test) renames Test_Check_Dev_Info_Type;
--  id:2.2/ad50ace35c4ff63a/Check_Dev_Info_Type/1/0/
   procedure Test_Check_Dev_Info_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:64:4:Check_Dev_Info_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Dev_Info_Type;
   begin
      Assert (Condition => C_Imports.C_Assert_Dev_Info_Type
              (Size              => Dev_Info_Type'Size / 8,
               Alignment         => Dev_Info_Type'Alignment,
               IRTE_Start_Offset => Dummy.IRTE_Start'Bit_Position / 8,
               IRQ_Start_Offset  => Dummy.IRQ_Start'Bit_Position / 8,
               IR_Count_Offset   => Dummy.IR_Count'Bit_Position / 8,
               Flags_Offset      => Dummy.Flags'Bit_Position / 8) = 1,
              Message   => "C dev info type mismatch");
--  begin read only
   end Test_Check_Dev_Info_Type;
--  end read only


--  begin read only
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test);
   procedure Test_Check_Subject_Info_Type_659906 (Gnattest_T : in out Test) renames Test_Check_Subject_Info_Type;
--  id:2.2/659906a031093bd7/Check_Subject_Info_Type/1/0/
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test) is
   --  musinfo-interop.ads:67:4:Check_Subject_Info_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Dummy : Subject_Info_Type;
   begin

      Assert
        (Condition => C_Imports.C_Assert_Subject_Info_Type
           (Size                  => Subject_Info_Type'Size / 8,
            Alignment             => Subject_Info_Type'Alignment,
            Magic_Offset          => Dummy.Magic'Bit_Position / 8,
            Resource_Count_Offset => Dummy.Resource_Count'Bit_Position / 8,
            Memreg_Count_Offset   => Dummy.Memregion_Count'Bit_Position / 8,
            Channel_Count_Offset  => Dummy.Channel_Info_Count'Bit_Position / 8,
            Dev_Count_Offset      => Dummy.Dev_Info_Count'Bit_Position / 8,
            TSC_Khz_Offset        => Dummy.TSC_Khz'Bit_Position / 8,
            TSC_Schd_Start_Offset => Dummy.TSC_Schedule_Start'Bit_Position / 8,
            TSC_Schd_End_Offset   => Dummy.TSC_Schedule_End'Bit_Position / 8,
            Resources_Offset      => Dummy.Resources'Bit_Position / 8,
            Memregions_Offset     => Dummy.Memregions'Bit_Position / 8,
            Channels_Offset       => Dummy.Channels_Info'Bit_Position / 8,
            Dev_Info_Offset       => Dummy.Dev_Info'Bit_Position / 8) = 1,
         Message   => "C subject info type mismatch");

--  begin read only
   end Test_Check_Subject_Info_Type;
--  end read only

end Musinfo.Interop.Test_Data.Tests;