--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Tables.Test_Data.Tests is


--  begin read only
   procedure Test_1_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_7bb9cf (Gnattest_T : in out Test) renames Test_1_Add_Entry;
--  id:2.2/7bb9cf95e4456862/Add_Entry/1/0/
   procedure Test_1_Add_Entry (Gnattest_T : in out Test) is
   --  vtd-tables.ads:40:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Root_Table : Root_Table_Type;
   begin
      Add_Entry (RT  => Root_Table,
                 Bus => 12,
                 CTP => 16#1000#);
      Assert (Condition => Root_Table.Entries (12).Present = 1,
              Message   => "Entry not present (1)");
      Assert (Condition => Root_Table.Entries (12).CTP = 1,
              Message   => "CTP mismatch (1)");

      Add_Entry (RT  => Root_Table,
                 Bus => 255,
                 CTP => 16#ffff_ffff#);
      Assert (Condition => Root_Table.Entries (255).Present = 1,
              Message   => "Entry not present (2)");
      Assert (Condition => Root_Table.Entries (255).CTP = 16#000f_ffff#,
              Message   => "CTP mismatch (2)");

--  begin read only
   end Test_1_Add_Entry;
--  end read only


--  begin read only
   procedure Test_1_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_3e830c (Gnattest_T : in out Test) renames Test_1_Serialize;
--  id:2.2/3e830c731d3b0f3a/Serialize/1/0/
   procedure Test_1_Serialize (Gnattest_T : in out Test) is
   --  vtd-tables.ads:46:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      Root_Table : Root_Table_Type;
   begin
      Serialize (RT       => Root_Table,
                 Filename => "obj/serialize_rt_default");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_rt_default",
               Filename2 => "data/serialize_rt_default"),
              Message   => "Default table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_rt_default");

      Add_Entry (RT  => Root_Table,
                 Bus => Table_Range'First,
                 CTP => Table_Pointer_Type'Last);
      Add_Entry (RT  => Root_Table,
                 Bus => Table_Range'Last,
                 CTP => Table_Pointer_Type'Last);
      Serialize (RT       => Root_Table,
                 Filename => "obj/serialize_rt");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_rt",
               Filename2 => "data/serialize_rt"),
              Message   => "Table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_rt");
--  begin read only
   end Test_1_Serialize;
--  end read only


--  begin read only
   procedure Test_2_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_f03eb2 (Gnattest_T : in out Test) renames Test_2_Add_Entry;
--  id:2.2/f03eb2d99e674632/Add_Entry/0/0/
   procedure Test_2_Add_Entry (Gnattest_T : in out Test) is
   --  vtd-tables.ads:62:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;

      Context_Table : Context_Table_Type;
   begin
      Add_Entry (CT      => Context_Table,
                 Device  => 1,
                 Func    => 6,
                 Domain  => 24,
                 SLPTPTR => 16#4000#);
      Assert (Condition => Context_Table.Entries (14).Present = 1,
              Message   => "Entry not present");
      Assert (Condition => Context_Table.Entries (14).DID = 24,
              Message   => "DID mismatch");
      Assert (Condition => Context_Table.Entries (14).SLPTPTR = 4,
              Message   => "SLPTPTR mismatch");
--  begin read only
   end Test_2_Add_Entry;
--  end read only


--  begin read only
   procedure Test_2_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_eeeebf (Gnattest_T : in out Test) renames Test_2_Serialize;
--  id:2.2/eeeebf76e1fa2a78/Serialize/0/0/
   procedure Test_2_Serialize (Gnattest_T : in out Test) is
   --  vtd-tables.ads:70:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      Context_Table : Context_Table_Type;
   begin
      Serialize (CT       => Context_Table,
                 Filename => "obj/serialize_ct_default");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_ct_default",
               Filename2 => "data/serialize_ct_default"),
              Message   => "Default table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_ct_default");

      Add_Entry (CT      => Context_Table,
                 Device  => 2,
                 Func    => 2,
                 Domain  => 128,
                 SLPTPTR => Table_Pointer_Type'Last);
      Add_Entry (CT      => Context_Table,
                 Device  => 31,
                 Func    => 7,
                 Domain  => 255,
                 SLPTPTR => Table_Pointer_Type'Last);
      Serialize (CT       => Context_Table,
                 Filename => "obj/serialize_ct");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_ct",
               Filename2 => "data/serialize_ct"),
              Message   => "Table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_ct");
--  begin read only
   end Test_2_Serialize;
--  end read only

end VTd.Tables.Test_Data.Tests;
