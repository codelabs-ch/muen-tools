--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Zp.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Zp.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Create_e820_Map (Gnattest_T : in out Test);
   procedure Test_Create_e820_Map_f36a26 (Gnattest_T : in out Test) renames Test_Create_e820_Map;
--  id:2.2/f36a2614086ebbe1/Create_e820_Map/1/0/
   procedure Test_Create_e820_Map (Gnattest_T : in out Test) is
   --  zp-utils.ads:27:4:Create_e820_Map
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type bootparam_h.boot_params_e820_map_array;

      Ref_Map : constant bootparam_h.boot_params_e820_map_array
        := (0      => (addr   => 16#3000#,
                       size   => 16#d000#,
                       c_type => Constants.E820_RESERVED),
            1      => (addr   => 16#1000#,
                       size   => 16#1000#,
                       c_type => Constants.E820_RESERVED),
            2      => (addr   => 16#f000#,
                       size   => 16#9000#,
                       c_type => Constants.E820_RESERVED),
            3      => (addr   => 16#1000_0000#,
                       size   => 16#1000#,
                       c_type => Constants.E820_ACPI),
            4      => (addr   => 16#ffff_f000#,
                       size   => 16#1000#,
                       c_type => Constants.E820_RESERVED),
            others => (addr   => 0,
                       size   => 0,
                       c_type => 0));
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Mem_Nodes : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/subjects/subject[@name='lnx']/memory/memory");
         Map : bootparam_h.boot_params_e820_map_array;
      begin
         Map := Create_e820_Map (Memory => Mem_Nodes);

         Assert (Condition => Ref_Map = Map,
                 Message   => "e820 map differs");
      end;
--  begin read only
   end Test_Create_e820_Map;
--  end read only

end Zp.Utils.Test_Data.Tests;
