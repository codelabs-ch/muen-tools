--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Muxml.Utils;

with Expanders.Platform;
with Test_Utils.Expander;

package Expanders.Subjects.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Remove resources of logical device 'xhci'.
   procedure Remove_Subj_Device_Resources (Data : in out Muxml.XML_Data_Type);

   --  Prepare subjects for loader expansion step.
   procedure Prepare_Loader_Expansion (Data : in out Muxml.XML_Data_Type);

   --  Prepare subjects for scheduling group info mappings expansion step.
   procedure Prepare_Sched_Info_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Inject Mugenschedcfg idle subject.
   procedure Inject_Idle_Subject (Data : in out Muxml.XML_Data_Type);

end Expanders.Subjects.Test_Data;
