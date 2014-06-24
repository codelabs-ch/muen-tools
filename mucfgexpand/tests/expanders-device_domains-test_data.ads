--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with DOM.Core.Documents;

with Muxml.Utils;

with Test_Utils.Expander;

package Expanders.Device_Domains.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Remove device domains from policy.
   procedure Remove_Device_Domains
     (Data : in out Muxml.XML_Data_Type);

end Expanders.Device_Domains.Test_Data;
