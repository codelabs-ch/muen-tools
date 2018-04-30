--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Expanders.Subjects.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Inject_Idle_Subject (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/minorFrame[1]",
         Name  => "subject",
         Value => "mugenschedcfg_auto_idle_0");
   end Inject_Idle_Subject;

   -------------------------------------------------------------------------

   procedure Prepare_Loader_Expansion (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Prepare_Profile (Data => Data);
      Add_Sinfo_Regions (Data => Data);
      Handle_Profile (Data => Data);
   end Prepare_Loader_Expansion;

   -------------------------------------------------------------------------

   procedure Prepare_Profile (Data: in out Muxml.XML_Data_Type)
   is
   begin
      Add_Missing_Elements (Data => Data);
      Components.Add_Subject_Profile_VCPU (Data => Data);
      Siblings.Add_Subject_Profile_VCPU (Data => Data);
   end Prepare_Profile;

   -------------------------------------------------------------------------

   procedure Prepare_Sched_Info_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Missing_Elements (Data => Data);
      Add_Tau0 (Data => Data);
      Add_Global_IDs (Data => Data);
   end Prepare_Sched_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Remove_Subj_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects/subject[@name='lnx']/devices/"
           & "device[@logical='xhci']");
      Resources : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Node,
           XPath => "irq|memory|ioPort");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Resources) - 1 loop
         declare
            Res : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Resources,
               Index => I);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Node,
               Child_Name => DOM.Core.Nodes.Node_Name (N => Res));
         end;
      end loop;
   end Remove_Subj_Device_Resources;

end Expanders.Subjects.Test_Data;
