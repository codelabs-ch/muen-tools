--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Immutable_Processors;
with Mucfgcheck.Memory;
with Mucfgcheck.Device;
with Mucfgcheck.Events;
with Mucfgcheck.Platform;
with Mucfgcheck.Subject;

pragma Elaborate_All (Mutools.Immutable_Processors);

package body Expand.Pre_Checks
is

   --  Check the existence of channel endpoint (reader or writer) event
   --  attributes given by name. The XPath query specifies which global
   --  channels should be checked.
   procedure Check_Channel_Events_Attr
     (XML_Data  : Muxml.XML_Data_Type;
      XPath     : String;
      Endpoint  : String;
      Attr_Name : String);

   --  The procedure checks for all existing subjects in the specified policy
   --  that a given attribute of component resource mappings is unique
   --  per-subject.
   procedure Check_Subject_Resource_Maps_Attr_Uniqueness
     (XML_Data : Muxml.XML_Data_Type;
      Attr     : String);

   -------------------------------------------------------------------------

   procedure Channel_Reader_Has_Event_Vector (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Channel_Events_Attr
        (XML_Data  => XML_Data,
         XPath     => "/system/channels/channel[@hasEvent!='switch']",
         Endpoint  => "reader",
         Attr_Name => "vector");
   end Channel_Reader_Has_Event_Vector;

   -------------------------------------------------------------------------

   procedure Channel_Reader_Writer (XML_Data : Muxml.XML_Data_Type)
   is
      Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/channels/channel");
      Readers  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/reader");
      Writers  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/writer");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Channels)'Img & " channel(s) for reader/writer "
                 & "count");
      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
            Channel      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channels,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel,
                 Name => "name");
            Has_Event    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel,
                 Name => "hasEvent");
            Reader_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Readers,
                    Ref_Attr  => "physical",
                    Ref_Value => Channel_Name));
            Writer_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Writers,
                    Ref_Attr  => "physical",
                    Ref_Value => Channel_Name));
         begin
            if (Has_Event'Length > 0 and then Reader_Count /= 1)
              or (Has_Event'Length = 0 and then Reader_Count < 1)
            then
               raise Mucfgcheck.Validation_Error with "Invalid number of "
                 & "readers for channel '" & Channel_Name & "':"
                 & Reader_Count'Img;
            end if;

            if Writer_Count /= 1 then
               raise Mucfgcheck.Validation_Error with "Invalid number of "
                 & "writers for channel '" & Channel_Name & "':"
                 & Writer_Count'Img;
            end if;
         end;
      end loop;
   end Channel_Reader_Writer;

   -------------------------------------------------------------------------

   procedure Channel_Writer_Has_Event_ID (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Channel_Events_Attr
        (XML_Data  => XML_Data,
         XPath     => "/system/channels/channel[@hasEvent]",
         Endpoint  => "writer",
         Attr_Name => "event");
   end Channel_Writer_Has_Event_ID;

   -------------------------------------------------------------------------

   procedure Check_Channel_Events_Attr
     (XML_Data  : Muxml.XML_Data_Type;
      XPath     : String;
      Endpoint  : String;
      Attr_Name : String)
   is
      Channels  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => XPath);
      Endpoints : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/" & Endpoint);
   begin
      Mulog.Log (Msg => "Checking '" & Attr_Name & "' attribute of"
                 & DOM.Core.Nodes.Length (List => Channels)'Img & " channel "
                 & Endpoint & "(s) with associated event");

      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
            Channel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channels,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "name");
            Node         : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Endpoints,
                 Ref_Attr  => "physical",
                 Ref_Value => Channel_Name);
         begin
            if DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Attr_Name) = ""
            then
               raise Mucfgcheck.Validation_Error with "Missing '" & Attr_Name
                 & "' attribute for " & Endpoint & " of channel '"
                 & Channel_Name & "'";
            end if;
         end;
      end loop;
   end Check_Channel_Events_Attr;

   -------------------------------------------------------------------------

   procedure Check_Subject_Resource_Maps_Attr_Uniqueness
     (XML_Data : Muxml.XML_Data_Type;
      Attr     : String)
   is
      --  Check inequality of specified mappings attribute.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => Attr);
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => Attr);
      begin
         if Left_Name = Right_Name then
            raise Mucfgcheck.Validation_Error with "Multiple " & Attr
              & " resource mappings with name '" & Left_Name & "' in subject '"
              & DOM.Core.Elements.Get_Attribute
              (Elem => Muxml.Utils.Ancestor_Node
                 (Node  => Left,
                  Level => 2),
               Name => "name") & "'";
         end if;
      end Check_Inequality;

      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Mappings  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "component/map");
         begin
            if DOM.Core.Nodes.Length (List => Mappings) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Mappings)'Img
                          & " subject " & Attr & " resource mappings");
               Mucfgcheck.Compare_All (Nodes      => Mappings,
                                       Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Check_Subject_Resource_Maps_Attr_Uniqueness;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Check_Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   procedure Component_Channel_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");

      Component_Name : Unbounded_String;

      --  Check inequality of logical channel names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "logical");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "logical");
      begin
         if Left_Name = Right_Name then
            raise Mucfgcheck.Validation_Error with "Multiple channels with "
              & "name '" & Left_Name & "' in component '"
              & To_String (Component_Name) & "'";
         end if;
      end Check_Inequality;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Components,
                                      Index => I);
            Channels  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
         begin
            Component_Name := To_Unbounded_String
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Comp_Node,
                  Name => "name"));
            if DOM.Core.Nodes.Length (List => Channels) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Channels)'Img
                          & " channel names in component '"
                          & To_String (Component_Name) & "'");
               Mucfgcheck.Compare_All (Nodes      => Channels,
                                       Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Component_Channel_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Component_Channel_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Components    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Phys_Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/channels/channel");
      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1
      loop
         declare
            Subj_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "channels/*");
            Comp_Ref_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Elem => Subj_Node,
                    Name => "component"),
                 Index => 0);
            Comp_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
         begin
            if DOM.Core.Nodes.Length (Comp_Channels) > 0 then
               Mulog.Log (Msg => "Checking size of" & DOM.Core.Nodes.Length
                          (List => Comp_Channels)'Img & " component '"
                          & Comp_Name & "' channel(s) referenced by subject '"
                          & Subj_Name & "'");

               for J in 0 .. DOM.Core.Nodes.Length (List => Comp_Channels) - 1
               loop
                  declare
                     use type Interfaces.Unsigned_64;

                     Comp_Channel_Node : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Comp_Channels,
                          Index => J);
                     Comp_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Comp_Channel_Node,
                          Name => "logical");
                     Comp_Channel_Size : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Comp_Channel_Node,
                          Name => "size");
                     Subj_Channel_Link : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Subj_Channels,
                          Ref_Attr  => "logical",
                          Ref_Value => Comp_Channel_Name);
                     Phys_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Subj_Channel_Link,
                          Name => "physical");
                     Phys_Channel_Node : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Phys_Channels,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Channel_Name);
                     Phys_Channel_Size : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Channel_Node,
                          Name => "size");
                  begin
                     if Interfaces.Unsigned_64'Value (Phys_Channel_Size)
                       /= Interfaces.Unsigned_64'Value (Comp_Channel_Size)
                     then
                        raise Mucfgcheck.Validation_Error with "Component '"
                          & Comp_Name & "' referenced by subject '" & Subj_Name
                          & "' requests size " & Comp_Channel_Size & " for "
                          & "logical channel '" & Comp_Channel_Name & "' but "
                          & "linked physical channel '" & Phys_Channel_Name
                          & "' " & "has size " & Phys_Channel_Size;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Component_Channel_Size;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Platform_CPU_Count_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Attr_Path : constant String := "/system/platform/processor/@logicalCpus";
      Attr      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Attr_Path);
   begin
      Mulog.Log (Msg => "Checking presence of '" & Attr_Path & "' attribute");

      if DOM.Core.Nodes.Length (List => Attr) /= 1 then
         raise Mucfgcheck.Validation_Error with "Required "
           & "'/system/platform/processor/@logicalCpus' attribute not found, "
           & "add it or use mucfgmerge tool";
      end if;
   end Platform_CPU_Count_Presence;

   -------------------------------------------------------------------------

   procedure Platform_IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Device : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/devices/device[@name='ioapic']/memory");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Device);
   begin
      Mulog.Log (Msg => "Checking presence of I/O APIC device");

      if Count < 1 then
         raise Mucfgcheck.Validation_Error with "Required I/O APIC device with"
           & " memory region missing";
      elsif Count > 1 then
         raise Mucfgcheck.Validation_Error with "Multiple I/O APIC devices"
           & " or I/O APIC device with multiple memory regions present";
      end if;
   end Platform_IOAPIC_Presence;

   -------------------------------------------------------------------------

   procedure Platform_IOMMU_Memory (XML_Data : Muxml.XML_Data_Type)
   is
      IOMMUs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/devices/device[starts-with"
           & "(string(@name),'iommu')]");
      Dev_Count : constant Natural := DOM.Core.Nodes.Length (List => IOMMUs);
   begin
      if Dev_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking presence of" & Dev_Count'Img
                 & " IOMMU memory region(s)");

      for I in 0 .. Dev_Count - 1 loop
         declare
            IOMMU     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOMMUs,
                 Index => I);
            Dev_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOMMU,
                 Name => "name");
            Memory    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => IOMMU,
                 XPath => "memory");
            Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Memory);
         begin
            if Mem_Count < 1 then
               raise Mucfgcheck.Validation_Error with "IOMMU device '"
                 & Dev_Name & "' has no memory region";
            elsif Mem_Count > 1 then
               raise Mucfgcheck.Validation_Error with "IOMMU device '"
                 & Dev_Name & "' has multiple memory regions";
            end if;
         end;
      end loop;
   end Platform_IOMMU_Memory;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Check_Procs.Register
        (Process => Mucfgcheck.Memory.Physical_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.Device_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.PCI_Device_BDF_Uniqueness'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.IOMMU_Region_Size'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Platform.PCI_Config_Space_Address'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Subject_Event_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Source_Targets'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Self_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Subject.Runnability'Access);

      Check_Procs.Register (Process => Tau0_Presence_In_Scheduling'Access);
      Check_Procs.Register
        (Process => Subject_Resource_Maps_Logical_Uniqueness'Access);
      Check_Procs.Register
        (Process => Subject_Resource_Maps_Physical_Uniqueness'Access);
      Check_Procs.Register
        (Process => Subject_Resource_Maps_Physical_References'Access);
      Check_Procs.Register (Process => Subject_Monitor_References'Access);
      Check_Procs.Register (Process => Subject_Channel_References'Access);
      Check_Procs.Register (Process => Subject_Component_References'Access);
      Check_Procs.Register (Process => Subject_Channel_Exports'Access);
      Check_Procs.Register (Process => Channel_Reader_Writer'Access);
      Check_Procs.Register (Process => Channel_Writer_Has_Event_ID'Access);
      Check_Procs.Register (Process => Channel_Reader_Has_Event_Vector'Access);
      Check_Procs.Register (Process => Platform_CPU_Count_Presence'Access);
      Check_Procs.Register (Process => Platform_IOAPIC_Presence'Access);
      Check_Procs.Register (Process => Platform_IOMMU_Memory'Access);

      --  Register after platform CPU count presence check.

      Check_Procs.Register (Process => Mucfgcheck.Platform.CPU_Count'Access);

      Check_Procs.Register
        (Process => Component_Channel_Name_Uniqueness'Access);
      Check_Procs.Register (Process => Component_Channel_Size'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type) renames Check_Procs.Run;

   -------------------------------------------------------------------------

   procedure Subject_Channel_Exports (XML_Data : Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1
      loop
         declare
            Subj_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Ref_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Elem => Subj_Node,
                    Name => "component"),
                 Index => 0);
            Mappings      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
         begin
            if DOM.Core.Nodes.Length (Comp_Channels) > 0 then
               Mulog.Log (Msg => "Checking export of" & DOM.Core.Nodes.Length
                          (List => Mappings)'Img & " logical channel mappings"
                          & "(s) in subject '" & Subj_Name & "' with component"
                          & " '" & Comp_Name & "'");

               for J in 0 .. DOM.Core.Nodes.Length (List => Comp_Channels) - 1
               loop
                  declare
                     use type DOM.Core.Node;

                     Comp_Channel_Node : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Comp_Channels,
                          Index => J);
                     Comp_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Comp_Channel_Node,
                          Name => "logical");
                     Subj_Channel_Link : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Comp_Channel_Name);
                  begin
                     if Subj_Channel_Link = null then
                        raise Mucfgcheck.Validation_Error with "Subject '"
                          & Subj_Name & "' does not export logical channel '"
                          & Comp_Channel_Name & "' as requested by referenced "
                          & "component '" & Comp_Name & "'";
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Subject_Channel_Exports;

   -------------------------------------------------------------------------

   procedure Subject_Channel_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and channel.
      function Match_Channel_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Channel_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
         Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Channel '" & Ref_Channel_Name & "' referenced by subject '"
           & Subj_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Channel_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Ref_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Channel_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Channel_Name;
      end Match_Channel_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/channels/*",
         Ref_XPath    => "/system/channels/channel",
         Log_Message  => "subject channel reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Channel_Name'Access);
   end Subject_Channel_References;

   -------------------------------------------------------------------------

   procedure Subject_Component_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and component.
      function Match_Component_Name
        (Left, Right : DOM.Core.Node)
         return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Comp_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "ref");
         Subj_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
      begin
         return "Component '" & Ref_Comp_Name & "' referenced by subject '"
           & Subj_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Component_Name
        (Left, Right : DOM.Core.Node)
         return Boolean
      is
         Ref_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "ref");
         Comp_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Comp_Name;
      end Match_Component_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/component",
         Ref_XPath    => "/system/components/component",
         Log_Message  => "subject component reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Component_Name'Access);
   end Subject_Component_References;

   -------------------------------------------------------------------------

   procedure Subject_Monitor_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "subject");
         Subj_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Subject '" & Ref_Subj_Name & "' referenced by subject monitor"
           & " '" & Subj_Name & "' does not exist";
      end Error_Msg;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/monitor/state",
         Ref_XPath    => "/system/subjects/subject",
         Log_Message  => "subject monitor reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mucfgcheck.Match_Subject_Name'Access);
   end Subject_Monitor_References;

   -------------------------------------------------------------------------

   procedure Subject_Resource_Maps_Logical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Resource_Maps_Attr_Uniqueness
        (XML_Data => XML_Data,
         Attr     => "logical");
   end Subject_Resource_Maps_Logical_Uniqueness;

   -------------------------------------------------------------------------

   procedure Subject_Resource_Maps_Physical_References
     (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
         Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Physical channel '" & Ref_Name & "' referenced by subject '"
           & Subj_Name & "' component resource mapping does not exist";
      end Error_Msg;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/component/map",
         Ref_XPath    => "/system/channels/channel",
         Log_Message  => "subject resource mapping physical reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mucfgcheck.Is_Valid_Reference'Access);
   end Subject_Resource_Maps_Physical_References;

   -------------------------------------------------------------------------

   procedure Subject_Resource_Maps_Physical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Resource_Maps_Attr_Uniqueness
        (XML_Data => XML_Data,
         Attr     => "physical");
   end Subject_Resource_Maps_Physical_Uniqueness;

   -------------------------------------------------------------------------

   procedure Tau0_Presence_In_Scheduling (XML_Data : Muxml.XML_Data_Type)
   is
      Tau0_Node : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame"
           & "[@subject='tau0']");
   begin
      Mulog.Log
        (Msg => "Checking presence of tau0 subject in scheduling plan");
      if DOM.Core.Nodes.Length (List => Tau0_Node) = 0 then
         raise Mucfgcheck.Validation_Error with "Subject tau0 not present in "
           & "scheduling plan";
      end if;
   end Tau0_Presence_In_Scheduling;

end Expand.Pre_Checks;
