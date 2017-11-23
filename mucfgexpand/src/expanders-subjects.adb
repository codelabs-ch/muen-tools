--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.PCI;
with Mutools.Utils;
with Mutools.Types;
with Mutools.XML_Utils;
with Mutools.Constants;
with Mucfgvcpu;
with Mucfgcheck.Events;

with Expanders.Config;
with Expanders.Types;
with Expanders.Utils;
with Expanders.XML_Utils;
with Expanders.Subjects.Config;
with Expanders.Subjects.Profiles;

package body Expanders.Subjects
is

   use Ada.Strings.Unbounded;

   package MC renames Mutools.Constants;

   --  Mapping of subject profiles to legacy IRQ vector remapping offset.
   --  Note: Linux uses IRQ0 (vector 48) for the timer.
   Subj_IRQ_Remap_Offset : constant array
     (Types.Subject_Profile_Type) of Natural
     := (Types.Native           => MC.Host_IRQ_Remap_Offset,
         Types.VM | Types.Linux => 48);

   --  Mapping of subject profiles to MSI vector remapping offset.
   --  The value 40 is chosen to remap MSIs since it is the one used by Linux.
   Subj_MSI_Remap_Offset : constant array
     (Types.Subject_Profile_Type) of Natural
     := (Types.Native           => MC.Host_IRQ_Remap_Offset + 40,
         Types.VM | Types.Linux => 48 + 40);

   -------------------------------------------------------------------------

   procedure Add_Channel_Events (Data : in out Muxml.XML_Data_Type)
   is

      --  Add optional events/source/group[@name='vmcall'] elements.
      function Add_Optional_Events_Source_Group
        (Subject : DOM.Core.Node)
         return DOM.Core.Node;

      --  Add optional events/target element.
      function Add_Optional_Events_Target
        (Subject : DOM.Core.Node)
         return DOM.Core.Node;

      ----------------------------------------------------------------------

      function Add_Optional_Events_Source_Group
        (Subject : DOM.Core.Node)
         return DOM.Core.Node
      is
         use type DOM.Core.Node;

         Writer_Subj_Source_Node  : DOM.Core.Node;
         Writer_Subj_Source_Group : DOM.Core.Node;
         Writer_Subj_Events_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Subject,
                                       XPath => "events");
      begin
         Writer_Subj_Source_Node := Muxml.Utils.Get_Element
           (Doc   => Writer_Subj_Events_Node,
            XPath => "source");
         if Writer_Subj_Source_Node = null then
            Writer_Subj_Source_Node := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "source");
            Muxml.Utils.Insert_Before
              (Parent    => Writer_Subj_Events_Node,
               New_Child => Writer_Subj_Source_Node,
               Ref_Child => "target");
         end if;

         Writer_Subj_Source_Group := Muxml.Utils.Get_Element
           (Doc   =>  Writer_Subj_Source_Node,
            XPath => "group[@name='vmcall']");
         if Writer_Subj_Source_Group = null then
            Writer_Subj_Source_Group := DOM.Core.Nodes.Append_Child
              (N         => Writer_Subj_Source_Node,
               New_Child => DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "group"));
            DOM.Core.Elements.Set_Attribute
              (Elem  => Writer_Subj_Source_Group,
               Name  => "name",
               Value => "vmcall");
         end if;

         return Writer_Subj_Source_Group;
      end Add_Optional_Events_Source_Group;

      ----------------------------------------------------------------------

      function Add_Optional_Events_Target
        (Subject : DOM.Core.Node)
         return DOM.Core.Node
      is
         use type DOM.Core.Node;

         Reader_Subj_Events_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Subject,
              XPath => "events");
         Reader_Subj_Target_Node : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Reader_Subj_Events_Node,
              XPath => "target");
      begin
         if Reader_Subj_Target_Node = null then
            Reader_Subj_Target_Node := DOM.Core.Nodes.Append_Child
              (N         => Reader_Subj_Events_Node,
               New_Child => DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "target"));
         end if;

         return Reader_Subj_Target_Node;
      end Add_Optional_Events_Target;

      ----------------------------------------------------------------------

      Events_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/events");
      Channels    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/channels/channel[@hasEvent]");
   begin
      Mulog.Log (Msg => "Adding events for" & DOM.Core.Nodes.Length
                 (List => Channels)'Img & " channel(s)");

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
            Channel_Mode : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "hasEvent");
            Event_Node  : DOM.Core.Node;
            Writer_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject/channels/writer"
                 & "[@physical='" & Channel_Name & "']");
            Reader_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject/channels/reader"
                 & "[@physical='" & Channel_Name & "']");
            Writer_Subj_Source_Group, Reader_Subj_Target_Node : DOM.Core.Node;
         begin
            Event_Node := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "event");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Event_Node,
               Name  => "name",
               Value => Channel_Name);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Event_Node,
               Name  => "mode",
               Value => Channel_Mode);
            Muxml.Utils.Append_Child
              (Node      => Events_Node,
               New_Child => Event_Node);

            Writer_Subj_Source_Group := Add_Optional_Events_Source_Group
              (Subject => Muxml.Utils.Ancestor_Node
                 (Node  => Writer_Node,
                  Level => 2));
            Reader_Subj_Target_Node := Add_Optional_Events_Target
              (Subject => Muxml.Utils.Ancestor_Node
                 (Node  => Reader_Node,
                  Level => 2));

            declare
               ID : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Writer_Node,
                    Name => "event");
               Vector : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Reader_Node,
                    Name => "vector");
               Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Writer_Node,
                    Name => "physical");
            begin
               Muxml.Utils.Append_Child
                 (Node      => Writer_Subj_Source_Group,
                  New_Child => XML_Utils.Create_Source_Event_Node
                    (Policy        => Data,
                     ID            => ID,
                     Logical_Name  => "channel_event_" & Name,
                     Physical_Name => Name));
               Muxml.Utils.Append_Child
                 (Node      => Reader_Subj_Target_Node,
                  New_Child => XML_Utils.Create_Target_Event_Node
                    (Policy        => Data,
                     Logical_Name  => "channel_event_" & Name,
                     Physical_Name => Name,
                     Vector        => Vector));
            end;
         end;
      end loop;
   end Add_Channel_Events;

   -------------------------------------------------------------------------

   procedure Add_Channel_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/channels/*");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Channel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "physical");
            Logical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "logical");
            Channel_Addr : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "virtualAddress");
            Channel_Writer : constant Boolean
              := DOM.Core.Nodes.Node_Name (N => Channel_Node) = "writer";
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Channel_Node,
                 Level => 2);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Mapping channel '" & Channel_Name & "' "
                       & (if Channel_Writer then "writable" else "readable")
                       & " to virtual address " & Channel_Addr
                       & " of subject '" & Subj_Name & "'");
            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => Logical_Name,
                  Physical_Name => Channel_Name,
                  Address       => Channel_Addr,
                  Writable      => Channel_Writer,
                  Executable    => False));
         end;
      end loop;
   end Add_Channel_Mappings;

   -------------------------------------------------------------------------

   procedure Add_CPU_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            CPU_ID     : constant Integer
              := Mutools.XML_Utils.Get_Executing_CPU
                (Data    => Data,
                 Subject => Subj_Node);
            CPU_ID_Str : constant String
              := Ada.Strings.Fixed.Trim (Source => CPU_ID'Img,
                                         Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting cpu of subject '" & Subj_Name
                       & "' to " & CPU_ID_Str);
            DOM.Core.Elements.Set_Attribute (Elem  => Subj_Node,
                                             Name  => "cpu",
                                             Value => CPU_ID_Str);
         end;
      end loop;
   end Add_CPU_IDs;

   -------------------------------------------------------------------------

   procedure Add_Default_Events (Data : in out Muxml.XML_Data_Type)
   is
      Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/default");

      --  Returns True if an event with specified reference ID exists in the
      --  given node list.
      function ID_Exists
        (Nodes  : DOM.Core.Node_List;
         Ref_ID : Natural)
         return Boolean;

      ----------------------------------------------------------------------

      function ID_Exists
        (Nodes  : DOM.Core.Node_List;
         Ref_ID : Natural)
         return Boolean
      is
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
            declare
               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I);
               ID_Str : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "id");
            begin
               if Natural'Value (ID_Str) = Ref_ID then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end ID_Exists;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Def_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Physical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Def_Node,
                 Name => "physical");
            Group_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Def_Node);
            Group_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Group_Node,
                 Name => "name");
            Group : constant Mutools.Types.Event_Group_Type
              := Mutools.Types.Event_Group_Type'Value (Group_Name);
            Group_Events : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Group_Node,
                 XPath => "event");
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Def_Node,
                 Level => 4);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Group_Max_Event : constant Natural := Mucfgcheck.Events.Get_Max_ID
              (Group => Group);
         begin
            Mulog.Log (Msg => "Adding default events to event group '"
                       & Group_Name & "' of subject '" & Subj_Name & "'");

            for ID in Natural range 0 .. Group_Max_Event loop
               declare
                  ID_Str : constant String := Ada.Strings.Fixed.Trim
                    (Source => ID'Img,
                     Side   => Ada.Strings.Left);
               begin
                  if Mucfgcheck.Events.Is_Valid_Event_ID
                    (Group => Group,
                     ID    => ID)
                    and then
                      not ID_Exists (Nodes  => Group_Events,
                                     Ref_ID => ID)
                  then
                     Muxml.Utils.Append_Child
                       (Node      => Group_Node,
                        New_Child => XML_Utils.Create_Source_Event_Node
                          (Policy        => Data,
                           ID            => ID_Str,
                           Logical_Name  => "default_event_" & ID_Str,
                           Physical_Name => Physical_Name));
                  end if;
               end;
            end loop;

            Muxml.Utils.Remove_Child (Node       => Group_Node,
                                      Child_Name => "default");
         end;
      end loop;
   end Add_Default_Events;

   -------------------------------------------------------------------------

   procedure Add_Device_BDFs (Data : in out Muxml.XML_Data_Type)
   is
      PCI_Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[pci]");
      Subjects    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[devices/device]");

      --  Return all subject devices that are part of a physical PCI
      --  multi-function device with the given physical device. The specified
      --  physical device is always included in the returned list.
      function Get_All_Device_Functions_Refs
        (Subject_Devices : DOM.Core.Node_List;
         Physical_Device : DOM.Core.Node)
         return DOM.Core.Node_List;

      ----------------------------------------------------------------------

      function Get_All_Device_Functions_Refs
        (Subject_Devices : DOM.Core.Node_List;
         Physical_Device : DOM.Core.Node)
         return DOM.Core.Node_List
      is
         Bus_Nr        : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Physical_Device,
              XPath => "pci",
              Name  => "bus");
         Dev_Nr        : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Physical_Device,
              XPath => "pci",
              Name  => "device");
         Phys_Siblings : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Data.Doc,
              XPath => "/system/hardware/devices/device/pci[@bus='" & Bus_Nr
              & "' and @device='" & Dev_Nr & "']/..");

         Sibling_Devs : DOM.Core.Node_List;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Phys_Siblings) - 1 loop
            declare
               use type DOM.Core.Node;

               Phys_Dev  : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Phys_Siblings,
                                         Index => I);
               Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Dev,
                    Name => "name");
               Log_Dev : constant DOM.Core.Node := Muxml.Utils.Get_Element
                    (Nodes     => Subject_Devices,
                     Ref_Attr  => "physical",
                     Ref_Value => Phys_Name);
            begin
               if Log_Dev /= null then
                  DOM.Core.Append_Node
                    (List => Sibling_Devs,
                     N    => Log_Dev);
               end if;
            end;
         end loop;

         return Sibling_Devs;
      end Get_All_Device_Functions_Refs;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            All_Subj_Devs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device");
            Subject_Devs  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device[not (pci)]");
            Assigned_BDFs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/pci");
            Dev_Nr_Allocator : Utils.Number_Allocator_Type (Range_Start => 1,
                                                            Range_End   => 31);
         begin
            Utils.Reserve_Numbers (Allocator => Dev_Nr_Allocator,
                                   Nodes     => Assigned_BDFs,
                                   Attribute => "device");
            for J in 0 .. DOM.Core.Nodes.Length (List => Subject_Devs) - 1
            loop
               declare
                  use type DOM.Core.Node;

                  Subj_Dev  : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subject_Devs,
                       Index => J);
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Dev,
                       Name => "physical");
                  Phys_Dev  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => PCI_Devices,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Name);
               begin

                  --  Phys_Dev may be null if referenced physical device is not
                  --  a PCI device.

                  if Phys_Dev /= null then
                     declare
                        Siblings         : constant DOM.Core.Node_List
                          := Get_All_Device_Functions_Refs
                            (Subject_Devices => All_Subj_Devs,
                             Physical_Device => Phys_Dev);
                        Device_Nr        : Natural := 0;
                        Fun_Nr_Allocator : Utils.Number_Allocator_Type
                          (Range_Start => 0,
                           Range_End   => 7);

                        Devs_To_Allocate : DOM.Core.Node_List;
                     begin
                        for K in 0 .. DOM.Core.Nodes.Length
                          (List => Siblings) - 1
                        loop
                           declare
                              Cur_Sibling : constant DOM.Core.Node
                                := DOM.Core.Nodes.Item (List  => Siblings,
                                                        Index => K);
                              Dev_Nr      : constant String
                                := Muxml.Utils.Get_Attribute
                                  (Doc   => Cur_Sibling,
                                   XPath => "pci",
                                   Name  => "device");
                              Fun_Nr      : constant String
                                := Muxml.Utils.Get_Attribute
                                  (Doc   => Cur_Sibling,
                                   XPath => "pci",
                                   Name  => "function");
                           begin
                              if Dev_Nr'Length > 0 then

                                 --  Sibling has logical BDF already set in the
                                 --  policy. Remember assigned device number
                                 --  for later allocation step and reserve
                                 --  assigned function number.

                                 Device_Nr := Natural'Value (Dev_Nr);

                                 Utils.Reserve_Number
                                   (Allocator => Fun_Nr_Allocator,
                                    Number    => Natural'Value (Fun_Nr));
                              else

                                 --  Sibling has no logical BDF, store it in
                                 --  list of devices to allocate BDFs.

                                 DOM.Core.Append_Node
                                   (List => Devs_To_Allocate,
                                    N    => Cur_Sibling);
                              end if;
                           end;
                        end loop;

                        if Device_Nr = 0 then

                           --  Get next free device number if no sibling device
                           --  had a logical BDF assigned in the policy.

                           Utils.Allocate (Allocator => Dev_Nr_Allocator,
                                           Number    => Device_Nr);
                        end if;

                        for L in 0 .. DOM.Core.Nodes.Length
                          (List => Devs_To_Allocate) - 1
                        loop
                           declare
                              Alloc_Dev  : constant DOM.Core.Node
                                := DOM.Core.Nodes.Item
                                  (List  => Devs_To_Allocate,
                                   Index => L);
                              Log_Name   : constant String
                                := DOM.Core.Elements.Get_Attribute
                                  (Elem => Alloc_Dev,
                                   Name => "logical");
                              PCI_Node   : DOM.Core.Node;
                              Fun_Number : Natural;
                           begin
                              Utils.Allocate (Allocator => Fun_Nr_Allocator,
                                              Number    => Fun_Number);
                              Mulog.Log
                                (Msg => "Setting BDF of logical device '"
                                 & Log_Name & "' to 00:"
                                 & Mutools.Utils.To_Hex
                                   (Number     => Interfaces.Unsigned_64
                                        (Device_Nr),
                                    Normalize  => False,
                                    Byte_Short => True) & "."
                                 & Ada.Strings.Fixed.Trim
                                   (Source => Fun_Number'Img,
                                    Side   => Ada.Strings.Left));

                              PCI_Node := Mutools.PCI.Create_PCI_Node
                                (Policy => Data,
                                 Bus    => 0,
                                 Device => Mutools.PCI.Device_Range
                                   (Device_Nr),
                                 Func   => Mutools.PCI.Function_Range
                                   (Fun_Number));

                              PCI_Node := DOM.Core.Nodes.Insert_Before
                                (N         => Alloc_Dev,
                                 New_Child => PCI_Node,
                                 Ref_Child => DOM.Core.Nodes.First_Child
                                   (N => Alloc_Dev));
                           end;
                        end loop;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Add_Device_BDFs;

   -------------------------------------------------------------------------

   procedure Add_Device_Memory_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Devices_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices");
      Unmapped_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device/memory"
           & "[not (@virtualAddress)]");
      Count : constant Natural := DOM.Core.Nodes.Length
        (List => Unmapped_Memory);
   begin
      if Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Adding" & Count'Img & " mapping(s) for device "
                 & "memory");

      for I in 0 .. Count - 1 loop
         declare
            Memory_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Unmapped_Memory,
                 Index => I);
            Memory_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Memory_Node,
                 Name => "physical");
            Dev_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Memory_Node);
            Dev_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Node,
                 Name => "physical");
            Physmem_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   => Data.Doc,
                    XPath => "/system/hardware/devices/device[@name='"
                    & Dev_Ref & "']/memory[@name='" & Memory_Ref & "']",
                    Name  => "physicalAddress"));
            Mapping_Addr : Interfaces.Unsigned_64 := Physmem_Addr;
         begin
            if Mutools.XML_Utils.Is_Physical_Mmconf_Region
              (Devices_Node => Devices_Node,
               Addr         => Physmem_Addr)
            then
               Mapping_Addr := Mutools.XML_Utils.Calculate_PCI_Cfg_Address
                 (Base_Address => MC.Subject_PCI_Config_Space_Addr,
                  PCI_Node     => Muxml.Utils.Get_Element
                    (Doc   => Dev_Node,
                     XPath => "pci"));
            end if;

            DOM.Core.Elements.Set_Attribute
              (Elem  => Memory_Node,
               Name  => "virtualAddress",
               Value => Mutools.Utils.To_Hex (Number => Mapping_Addr));
         end;
      end loop;
   end Add_Device_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Devices : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices");
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[*]");
      Subj_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device[not(*)"
           & " or (count(*)=1 and pci)]");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Subj_Devs) loop
         declare
            use type DOM.Core.Node;

            Subj_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subj_Devs,
                                      Index => I - 1);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Subj_Dev,
                    Level => 2),
                 Name => "name");
            Log_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "logical");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "physical");
            Phys_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
         begin
            if Phys_Dev /= null then
               Mulog.Log (Msg => "Adding device resources of physical device '"
                          & Phys_Name & "' to logical device '" & Log_Name
                          & "' of subject '" & Subj_Name & "'");
               declare
                  Subj_Dev_PCI : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Subj_Dev,
                       XPath => "pci");
                  Phys_Resources : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Phys_Dev,
                       XPath => "memory|irq|ioPort");
                  Phys_Res_Count : constant Natural
                    := DOM.Core.Nodes.Length (List => Phys_Resources);
                  Mmconf_Base : constant
                    := MC.Subject_PCI_Config_Space_Addr;
               begin
                  for J in 1 .. Phys_Res_Count loop
                     Mutools.XML_Utils.Add_Resource
                       (Logical_Device         => Subj_Dev,
                        Physical_Resource      => DOM.Core.Nodes.Item
                          (List  => Phys_Resources,
                           Index => J - 1),
                        Mmconf_Devices_Node    => Devices,
                        Mmconf_Device_PCI_Node => Subj_Dev_PCI,
                        Mmconf_Virt_Base       => Mmconf_Base);
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Add_Device_Resources;

   -------------------------------------------------------------------------

   procedure Add_Device_Vectors (Data : in out Muxml.XML_Data_Type)
   is
      PCI_MSI_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[pci/@msi='true']");
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[devices/device]");

      --  Allocate vectors for IRQs of the specified logical device using the
      --  given allocator. If 'Consecutive' is set to True, the IRQs are
      --  allocated consecutively.
      procedure Allocate_Vectors
        (Logical_Device :        DOM.Core.Node;
         Allocator      : in out Utils.Number_Allocator_Type;
         Consecutive    :        Boolean := False);

      ----------------------------------------------------------------------

      procedure Allocate_Vectors
        (Logical_Device :        DOM.Core.Node;
         Allocator      : in out Utils.Number_Allocator_Type;
         Consecutive    :        Boolean := False)
      is
         IRQs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Logical_Device,
              XPath => "irq[not(@vector)]");
         IRQ_Count : constant Natural := DOM.Core.Nodes.Length (List => IRQs);
      begin
         if Consecutive then
            declare
               Cur_Idx, Cur_End : Natural;
            begin
               Utils.Allocate_Range
                 (Allocator   => Allocator,
                  Range_Size  => IRQ_Count,
                  Range_Start => Cur_Idx,
                  Range_End   => Cur_End);
               for I in 0 .. IRQ_Count - 1 loop
                  declare
                     Cur_Irq : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => IRQs,
                          Index => I);
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Cur_Irq,
                        Name  => "vector",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Cur_Idx'Img,
                           Side   => Ada.Strings.Left));
                     Cur_Idx := Cur_Idx + 1;
                  end;
               end loop;

               pragma Assert
                 (Check   => Cur_Idx = Cur_End + 1,
                  Message => "Vector range and IRQ count mismatch");
            end;
         else
            for I in 0 .. IRQ_Count - 1 loop
               declare
                  Cur_Irq    : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => IRQs,
                       Index => I);
                  Cur_Vector : Natural;
               begin
                  Utils.Allocate (Allocator => Allocator,
                                  Number    => Cur_Vector);
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Cur_Irq,
                     Name  => "vector",
                     Value => Ada.Strings.Fixed.Trim
                       (Source => Cur_Vector'Img,
                        Side   => Ada.Strings.Left));
               end;
            end loop;
         end if;
      end Allocate_Vectors;
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Subjects) loop
         declare
            use type Types.Subject_Profile_Type;

            Subject        : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I - 1);
            Subject_Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            Subj_Profile   : constant Types.Subject_Profile_Type
              := Types.Subject_Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subject,
                    Name => "profile"));
            Alloc_Devs     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq[not(@vector)]/..");
            Alloc_Count    : constant Natural
              := DOM.Core.Nodes.Length (List => Alloc_Devs);
            Device_Vectors : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq[@vector]");
            Event_Vectors  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "events/target/event/inject_interrupt");
            IRQ_Alloc      : Utils.Number_Allocator_Type
              (Range_Start => Subj_IRQ_Remap_Offset (Subj_Profile),
               Range_End   => Subj_IRQ_Remap_Offset (Subj_Profile) + 15);
            MSI_Alloc      : Utils.Number_Allocator_Type
              (Range_Start => Subj_MSI_Remap_Offset (Subj_Profile),
               Range_End   => 255);
         begin
            if Alloc_Count > 0 then
               Mulog.Log (Msg => "Allocating logical IRQ vector(s) for subject"
                          & " '" & Subject_Name & "'");

               Utils.Reserve_Numbers (Allocator => IRQ_Alloc,
                                      Nodes     => Device_Vectors,
                                      Attribute => "vector");
               Utils.Reserve_Numbers (Allocator => IRQ_Alloc,
                                      Nodes     => Event_Vectors,
                                      Attribute => "vector");
               Utils.Reserve_Numbers (Allocator => MSI_Alloc,
                                      Nodes     => Device_Vectors,
                                      Attribute => "vector");
               Utils.Reserve_Numbers (Allocator => MSI_Alloc,
                                      Nodes     => Event_Vectors,
                                      Attribute => "vector");

               if Subj_Profile = Types.Linux then

                  --  Reserve IRQ0 .. IRQ4 to avoid clashes with Linux legacy
                  --  device drivers.

                  for I in IRQ_Alloc.Range_Start .. IRQ_Alloc.Range_Start + 4
                  loop
                     Utils.Reserve_Number (Allocator => IRQ_Alloc,
                                           Number    => I);
                     Utils.Reserve_Number (Allocator => MSI_Alloc,
                                           Number    => I);
                  end loop;
               end if;

               for J in 1 .. DOM.Core.Nodes.Length (List => Alloc_Devs) loop
                  declare
                     use type DOM.Core.Node;

                     Cur_Dev : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Alloc_Devs,
                          Index => J - 1);
                     Phys_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Cur_Dev,
                          Name => "physical");
                     Phys_MSI_Dev : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => PCI_MSI_Devs,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Name);
                  begin
                     if Phys_MSI_Dev /= null then
                        Allocate_Vectors (Logical_Device => Cur_Dev,
                                          Allocator      => MSI_Alloc,
                                          Consecutive    => True);
                     else
                        Allocate_Vectors (Logical_Device => Cur_Dev,
                                          Allocator      => IRQ_Alloc);
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Device_Vectors;

   -------------------------------------------------------------------------

   procedure Add_Global_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[not (@globalId)]");
      Cur_ID : Positive := 1;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subj_Node,
               Name => "name");
            ID_Str    : constant String := Ada.Strings.Fixed.Trim
              (Source => Cur_ID'Img,
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting global ID of subject '" & Subj_Name
                       & "' to " & ID_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Subj_Node,
               Name  => "globalId",
               Value => ID_Str);
            Cur_ID := Cur_ID + 1;
         end;
      end loop;
   end Add_Global_IDs;

   -------------------------------------------------------------------------

   procedure Add_Local_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Subjects  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[not (@localId)]");
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);

      Cur_CPU_ID : array (Natural range 0 .. CPU_Count - 1) of Natural
        := (others => 0);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Cur_Subj  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Subj,
               Name => "name");
            Cur_CPU   : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Subj,
                  Name => "cpu"));
            Cur_ID    : constant String
              := Ada.Strings.Fixed.Trim
                (Source => Cur_CPU_ID (Cur_CPU)'Img,
                 Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting local ID of subject '" & Subj_Name
                       & "' to " & Cur_ID);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Cur_Subj,
               Name  => "localId",
               Value => Cur_ID);

            Cur_CPU_ID (Cur_CPU) := Cur_CPU_ID (Cur_CPU) + 1;
         end;
      end loop;
   end Add_Local_IDs;

   -------------------------------------------------------------------------

   procedure Add_Missing_Elements (Data : in out Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            VCPU_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "vcpu");
         begin
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "devices",
               Ref_Names  => (1 => To_Unbounded_String ("events")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "memory",
               Ref_Names  => (1 => To_Unbounded_String ("devices")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "bootparams",
               Ref_Names  => (1 => To_Unbounded_String ("memory")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "channels",
               Ref_Names  => (1 => To_Unbounded_String ("monitor"),
                              2 => To_Unbounded_String ("component")));

            if VCPU_Node = null then
               Muxml.Utils.Add_Child
                 (Parent     => Subj_Node,
                  Child_Name => "vcpu",
                  Ref_Names  => (1 => To_Unbounded_String ("bootparams")));
            end if;
         end;
      end loop;
   end Add_Missing_Elements;

   -------------------------------------------------------------------------

   procedure Add_Mugensched_Idle_Subjects (Data : in out Muxml.XML_Data_Type)
   is
      package Unbounded_Set_Package is new Ada.Containers.Ordered_Sets
        (Element_Type => Unbounded_String);

      Processed : Unbounded_Set_Package.Set;

      Auto_Idle : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame"
           & "[starts-with(@subject,'mugenschedcfg_auto_idle_')]");
      Subjects_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Auto_Idle);
   begin
      if Count > 0 then
         Mulog.Log (Msg => "Adding idle subject(s) for Mugenschedcfg-generated"
                    & " scheduling plan");
         for I in 0 .. DOM.Core.Nodes.Length (List => Auto_Idle) - 1 loop
            declare

               --  Add idle subject with given name.
               procedure Add_Subject (Name : String);

               -------------------------------------------------------------

               procedure Add_Subject (Name : String)
               is
                  N1, N2 : DOM.Core.Node;
               begin
                  N1 := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "subject");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => N1,
                     Name  => "name",
                     Value => Name);
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => N1,
                     Name  => "profile",
                     Value => "native");
                  N1 := DOM.Core.Nodes.Append_Child
                    (N         => Subjects_Node,
                     New_Child => N1);

                  N2 := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "vcpu");
                  Muxml.Utils.Append_Child
                    (Node      => N1,
                     New_Child => N2);
                  Mucfgvcpu.Set_VCPU_Profile
                    (Profile => Mucfgvcpu.Native,
                     Node    => N2);

                  Muxml.Utils.Add_Child
                    (Parent     => N1,
                     Child_Name => "bootparams");

                  N2 := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "memory");
                  N2 := DOM.Core.Nodes.Append_Child
                    (N         => N1,
                     New_Child => N2);

                  Muxml.Utils.Append_Child
                    (Node      => N2,
                     New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => "binary",
                        Physical_Name => Name & "|bin",
                        Address       => "16#1000#",
                        Writable      => True,
                        Executable    => True));

                  Muxml.Utils.Add_Child
                    (Parent     => N1,
                     Child_Name => "devices");
                  Muxml.Utils.Add_Child
                    (Parent     => N1,
                     Child_Name => "events");

                  Mutools.XML_Utils.Add_Memory_Region
                    (Policy      => Data,
                     Name        => Name & "|bin",
                     Address     => "",
                     Size        => "16#7000#",
                     Caching     => "WB",
                     Alignment   => "16#1000#",
                     Memory_Type => "subject_binary",
                     File_Name   => "idle",
                     File_Offset => "none");
               end Add_Subject;

               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Auto_Idle,
                    Index => I);
               Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "subject");
            begin
               if not Processed.Contains (Item => To_Unbounded_String (Name))
               then
                  Add_Subject (Name => Name);
                  Processed.Insert (New_Item => To_Unbounded_String (Name));
               end if;
            end;
         end loop;
      end if;
   end Add_Mugensched_Idle_Subjects;

   -------------------------------------------------------------------------

   procedure Add_Sched_Group_Info_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      package MXU renames Mutools.XML_Utils;
      use type Interfaces.Unsigned_64;

      Sched_Info_Virtual_Address : constant String := Mutools.Utils.To_Hex
        (Number => Config.Subject_Info_Virtual_Addr +
           Expanders.Config.Subject_Sinfo_Region_Size);

      Subject_To_Group_Map : constant MXU.ID_Map_Array
        := MXU.Get_Subject_To_Scheduling_Group_Map (Data => Data);
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            Subj_ID : constant Natural
              := Natural'Value
                (DOM.Core.Elements.Get_Attribute
                     (Elem => Subject,
                      Name => "globalId"));
            Group_ID_Str : constant String
              := Ada.Strings.Fixed.Trim
                (Source => Subject_To_Group_Map (Subj_ID)'Img,
                 Side   => Ada.Strings.Left);
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subject,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Adding mapping of scheduling group "
                       &  Group_ID_Str & " info region to subject '"
                       & Subj_Name & "'");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "sched_group_info",
                  Physical_Name => "scheduling_group_info_" & Group_ID_Str,
                  Address       => Sched_Info_Virtual_Address,
                  Writable      => False,
                  Executable    => False));
         end;
      end loop;
   end Add_Sched_Group_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Sinfo_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Adding info region for subject '"
                       & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|sinfo",
               Address     => "",
               Size        => Mutools.Utils.To_Hex
                 (Number => Expanders.Config.Subject_Sinfo_Region_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "subject_info",
               File_Name   => Subj_Name & "_sinfo",
               File_Offset => "none");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "sinfo",
                  Physical_Name => Subj_Name & "|sinfo",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Config.Subject_Info_Virtual_Addr),
                  Writable      => False,
                  Executable    => False));
         end;
      end loop;
   end Add_Sinfo_Regions;

   -------------------------------------------------------------------------

   procedure Add_Target_Event_IDs  (Data : in out Muxml.XML_Data_Type)
   is
      Subjects  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[events/target/event]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Events    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "events/target/event");
            Cur_ID    : Natural := 0;
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
               declare
                  Ev_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Events,
                       Index => J);
                  Ev_Name :  constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ev_Node,
                       Name => "logical");
                  ID_Str  : constant String
                    := Ada.Strings.Fixed.Trim
                      (Source => Cur_ID'Img,
                       Side   => Ada.Strings.Left);
               begin
                  Mulog.Log (Msg => "Setting id of target event '" & Ev_Name
                             & "' of subject '" & Subj_Name & "' to "
                             & ID_Str);
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Ev_Node,
                     Name  => "id",
                     Value => ID_Str);
                  Cur_ID := Cur_ID + 1;
               end;
            end loop;
         end;
      end loop;
   end Add_Target_Event_IDs;

   -------------------------------------------------------------------------

   procedure Add_Tau0 (Data : in out Muxml.XML_Data_Type)
   is
      Tau0_CPU : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/"
           & "minorFrame[@subject='tau0']/..",
           Name  => "id");
      Subjects_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects");
      Tau0_Node : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "subject");
      Mem_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
   begin
      Mulog.Log (Msg => "Adding tau0 subject");

      Tau0_Node := DOM.Core.Nodes.Insert_Before
        (N         => Subjects_Node,
         New_Child => Tau0_Node,
         Ref_Child => DOM.Core.Nodes.First_Child (N => Subjects_Node));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "globalId",
         Value => "0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "name",
         Value => "tau0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "profile",
         Value => "native");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "cpu",
         Value => Tau0_CPU);

      declare
         VCPU_Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "vcpu");
      begin
         Muxml.Utils.Append_Child
           (Node      => Tau0_Node,
            New_Child => VCPU_Node);
         Mucfgvcpu.Set_VCPU_Profile
           (Profile => Mucfgvcpu.Native,
            Node    => VCPU_Node);
      end;

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "bootparams"));

      Muxml.Utils.Append_Child
        (Node      => Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "sys_interface",
            Physical_Name => "sys_interface",
            Address       => Mutools.Utils.To_Hex
              (Number => Expanders.Config.Tau0_Interface_Virtual_Addr),
            Writable      => True,
            Executable    => False));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => Mem_Node);

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "devices"));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "events"));

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "tau0|bin",
         Address     => "",
         Size        => "16#0001_4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "tau0",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "binary",
            Physical_Name => "tau0|bin",
            Address       => "16#1000#",
            Writable      => True,
            Executable    => True));
   end Add_Tau0;

   -------------------------------------------------------------------------

   procedure Add_Timed_Event_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Adding timed event page for subject '"
                       & Subj_Name & "'");

            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "timed_event",
                  Physical_Name => Subj_Name & "|timed_event",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Config.Subject_Timed_Event_Virtual_Addr),
                  Writable      => True,
                  Executable    => False));
         end;
      end loop;
   end Add_Timed_Event_Mappings;

   -------------------------------------------------------------------------

   procedure Handle_Loaders (Data : in out Muxml.XML_Data_Type)
   is
      package DCN renames DOM.Core.Nodes;
      package MXU renames Mutools.XML_Utils;

      Memory_Section : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Data.Doc,
                                    XPath => "/system/memory");
      File_Memory    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Memory_Section,
           XPath => "memory[file]");
      Subjects       : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
      Loader_Subjs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[monitor/loader]");
   begin
      for I in 0 .. DCN.Length (List => Loader_Subjs) - 1 loop
         declare
            Ldr_Subj_Node       : constant DOM.Core.Node
              := DCN.Item
                (List  => Loader_Subjs,
                 Index => I);
            Ldr_Subj_Name       : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ldr_Subj_Node,
                 Name => "name");
            Ldr_Mem_Node        : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Ldr_Subj_Node,
                 XPath => "memory");
            Loader_Nodes        : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Ldr_Subj_Node,
                 XPath => "monitor/loader");
            Current_Loader_Addr : Interfaces.Unsigned_64
              := Expanders.Config.Subject_Loader_Source_Base_Addr;
         begin
            for J in 0 .. DCN.Length (List => Loader_Nodes) - 1 loop
               declare
                  Ldr_Node        : constant DOM.Core.Node
                    := DCN.Item
                      (List  => Loader_Nodes,
                       Index => J);
                  Ldr_Addr        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                           (Elem => Ldr_Node,
                            Name => "virtualAddress"));
                  Ldr_Writable    : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                           (Elem => Ldr_Node,
                            Name => "writable"));
                  Loadee_Name     : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ldr_Node,
                       Name => "subject");
                  Loadee_Subj     : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Subjects,
                       Ref_Attr  => "name",
                       Ref_Value => Loadee_Name);
                  Loadee_Mem_Node : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Loadee_Subj,
                       XPath => "memory");
                  Loadee_Mappings : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Loadee_Mem_Node,
                       XPath => "memory");
               begin
                  for K in 0 .. DCN.Length
                    (List => Loadee_Mappings) - 1
                  loop
                     declare
                        use type Interfaces.Unsigned_64;

                        Map_Node        : constant DOM.Core.Node
                          := DCN.Item
                            (List  => Loadee_Mappings,
                             Index => K);
                        Map_Log_Name    : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Map_Node,
                             Name => "logical");
                        Map_Phys_Name   : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Map_Node,
                             Name => "physical");
                        Map_Addr        : constant Interfaces.Unsigned_64
                          := Interfaces.Unsigned_64'Value
                            (DOM.Core.Elements.Get_Attribute
                                 (Elem => Map_Node,
                                  Name => "virtualAddress"));
                        Map_Is_Writable : constant Boolean
                          := Boolean'Value
                            (DOM.Core.Elements.Get_Attribute
                                 (Elem => Map_Node,
                                  Name => "writable"));
                        Target_Name     : constant String
                          := Map_Phys_Name & "_dst";
                        Log_Name        : constant String
                          := (if Map_Log_Name = "sinfo" then
                                 "monitor_sinfo_" & Loadee_Name
                              else
                                 Loadee_Name & "_" & Map_Log_Name);
                        Virtual_Addr    : constant String
                          := Mutools.Utils.To_Hex
                            (Number => Ldr_Addr + Map_Addr);
                        Loader_Mapping  : constant DOM.Core.Node
                          := MXU.Create_Virtual_Memory_Node
                            (Policy        => Data,
                             Logical_Name  => Log_Name,
                             Physical_Name => Map_Phys_Name,
                             Address       => Virtual_Addr,
                             Writable      => Ldr_Writable,
                             Executable    => False);
                     begin
                        Mulog.Log
                          (Msg => "Mapping memory region '" & Map_Log_Name
                           & "' of subject '" & Loadee_Name & "' "
                           & (if Ldr_Writable then "writable" else "readable")
                           & " to virtual address " & Virtual_Addr
                           & " of loader subject '" & Ldr_Subj_Name & "'");

                        --  Map region into loader.

                        Muxml.Utils.Append_Child
                          (Node      => Ldr_Mem_Node,
                           New_Child => Loader_Mapping);

                        --  If writable and file-backed, create physical target
                        --  region and swap original mapping.

                        if Map_Is_Writable then
                           declare
                              use type DOM.Core.Node;

                              Phys_Mem : constant DOM.Core.Node
                                := Muxml.Utils.Get_Element
                                  (Nodes     => File_Memory,
                                   Ref_Attr  => "name",
                                   Ref_Value => Map_Phys_Name);
                           begin
                              if Phys_Mem /= null then
                                 declare
                                    Phys_Size       : constant String
                                      := DOM.Core.Elements.Get_Attribute
                                        (Elem => Phys_Mem,
                                         Name => "size");
                                    Target_Phys_Mem : constant DOM.Core.Node
                                      := MXU.Create_Memory_Node
                                        (Policy      => Data,
                                         Name        => Target_Name,
                                         Address     => "",
                                         Size        => Phys_Size,
                                         Caching     =>
                                           DOM.Core.Elements.Get_Attribute
                                             (Elem => Phys_Mem,
                                              Name => "caching"),
                                         Alignment   =>
                                           DOM.Core.Elements.Get_Attribute
                                             (Elem => Phys_Mem,
                                              Name => "alignment"),
                                         Memory_Type => "subject");
                                    Hash_Ref        : constant DOM.Core.Node
                                      := DOM.Core.Documents.Create_Element
                                        (Doc      => Data.Doc,
                                         Tag_Name => "hashRef");
                                    Mapping         : constant DOM.Core.Node
                                      := MXU.Create_Virtual_Memory_Node
                                        (Policy        => Data,
                                         Logical_Name  => Log_Name & "_src",
                                         Physical_Name => Map_Phys_Name,
                                         Address       => Mutools.Utils.To_Hex
                                           (Number => Current_Loader_Addr),
                                         Writable      => Ldr_Writable,
                                         Executable    => False);
                                 begin
                                    Mulog.Log
                                      (Msg => "Swapping file-backed source "
                                       & "region '" & Map_Phys_Name
                                       & "' with new target memory region '"
                                       & Target_Name & "'");

                                    Muxml.Utils.Append_Child
                                      (Node      => DCN.Insert_Before
                                         (N         => Memory_Section,
                                          New_Child => Target_Phys_Mem,
                                          Ref_Child => Phys_Mem),
                                       New_Child => Hash_Ref);
                                    DOM.Core.Elements.Set_Attribute
                                      (Elem  => Hash_Ref,
                                       Name  => "memory",
                                       Value => Map_Phys_Name);

                                    --  Map new target region into loadee.

                                    DOM.Core.Elements.Set_Attribute
                                      (Elem  => Map_Node,
                                       Name  => "physical",
                                       Value => Target_Name);

                                    --  Map new target region into loader.

                                    DOM.Core.Elements.Set_Attribute
                                      (Elem  => Loader_Mapping,
                                       Name  => "physical",
                                       Value => Target_Name);

                                    Muxml.Utils.Append_Child
                                      (Node      => Ldr_Mem_Node,
                                       New_Child => Mapping);

                                    Current_Loader_Addr := Current_Loader_Addr
                                      + Interfaces.Unsigned_64'Value
                                      (Phys_Size);
                                 end;
                              end if;
                           end;
                        end if;

                  --  Clear CR4.VMXE in loadee subject state.

                        declare
                           VMXE_Node : constant DOM.Core.Node
                             := Muxml.Utils.Get_Element
                               (Doc   => Loadee_Subj,
                                XPath => "vcpu/registers/cr4/VMXEnable");
                        begin
                           DCN.Normalize (N => VMXE_Node);
                           DCN.Set_Node_Value
                             (N     => DCN.First_Child (N => VMXE_Node),
                              Value => "0");
                        end;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Handle_Loaders;

   -------------------------------------------------------------------------

   procedure Handle_Monitors (Data : in out Muxml.XML_Data_Type)
   is
      Monitor_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/monitor");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Monitor_Nodes) - 1 loop
         declare
            Monitor_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Monitor_Nodes,
                 Index => I);
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Monitor_Node,
                 Level => 1);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");
            Refs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Monitor_Node,
                 XPath => "*[self::state or self::timed_event "
                 & "or self::interrupts]");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Refs) - 1 loop
               declare
                  Ref_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Refs,
                       Index => J);
                  Ref_Type : constant String
                    := DOM.Core.Elements.Get_Tag_Name (Elem => Ref_Node);
                  Monitored_Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ref_Node,
                       Name => "subject");
                  Address : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ref_Node,
                       Name => "virtualAddress");
                  Logical : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ref_Node,
                       Name => "logical");
                  Writable : constant Boolean := Boolean'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Ref_Node,
                        Name => "writable"));
               begin
                  Mulog.Log (Msg => "Mapping " & Ref_Type & " of subject '"
                             & Monitored_Subj_Name & "' "
                             & (if Writable then "writable" else "readable")
                             & " to virtual address " & Address
                             & " of subject '" & Subj_Name & "'");

                  Muxml.Utils.Append_Child
                    (Node      => Mem_Node,
                     New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => Logical,
                        Physical_Name => Monitored_Subj_Name & "|" & Ref_Type,
                        Address       => Address,
                        Writable      => Writable,
                        Executable    => False));
               end;
            end loop;
         end;
      end loop;
   end Handle_Monitors;

   -------------------------------------------------------------------------

   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type Mucfgvcpu.Profile_Type;

            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Profile : constant Types.Subject_Profile_Type
              := Types.Subject_Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subj,
                    Name => "profile"));
         begin
            case Profile is
               when Types.Native => null;
               when Types.VM     => null;
               when Types.Linux  =>
                  Profiles.Handle_Linux_Profile
                    (Data    => Data,
                     Subject => Subj);
            end case;

            DOM.Core.Elements.Remove_Attribute
              (Elem => Subj,
               Name => "profile");
         end;
      end loop;
   end Handle_Profile;

   -------------------------------------------------------------------------

   procedure Remove_Channel_Elements (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/channels/..");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I),
            Child_Name => "channels");
      end loop;
   end Remove_Channel_Elements;

   -------------------------------------------------------------------------

   procedure Remove_Monitors (Data : in out Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[monitor]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Subject_Node,
               Child_Name => "monitor");
         end;
      end loop;
   end Remove_Monitors;

end Expanders.Subjects;
