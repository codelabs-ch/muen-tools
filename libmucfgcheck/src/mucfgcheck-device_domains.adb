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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Types;
with Mutools.Utils;

package body Mucfgcheck.Device_Domains
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Device_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/deviceDomains/domain/devices/device");

      --  Check inequality of device reference physical names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "physical");
      begin
         if Left_Name = Right_Name then
            declare
               L_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Left,
                                                     Level => 2),
                  Name => "name");
               R_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Right,
                                                     Level => 2),
                  Name => "name");
            begin
               raise Validation_Error with "Device domains '" & L_Dom_Name
                 & "' and '" & R_Dom_Name & "' "
                 & "reference same physical device '" & Left_Name & "'";
            end;
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " security domain device reference(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Device_Reference_Uniqueness;

   -------------------------------------------------------------------------

   procedure Domain_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Domain   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I);
            Dom_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Domain,
                 Name => "name");
            Memory   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Domain,
                 XPath => "memory/memory");
         begin
            if DOM.Core.Nodes.Length (List => Memory) > 1 then
               for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
                  Set_Size
                    (Virtual_Mem_Node => DOM.Core.Nodes.Item
                       (List  => Memory,
                        Index => J),
                     Ref_Nodes_Path   => "/system/memory/memory",
                     XML_Data         => XML_Data);
               end loop;

               Check_Memory_Overlap
                 (Nodes        => Memory,
                  Region_Type  => "domain memory region",
                  Address_Attr => "virtualAddress",
                  Name_Attr    => "logical",
                  Add_Msg      => " of device domain '" & Dom_Name & "'");
            end if;
         end;
      end loop;
   end Domain_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure Domain_Memory_Type (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain/memory/memory");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      Mulog.Log (Msg => "Checking memory type of" & Count'Img
                 & " security domain memory reference(s)");
      for I in 0 .. Count - 1 loop
         declare
            use type Mutools.Types.Memory_Kind;

            Dom_Mem   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dom_Mem,
                 Name => "physical");
            Phys_Mem  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => XML_Data.Doc,
                 XPath => "/system/memory/memory[@name='" & Phys_Name & "']");
            Mem_Type  : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "type"));
         begin
            if Mem_Type /= Mutools.Types.Subject then
               raise Validation_Error with "Device domain memory '"
                 & Phys_Name & "' has invalid memory type " & Mem_Type'Img;
            end if;
         end;
      end loop;
   end Domain_Memory_Type;

   -------------------------------------------------------------------------

   procedure Domain_PT_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Domains   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain");
      Dom_Count : constant Natural := DOM.Core.Nodes.Length (List => Domains);
   begin
      Mulog.Log (Msg => "Checking presence of" & Dom_Count'Img
                 & " security domain PT memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            use type DOM.Core.Node;

            Dom_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Domains,
                                      Index => I);
            Dom_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dom_Node,
                 Name => "name");
            PT_Node  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => XML_Data.Doc,
                 XPath => "/system/memory/memory[@type='system_pt' and "
                 & "contains(string(@name),'" & Dom_Name & "')]/file");
         begin
            if PT_Node = null then
               raise Validation_Error with "No file-backed PT region for "
                 & "device domain '" & Dom_Name & "' found";
            end if;
         end;
      end loop;
   end Domain_PT_Region_Presence;

   -------------------------------------------------------------------------

   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Domains   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain");
      Dom_Count : constant Natural := DOM.Core.Nodes.Length (List => Domains);
   begin
      if Dom_Count > 0 then
         Mulog.Log (Msg => "Checking presence of IOMMU device(s)");
         declare
            IOMMUs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "/system/platform/devices/"
                 & "device[starts-with(@name,'iommu')]");
         begin
            if DOM.Core.Nodes.Length (List => IOMMUs) = 0 then
               raise Validation_Error with "Device domains specified but no"
                 & " IOMMU device provided by platform";
            end if;
         end;
      end if;
   end IOMMU_Presence;

   -------------------------------------------------------------------------

   procedure Memory_Mapping_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain/memory/memory");
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " security domain memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Dom_Mem      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Phys_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dom_Mem,
                 Name => "physical");
            Dom_Mem_Addr : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dom_Mem,
                 Name => "virtualAddress");
            Mem_Refs     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "/system/subjects/subject/memory/memory[@physical='"
                 & Phys_Name & "']");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Mem_Refs) - 1 loop
               declare
                  Mem_Ref      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Mem_Refs,
                       Index => J);
                  Mem_Ref_Addr : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Ref,
                       Name => "virtualAddress");
               begin
                  if Dom_Mem_Addr /= Mem_Ref_Addr then
                     declare
                        Dom_Name  : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Muxml.Utils.Ancestor_Node
                               (Node  => Dom_Mem,
                                Level => 2),
                             Name => "name");
                        Subj_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Muxml.Utils.Ancestor_Node
                               (Node  => Mem_Ref,
                                Level => 2),
                             Name => "name");
                     begin
                        raise Validation_Error with "Physical memory region '"
                          & Phys_Name & "' referenced by device domain '"
                          & Dom_Name  & "' and subject '" & Subj_Name & "' not"
                          & " mapped at the same address: " & Dom_Mem_Addr
                          & " /= " & Mem_Ref_Addr;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Memory_Mapping_Address_Equality;

   -------------------------------------------------------------------------

   procedure Memory_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/deviceDomains/domain/memory/memory");

      --  Check inequality of memory reference physical names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "physical");
      begin
         if Left_Name = Right_Name then
            declare
               L_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Left,
                                                     Level => 2),
                  Name => "name");
               R_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Right,
                                                     Level => 2),
                  Name => "name");
            begin
               raise Validation_Error with "Device domains '" & L_Dom_Name
                 & "' and '" & R_Dom_Name & "' "
                 & "reference same physical memory region '" & Left_Name & "'";
            end;
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " security domain memory reference(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Memory_Reference_Uniqueness;

   -------------------------------------------------------------------------

   procedure PCI_Bus_Context_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Devs  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Devs);
   begin
      if Count > 0 then
         Mulog.Log (Msg => "Checking presence of VT-d context table memory "
                    & "region(s)");

         for I in 0 .. Count - 1 loop
            declare
               use type DOM.Core.Node;

               Dev_Ref    : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Devs,
                                         Index => I);
               Dev_Name   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Ref,
                    Name => "physical");
               Bus_Nr     : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (Muxml.Utils.Get_Attribute
                      (Doc   => XML_Data.Doc,
                       XPath => "/system/platform/devices/device[@name='"
                       & Dev_Name & "']/pci",
                       Name  => "bus"));
               Bus_Nr_Hex : constant String := Mutools.Utils.To_Hex
                 (Number    => Bus_Nr,
                  Normalize => False);
               Ctx_Node   : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => XML_Data.Doc,
                    XPath => "/system/memory/memory"
                    & "[@type='system_vtd_context']"
                    & "/file[@filename='vtd_context_bus_" & Bus_Nr_Hex & "']");
            begin
               if Ctx_Node = null then
                  raise Validation_Error with "No file-backed VT-d context "
                    & "table memory region found for PCI bus "
                    & Mutools.Utils.To_Hex (Number     => Bus_Nr,
                                            Byte_Short => True);
               end if;
            end;
         end loop;
      end if;
   end PCI_Bus_Context_Region_Presence;

   -------------------------------------------------------------------------

   procedure PCI_Device_Domain_Assignment (XML_Data : Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain");
      Devs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/devices/device[pci]");
   begin
      if DOM.Core.Nodes.Length (List => Domains) > 0 then
         Mulog.Log (Msg => "Checking security domain assignment of"
                    & DOM.Core.Nodes.Length (List => Devs)'Img
                    & " PCI device(s)");

         for I in 0 .. DOM.Core.Nodes.Length (List => Devs) - 1 loop
            declare
               use type DOM.Core.Node;

               Dev      : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Devs,
                                         Index => I);
               Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev,
                    Name => "name");
               Subj_Dev : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => XML_Data.Doc,
                    XPath => "/system/subjects/subject/devices/device"
                    & "[@physical='" & Dev_Name & "']");
            begin
               if Subj_Dev /= null then
                  declare
                     Dom_Dev : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Doc   => XML_Data.Doc,
                          XPath => "/system/deviceDomains/domain/devices/"
                          & "device[@physical='" & Dev_Name & "']");
                  begin
                     if Dom_Dev = null then
                        declare
                           Subj_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Muxml.Utils.Ancestor_Node
                                  (Node  => Subj_Dev,
                                   Level => 2),
                                Name => "name");
                        begin
                           raise Validation_Error with "PCI device '"
                             & Dev_Name & "' referenced by subject '"
                             & Subj_Name & "' is not assigned to any device "
                             & "domain";
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end if;
   end PCI_Device_Domain_Assignment;

   -------------------------------------------------------------------------

   procedure PCI_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant  DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      Mulog.Log (Msg => "Checking type of" & Count'Img
                 & " security domain device reference(s)");

      for I in 0 .. Count - 1 loop
         declare
            use type DOM.Core.Node;

            Dev_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Node,
                 Name => "physical");
            Phys_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => XML_Data.Doc,
                 XPath => "/system/platform/devices/device[@name='" & Dev_Name
                 & "']");
            PCI_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Phys_Dev,
                 XPath => "pci");
         begin
            if PCI_Node = null then
               declare
                  Dom_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node (Node  => Dev_Node,
                                                          Level => 2),
                       Name => "name");
               begin
                  raise Validation_Error with "Physical device '" & Dev_Name
                    & "' referenced by device domain '" & Dom_Name  & "' is "
                    & "not a PCI device";
               end;
            end if;
         end;
      end loop;
   end PCI_Device_References;

end Mucfgcheck.Device_Domains;
