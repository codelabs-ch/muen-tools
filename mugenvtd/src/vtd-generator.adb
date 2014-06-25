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

with Ada.Streams.Stream_IO;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.EPT;
with Paging.Layouts;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Utils;
with Mutools.XML_Utils;

with VTd.Tables;

package body VTd.Generator
is

   --  Write VT-d DMAR root table as specified by the policy to the given
   --  output directory.
   procedure Write_Root_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write device security domain pagetables as specified by the policy to
   --  the given output directory.
   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/platform/devices/device[starts-with"
           & "(string(@name),'iommu')]");
   begin
      if DOM.Core.Nodes.Length (List => IOMMUs) = 0 then
         Mulog.Log (Msg => "No IOMMU device found, not adding VT-d tables");
         return;
      end if;

      Write_Root_Table
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Write_Domain_Pagetables
        (Output_Dir => Output_Dir,
         Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/deviceDomains/domain");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Cur_Dom    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Domains,
                                      Index => I);
            Dom_Name   : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Dom,
               Name => "name");
            PT_Node    : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@type='system_pt' and "
               & "starts-with(string(@name),'" & Dom_Name & "')]");
            Filename   : constant String := Muxml.Utils.Get_Attribute
              (Doc   => PT_Node,
               XPath => "file",
               Name  => "filename");
            Tables_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PT_Node,
                    Name => "physicalAddress"));
            Memory      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Dom,
                 XPath => "memory/memory");
            Mem_Layout  : Paging.Layouts.Memory_Layout_Type (Levels => 3);
            File        : Ada.Streams.Stream_IO.File_Type;
         begin
            Mulog.Log (Msg => "Writing VT-d pagetable of device domain '"
                       & Dom_Name & "' to '" & Output_Dir & "/"
                       & Filename & "'");
            Paging.Layouts.Set_Large_Page_Support
              (Mem_Layout => Mem_Layout,
               State      => False);
            Paging.Layouts.Set_Address
              (Mem_Layout => Mem_Layout,
               Address    => Tables_Addr);

            for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
               declare
                  Mem_Node   : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Memory,
                                            Index => J);
                  Log_Name   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "logical");
                  Phys_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "physical");
                  Phys_Node  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Policy.Doc,
                       XPath => "/system/memory/memory[@name='" & Phys_Name
                       & "']");
                  Size       : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "size"));
                  PMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "physicalAddress"));
                  Mem_Type   : constant Paging.Caching_Type
                    := Paging.Caching_Type'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "caching"));
                  VMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "virtualAddress"));
                  Executable : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "executable"));
                  Writable   : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "writable"));
               begin
                  Mulog.Log (Msg => "Adding region " & Log_Name
                             & "[" & Phys_Name & "] to domain '"
                             & Dom_Name & "'");
                  Paging.Layouts.Add_Memory_Region
                    (Mem_Layout       => Mem_Layout,
                     Physical_Address => PMA,
                     Virtual_Address  => VMA,
                     Size             => Size,
                     Caching          => Mem_Type,
                     Writable         => Writable,
                     Executable       => Executable);
               end;
            end loop;

            Paging.Layouts.Update_References (Mem_Layout => Mem_Layout);

            Mutools.Files.Open (Filename => Output_Dir & "/" & Filename,
                                File     => File);
            Paging.Layouts.Serialize
              (Stream      => Ada.Streams.Stream_IO.Stream (File),
               Mem_Layout  => Mem_Layout,
               Serializers =>
                 (1 => Paging.EPT.Serialize_PDPT'Access,
                  2 => Paging.EPT.Serialize_PD'Access,
                  3 => Paging.EPT.Serialize_PT'Access));
            Ada.Streams.Stream_IO.Close (File => File);
         end;
      end loop;
   end Write_Domain_Pagetables;

   -------------------------------------------------------------------------

   procedure Write_Root_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      package MX renames Mutools.XML_Utils;

      Buses     : constant MX.PCI_Bus_Set.Set := MX.Get_Occupied_PCI_Buses
        (Data => Policy);
      Ctx_Pos   : MX.PCI_Bus_Set.Cursor       := Buses.First;
      Root      : Tables.Root_Table_Type;
      Root_File : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/memory/memory/file[@filename='vtd_root']"),
           Name => "filename");
   begin
      while MX.PCI_Bus_Set.Has_Element (Position => Ctx_Pos) loop
         declare
            Ctx_Bus   : constant MX.PCI_Bus_Range
              := MX.PCI_Bus_Set.Element (Position => Ctx_Pos);
            Bus_Str   : constant String
              := Mutools.Utils.To_Hex
                (Number    => Interfaces.Unsigned_64 (Ctx_Bus),
                 Normalize => False);
            Bus_Str_N : constant String
              := Mutools.Utils.To_Hex
                (Number     => Interfaces.Unsigned_64 (Ctx_Bus),
                 Byte_Short => True);
            Filename  : constant String := "vtd_context_bus_" & Bus_Str;
            Mem_Node  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/memory/memory/file[@filename='"
                 & Filename & "']/..");
            Ctx_Addr  : constant Tables.Table_Pointer_Type
              := Tables.Table_Pointer_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "physicalAddress"));
         begin
            Mulog.Log (Msg => "Adding root entry for PCI bus " & Bus_Str_N
                       & ": " & Mutools.Utils.To_Hex
                         (Number => Interfaces.Unsigned_64 (Ctx_Addr)));
            Tables.Add_Entry (RT  => Root,
                              Bus => Tables.Table_Index_Type (Ctx_Bus),
                              CTP => Ctx_Addr);
         end;

         MX.PCI_Bus_Set.Next (Position => Ctx_Pos);
      end loop;

      Mulog.Log (Msg => "Writing VT-d root table to file '" & Output_Dir & "/"
                 & Root_File & "'");
      Tables.Serialize (RT       => Root,
                        Filename => Output_Dir & "/" & Root_File);
   end Write_Root_Table;

end VTd.Generator;
