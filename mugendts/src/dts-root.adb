--
--  Copyright (C) 2023, 2023  David Loosli <david@codelabs.ch>
--  Copyright (C) 2024, 2024  Tobias Brunner <tobias@codelabs.ch>
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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Types;
with Mutools.Utils;
with Mutools.XML_Utils;
with Muxml.Utils;

with String_Templates;

with DTS.APU_Devices;
with DTS.SoC_Devices;

package body DTS.Root
is

   -------------------------------------------------------------------------

   procedure Add_Aliases_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      pragma Unreferenced (Policy, Subject);
   begin
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__serial_alias__",
         Content  => "serial_0");
   end Add_Aliases_Node;

   -------------------------------------------------------------------------

   procedure Add_Chosen_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      Phys_Initrd_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_initrd']");

      Subject_Memory  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "memory/memory");

      Initrd_Address, Initrd_Size : Interfaces.Unsigned_64 := 0;
   begin
      Mutools.XML_Utils.Get_Addr_And_Size
        (Virtual_Mappings => Subject_Memory,
         Physical_Memory  => Phys_Initrd_Mem,
         Virtual_Address  => Initrd_Address,
         Size             => Initrd_Size);

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__chosen_bootparams__",
         Content  => Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "bootparams")
            & " initrd=0x" & Mutools.Utils.To_Hex
              (Number => Initrd_Address,
               Normalize  => False)
            & ",0x" & Mutools.Utils.To_Hex
              (Number => Initrd_Size,
               Normalize  => False));
   end Add_Chosen_Node;

   -------------------------------------------------------------------------

   procedure Add_Memory_Nodes
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      Physical_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");

      Subject_Memory  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "memory/memory");

      Register_Offset : constant String
        :=  Mutools.Utils.Indent (15, 1);
      Reserved_Offset : constant String
        :=  Mutools.Utils.Indent (19, 1);

      Base_Address    : Unsigned_64 := Unsigned_64'Last;
      Register_Ranges : Unbounded_String;
      Reserved_Base   : Unsigned_64 := Unsigned_64'Last;
      Reserved_Ranges : Unbounded_String;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (Subject_Memory) - 1 loop
         declare
            Virtual_Memory_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subject_Memory,
                                      Index => I);
            Virtual_Memory_Base : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Virtual_Memory_Node,
                                                  Name => "virtualAddress");
            Virtual_Memory_Phys : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Virtual_Memory_Node,
                                                  Name => "physical");

            Physical_Memory_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element (Nodes     => Physical_Memory,
                                          Ref_Attr  => "name",
                                          Ref_Value => Virtual_Memory_Phys);
            Physical_Memory_Type : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Physical_Memory_Node,
                                                  Name => "type"));
            Physical_Memory_Size : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Physical_Memory_Node,
                                                  Name => "size");
         begin
            --  The kernel image is added to the usable memory as according to
            --  Documentation/arch/arm64/booting.rst the image must be placed
            --  "anywhere in usable system RAM" and the kernel does access some
            --  of it via virtual addresses (e.g. the alternatives framework),
            --  which won't work if it doesn't create page tables for it.
            if
              Physical_Memory_Type in Mutools.Types.Subject_RAM_Memory or
              Physical_Memory_Type in Mutools.Types.Subject_Binary
            then
               if Unsigned_64'Value (Virtual_Memory_Base) < Base_Address then
                  Base_Address := Unsigned_64'Value (Virtual_Memory_Base);
               end if;

               if Length (Register_Ranges) > 0 then
                  Append (Source   => Register_Ranges,
                          New_Item => ASCII.LF & Register_Offset);
               end if;

               Append (Source   => Register_Ranges,
                       New_Item => To_DTS_Cell (Unsigned_64'Value
                         (Virtual_Memory_Base)) & " " & To_DTS_Cell
                       (Unsigned_64'Value (Physical_Memory_Size)));
            end if;

            --  The kernel image is marked as reserved memory, so it won't get
            --  used by the kernel. In particular for DMA, so we don't have to
            --  map it into the SMMU.
            if Physical_Memory_Type in Mutools.Types.Subject_Binary then
               if Unsigned_64'Value (Virtual_Memory_Base) < Reserved_Base then
                  Reserved_Base := Unsigned_64'Value (Virtual_Memory_Base);
               end if;

               if Length (Reserved_Ranges) > 0 then
                  Append (Source   => Reserved_Ranges,
                          New_Item => ASCII.LF & Reserved_Offset);
               end if;

               Append (Source   => Reserved_Ranges,
                       New_Item => To_DTS_Cell (Unsigned_64'Value
                         (Virtual_Memory_Base)) & " " & To_DTS_Cell
                       (Unsigned_64'Value (Physical_Memory_Size)));
            end if;
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__memory_base__",
         Content  => Mutools.Utils.To_Hex (Number     => Base_Address,
                                           Normalize  => False,
                                           Byte_Short => False));

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__memory_registers__",
         Content  => To_String (Source => "reg = <" & Register_Ranges & ">;"));

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__reserved_base__",
         Content  => Mutools.Utils.To_Hex (Number     => Reserved_Base,
                                           Normalize  => False,
                                           Byte_Short => False));

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__reserved_registers__",
         Content  => To_String (Source => "reg = <" & Reserved_Ranges & ">;"));
   end Add_Memory_Nodes;

   -------------------------------------------------------------------------

   procedure Write
     (Policy       : Muxml.XML_Data_Type;
      Subject      : DOM.Core.Node;
      Subject_Name : String;
      Filename     : String)
   is
      pragma Unreferenced (Subject_Name);
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.devicetree_dsl);
   begin
      Add_Aliases_Node (Template => Template,
                        Policy   => Policy,
                        Subject  => Subject);

      Add_Chosen_Node (Template => Template,
                       Policy   => Policy,
                       Subject  => Subject);

      Add_Memory_Nodes (Template => Template,
                        Policy   => Policy,
                        Subject  => Subject);

      DTS.APU_Devices.Add_APU_Devices (Template => Template,
                                       Policy   => Policy,
                                       Subject  => Subject);

      DTS.SoC_Devices.Add_SoC_Devices (Template => Template,
                                       Policy   => Policy,
                                       Subject  => Subject);

      Mutools.Templates.Write (Template => Template,
                               Filename => Filename);
   end Write;

end DTS.Root;
