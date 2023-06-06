--
--  Copyright (C) 2023, 2023  David Loosli <david@codelabs.ch>
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

with Ada.Characters.Handling;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Utils;

with String_Templates;

package body DTS.APU_Devices
is

   -----------------------
   --  Add_APU_Devices  --
   -----------------------
   procedure Add_APU_Devices
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      APU_Buffer : Unbounded_String;
      APU_First  : Unsigned_64 := Unsigned_64'Last;
      APU_Last   : Unsigned_64 := 16#0#;
   begin
      for I in APU_Device_Type'Range loop
         declare
            --  (1) extract all physical devices with the currently       --
            --      investigated APU capability                           --
            Physical_APU_Dev : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/devices/device" &
                   "[capabilities/capability/@name='" & Ada.Characters.
                   Handling.To_Lower (APU_Device_Type'Image (I)) & "']");
         begin
            for K in 0 .. DOM.Core.Nodes.Length (Physical_APU_Dev) - 1 loop
               declare
                  --  (2) check if APU device is used by subject and      --
                  --      extract the virtual device node                 --
                  Virtual_APU_Dev : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Subject,
                       XPath => "devices/device[@physical='" & DOM.Core.
                         Elements.Get_Attribute
                           (Elem => DOM.Core.Nodes.Item
                              (List  => Physical_APU_Dev,
                               Index => K),
                            Name => "name") & "']");

                  Virtual_Dev_Entry : Unbounded_String;
                  Virtual_Dev_Range : DTS_Range_Type;
               begin
                  if DOM.Core.Nodes.Length (Virtual_APU_Dev) = 1 then
                     case I is
                        when GIC =>
                           Generate_GIC_Node (Policy    => Policy,
                                              Device    => DOM.Core.Nodes.Item
                                                (List  => Virtual_APU_Dev,
                                                 Index => 0),
                                              DTS_Entry => Virtual_Dev_Entry,
                                              DTS_Range => Virtual_Dev_Range);
                     end case;

                     if Virtual_Dev_Range.Base < APU_First then
                        APU_First := Virtual_Dev_Range.Base;
                     end if;

                     if APU_Last < Virtual_Dev_Range.Base then
                        APU_Last := Virtual_Dev_Range.Base +
                          Virtual_Dev_Range.Size;
                     end if;

                     Block_Indent (Block     => Virtual_Dev_Entry,
                                   N         => 2,
                                   Unit_Size => 4);
                     Append (Source   => APU_Buffer,
                             New_Item => ASCII.LF & Virtual_Dev_Entry);
                  end if;
               end;
            end loop;
         end;
      end loop;
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_apu_base__",
         Content  => Mutools.Utils.To_Hex (Number     => APU_First,
                                           Normalize  => False,
                                           Byte_Short => False));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_apu_ranges__",
         Content  => "ranges = <0x0 0x0 0x0 0x0 0x" & Mutools.Utils.
           To_Hex (Number     => APU_Last,
                   Normalize  => False,
                   Byte_Short => False) & ">;");
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_apu_devices__",
         Content  => To_String (Source => APU_Buffer));
   end Add_APU_Devices;

   -----------------------
   --  Add_Memory_Node  --
   -----------------------
   procedure Generate_GIC_Node
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.muen_vgic_dsl);

      --  (1) extract all virtual device memory regions (i.c. vGIC)       --
      Virtual_GIC_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Device,
           XPath => "memory");

      Physical_GIC_Name : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Device,
                                            Name => "physical");

      Base_Address : Unsigned_64 := Unsigned_64'Last;
      Range_Size   : Unsigned_64 := 16#0#;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (Virtual_GIC_Memory) - 1 loop
         declare
            -- (2) extract corresponding physical memory node (i.c. GIC)  --
            Physical_GIC_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/devices/device[@name='" &
                   Physical_GIC_Name & "']/memory[@name='" & DOM.Core.
                   Elements.Get_Attribute
                     (Elem => DOM.Core.Nodes.Item
                        (List  => Virtual_GIC_Memory,
                         Index => I),
                      Name => "physical") & "']");
         begin
            if DOM.Core.Nodes.Length (Physical_GIC_Memory) = 1 then
               declare
                  Memory_Address : constant Unsigned_64
                    := Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => DOM.Core.Nodes.Item
                            (List  => Virtual_GIC_Memory,
                             Index => I),
                          Name => "virtualAddress"));
                  Memory_Size : constant Unsigned_64
                    := Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => DOM.Core.Nodes.Item
                            (List  => Physical_GIC_Memory,
                             Index => 0),
                          Name => "size"));
               begin
                  if Memory_Address < Base_Address then
                     Base_Address := Memory_Address;
                  end if;
                  Range_Size := Range_Size + Memory_Size;
               end;
            end if;
         end;
      end loop;
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__vgic_bus_base__",
         Content  => Mutools.Utils.To_Hex (Number     => Base_Address,
                                           Normalize  => False,
                                           Byte_Short => False));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__vgic_registers__",
         Content  => "reg = <" & To_DTS_Cell (Value => Base_Address) &
           " 0x" & Mutools.Utils.To_Hex (Number     => Range_Size,
                                         Normalize  => False,
                                         Byte_Short => False) & ">;");
      Append (Source   => DTS_Entry,
              New_Item => Mutools.Templates.To_String (Template => Template));
      DTS_Range := (Base => Base_Address,
                    Size => Range_Size);
   end Generate_GIC_Node;

end DTS.APU_Devices;
