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

   -------------------------------------------------------------------------

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
            --  (1) extract all physical devices with the currently
            --  investigated APU capability
            Physical_APU_Dev : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/devices/device" &
                   "[capabilities/capability/@name='" & Ada.Characters.
                   Handling.To_Lower (APU_Device_Type'Image (I)) & "']");
         begin
            for K in 0 .. DOM.Core.Nodes.Length (Physical_APU_Dev) - 1 loop
               declare
                  --  (2) check if APU device is used by subject and extract
                  --  the virtual device node
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
      --  NOTE - the child bus address translation ranges are specified
      --  as (child-bus-address, parent-bus-address, length), i.c. no
      --  address translation is used for the AMBA APU child bus (c.f.
      --  official Xilinx ZCU104 device tree)
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_apu_ranges__",
         Content  => "ranges = <" &
           To_DTS_Cell (Value => 16#0000_0000#) & " " &
           To_DTS_Cell (Value => 16#0000_0000#) & " " &
           To_DTS_Cell (Value => APU_Last) & ">;");
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_apu_devices__",
         Content  => To_String (Source => APU_Buffer));
   end Add_APU_Devices;

   -------------------------------------------------------------------------

   procedure Generate_GIC_Node
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.muen_vgic_dsl);

      Register_Entry : Unbounded_String;
   begin
      DTS_Range_Register_Entry (Policy    => Policy,
                          Device    => Device,
                          DTS_Entry => Register_Entry,
                          DTS_Range => DTS_Range);

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__vgic_bus_base__",
         Content  => Mutools.Utils.To_Hex (Number     => DTS_Range.Base,
                                           Normalize  => False,
                                           Byte_Short => False));
      --  NOTE - because the Xilinx GIC device layout does not met the ARM
      --  GIC specification, the virtual memory for the CPU interface has
      --  to be split up; this has to be corrected for the DTS
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__vgic_registers__",
         Content  => "reg = <" &
           To_DTS_Cell (Value => DTS_Range.Base) & " " &
           To_DTS_Cell (Value => DTS_Range.Size) & ">;");

      Append (Source   => DTS_Entry,
              New_Item => Mutools.Templates.To_String (Template => Template));
   end Generate_GIC_Node;

end DTS.APU_Devices;
