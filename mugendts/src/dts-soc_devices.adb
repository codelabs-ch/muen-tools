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

package body DTS.SoC_Devices
is

   -----------------------
   --  Add_SoC_Devices  --
   -----------------------
   procedure Add_SoC_Devices
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      SoC_Buffer : Unbounded_String;
      SoC_First  : Unsigned_64 := Unsigned_64'Last;
      SoC_Last   : Unsigned_64 := 16#0#;
   begin
      for I in SoC_Device_Type'Range loop
         declare
            --  (1) extract all physical devices with the currently       --
            --      investigated SoC capability                           --
            Physical_SoC_Dev : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/devices/device" &
                   "[capabilities/capability/@name='" & Ada.Characters.
                   Handling.To_Lower (SoC_Device_Type'Image (I)) & "']");
         begin
            for K in 0 .. DOM.Core.Nodes.Length (Physical_SoC_Dev) - 1 loop
               declare
                  --  (2) check if SoC device is used by subject and      --
                  --      extract the virtual device node                 --
                  Virtual_SoC_Dev : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Subject,
                       XPath => "devices/device[@physical='" & DOM.Core.
                         Elements.Get_Attribute
                           (Elem => DOM.Core.Nodes.Item
                              (List  => Physical_SoC_Dev,
                               Index => K),
                            Name => "name") & "']");

                  Virtual_Dev_Entry : Unbounded_String;
                  Virtual_Dev_Range : DTS_Range_Type;
               begin
                  if DOM.Core.Nodes.Length (Virtual_SoC_Dev) = 1 then
                     case I is
                        when NIC  =>
                           Generate_NIC_Node (Policy    => Policy,
                                              Device    => DOM.Core.Nodes.Item
                                                (List  => Virtual_SoC_Dev,
                                                 Index => 0),
                                              DTS_Entry => Virtual_Dev_Entry,
                                              DTS_Range => Virtual_Dev_Range);
                        when UART =>
                           Generate_UART_Node (Policy    => Policy,
                                               Device    => DOM.Core.Nodes.Item
                                                 (List  => Virtual_SoC_Dev,
                                                  Index => 0),
                                               DTS_Entry => Virtual_Dev_Entry,
                                               DTS_Range => Virtual_Dev_Range);
                        when USB  =>
                           Generate_USB_Node (Policy    => Policy,
                                              Device    => DOM.Core.Nodes.Item
                                                (List  => Virtual_SoC_Dev,
                                                 Index => 0),
                                              DTS_Entry => Virtual_Dev_Entry,
                                              DTS_Range => Virtual_Dev_Range);
                     end case;

                     if Virtual_Dev_Range.Base < SoC_First then
                        SoC_First := Virtual_Dev_Range.Base;
                     end if;

                     if SoC_Last < Virtual_Dev_Range.Base then
                        SoC_Last := Virtual_Dev_Range.Base +
                          Virtual_Dev_Range.Size;
                     end if;

                     Block_Indent (Block     => Virtual_Dev_Entry,
                                   N         => 2,
                                   Unit_Size => 4);
                     Append (Source   => SoC_Buffer,
                             New_Item => ASCII.LF & Virtual_Dev_Entry);
                  end if;
               end;
            end loop;
         end;
      end loop;
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_soc_base__",
         Content  => Mutools.Utils.To_Hex (Number     => SoC_First,
                                           Normalize  => False,
                                           Byte_Short => False));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_soc_ranges__",
         Content  => "ranges = <0x0 0x0 0x0 0x0 0x" & Mutools.Utils.
           To_Hex (Number     => SoC_Last,
                   Normalize  => False,
                   Byte_Short => False) & ">;");
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_soc_devices__",
         Content  => To_String (Source => SoC_Buffer));
   end Add_SoC_Devices;

   -------------------------
   --  Generate_NIC_Node  --
   -------------------------
   procedure Generate_NIC_Node
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
   begin
      null;
   end Generate_NIC_Node;

   --------------------------
   --  Generate_UART_Node  --
   --------------------------
   procedure Generate_UART_Node
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
   begin
      null;
   end Generate_UART_Node;

   -------------------------
   --  Generate_USB_Node  --
   -------------------------
   procedure Generate_USB_Node
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
   begin
      null;
   end Generate_USB_Node;

end DTS.SoC_Devices;
