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

package body DTS.SoC_Devices
is

   UART_Device_Counter : Natural := 0;

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
                           UART_Device_Counter := UART_Device_Counter + 1;
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

                     if Length (Virtual_Dev_Entry) /= 0 then
                        Block_Indent (Block     => Virtual_Dev_Entry,
                                      N         => 2,
                                      Unit_Size => 4);
                        Append (Source   => SoC_Buffer,
                                New_Item => ASCII.LF & Virtual_Dev_Entry);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;

      UART_Device_Counter := 0;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_soc_base__",
         Content  => Mutools.Utils.To_Hex
           (Number     => (if Length (SoC_Buffer) /= 0
                           then SoC_First else 0),
            Normalize  => False,
            Byte_Short => False));
      --  NOTE - the child bus address translation ranges are specified   --
      --  as (child-bus-address, parent-bus-address, length), i.c. no     --
      --  address translation is used for the AMBA APU child bus (c.f.    --
      --  official Xilinx ZCU104 device tree)                             --
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__amba_soc_ranges__",
         Content  => "ranges = <" &
           To_DTS_Cell (Value => 16#0000_0000#) & " " &
           To_DTS_Cell (Value => 16#0000_0000#) & " " &
           To_DTS_Cell (Value => (if Length (SoC_Buffer) /= 0
                                  then SoC_Last else 0)) & ">;");
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
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.xilinx_xuartps_dsl);

      Virtual_IRQs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Device,
           XPath => "irq");

      Physical_Name : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Device,
                                            Name => "physical");

      Register_Entry : Unbounded_String;
   begin
      DTS_Register_Entry (Policy    => Policy,
                          Device    => Device,
                          DTS_Entry => Register_Entry,
                          DTS_Range => DTS_Range);

      if UART_Device_Counter = 0 then
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => "__uart_bus_alias__",
            Content  => "serial_0: ");
      else
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => "__uart_bus_alias__",
            Content  => "");
      end if;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__uart_bus_name__",
         Content  => Ada.Characters.Handling.To_Lower (Physical_Name));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__uart_bus_base__",
         Content  => Mutools.Utils.To_Hex (Number     => DTS_Range.Base,
                                           Normalize  => False,
                                           Byte_Short => False));
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__uart_registers__",
         Content  => To_String (Register_Entry));

      if DOM.Core.Nodes.Length (Virtual_IRQs) = 1 then
         declare
            Virtual_IRQ : constant Unsigned_64
              := Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Item
                      (List  => Virtual_IRQs,
                       Index => 0),
                    Name => "vector"));
            SPI_Offset  : constant Unsigned_64
              := 32;
         begin
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__uart_irq_irq__",
               Content  => Mutools.Utils.To_Hex
                 (Number     => Virtual_IRQ - SPI_Offset,
                  Normalize  => False,
                  Byte_Short => False));
         end;
      end if;

      Append (Source   => DTS_Entry,
              New_Item => Mutools.Templates.To_String (Template => Template));
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
