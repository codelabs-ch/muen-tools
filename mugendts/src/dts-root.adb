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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Utils;
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
      pragma Unreferenced (Policy);
   begin
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__chosen_bootparams__",
         Content  => Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "bootparams"));
   end Add_Chosen_Node;

   -------------------------------------------------------------------------

   procedure Add_Memory_Node
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

      Base_Address    : Unsigned_64 := Unsigned_64'Last;
      Register_Ranges : Unbounded_String;
   begin
      Append (Source   => Register_Ranges,
              New_Item => "reg = <");

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
            Physical_Memory_Size : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Physical_Memory_Node,
                                                  Name => "size");
            Physical_Memory_Type : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Physical_Memory_Node,
                                                  Name => "type");
         begin
            if Unsigned_64'Value (Virtual_Memory_Base) < Base_Address then
               Base_Address := Unsigned_64'Value (Virtual_Memory_Base);
            end if;

            if Physical_Memory_Type /= "subject_channel" then
               if I /= 0 then
                  Append (Source   => Register_Ranges,
                          New_Item => ASCII.LF & Register_Offset);
               end if;

               Append (Source   => Register_Ranges,
                       New_Item => To_DTS_Cell (Unsigned_64'Value
                         (Virtual_Memory_Base)) & " " & To_DTS_Cell
                       (Unsigned_64'Value (Physical_Memory_Size)));
            end if;
         end;
      end loop;

      Append (Source   => Register_Ranges,
              New_Item => ">;");

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__memory_base__",
         Content  => Mutools.Utils.To_Hex (Number     => Base_Address,
                                           Normalize  => False,
                                           Byte_Short => False));

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__memory_registers__",
         Content  => To_String (Source => Register_Ranges));
   end Add_Memory_Node;

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

      Add_Memory_Node (Template => Template,
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
