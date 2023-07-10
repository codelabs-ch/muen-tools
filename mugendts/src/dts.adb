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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Utils;

package body DTS
is

   --------------------
   --  Block_Indent  --
   --------------------
   procedure Block_Indent
     (Block     : in out Unbounded_String;
      N         :        Positive := 1;
      Unit_Size :        Positive := 4)
   is
      New_Line : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set (ASCII.LF);
      Indent   : constant String
        := Mutools.Utils.Indent (N, Unit_Size);

      Iterator : Natural := 0;
   begin
      loop
         while
           Iterator < Length (Source => Block) and then
           Element (Source => Block, Index => Iterator + 1) = ASCII.LF
         loop
            Iterator := Iterator + 1;
         end loop;

         exit when Iterator = Length (Source => Block);

         Insert (Source   => Block,
                 Before   => Iterator + 1,
                 New_Item => Indent);

         Iterator := Index (Source => Block,
                            Set    => New_Line,
                            From   => Iterator + 1);

         exit when Iterator = 0;
      end loop;
   end Block_Indent;

   --------------------------
   --  DTS_Register_Entry  --
   --------------------------
   procedure DTS_Register_Entry
     (Policy    :     Muxml.XML_Data_Type;
      Device    :     DOM.Core.Node;
      DTS_Entry : out Unbounded_String;
      DTS_Range : out DTS_Range_Type)
   is
      --  (1) extract all virtual device memory regions and its name      --
      Virtual_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Device,
           XPath => "memory");

      Physical_Name : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Device,
                                            Name => "physical");

      Base_Address : Unsigned_64 := Unsigned_64'Last;
      Range_Size   : Unsigned_64 := 16#0#;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (Virtual_Memory) - 1 loop
         declare
            -- (2) extract corresponding physical device memory node      --
            Physical_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/devices/device[@name='" &
                   Physical_Name & "']/memory[@name='" & DOM.Core.
                   Elements.Get_Attribute
                     (Elem => DOM.Core.Nodes.Item
                        (List  => Virtual_Memory,
                         Index => I),
                      Name => "physical") & "']");
         begin
            if DOM.Core.Nodes.Length (Physical_Memory) = 1 then
               declare
                  Memory_Address : constant Unsigned_64
                    := Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => DOM.Core.Nodes.Item
                            (List  => Virtual_Memory,
                             Index => I),
                          Name => "virtualAddress"));
                  Memory_Size : constant Unsigned_64
                    := Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => DOM.Core.Nodes.Item
                            (List  => Physical_Memory,
                             Index => 0),
                          Name => "size"));
               begin
                  if I = 0 then
                     Append (Source   => DTS_Entry,
                             New_Item => "reg = <" &
                               To_DTS_Cell (Value => Memory_Address) & " " &
                               To_DTS_Cell (Value => Memory_Size) & ">");
                  else
                     Append (Source   => DTS_Entry,
                             New_Item => "," & ASCII.LF & "        <" &
                               To_DTS_Cell (Value => Memory_Address) & " " &
                               To_DTS_Cell (Value => Memory_Size) & ">");
                  end if;

                  if Memory_Address < Base_Address then
                     Base_Address := Memory_Address;
                  end if;
                  Range_Size := Range_Size + Memory_Size;
               end;
            end if;
         end;
      end loop;
      Append (Source   => DTS_Entry,
              New_Item => ";");
      DTS_Range := (Base => Base_Address,
                    Size => Range_Size);
   end DTS_Register_Entry;

   ----------------------------------------------------------------
   --  To_DTS_Cell (for #address-cells and #size-cells = <0x2>)  --
   ----------------------------------------------------------------
   function To_DTS_Cell
     (Value : Unsigned_64)
      return String
   is
      use Ada.Strings.Fixed;

      Image  : constant String
        := Mutools.Utils.To_Hex (Number     => Value,
                                 Normalize  => False,
                                 Byte_Short => False);
      Result : String := "0x00000000 0x00000000";
   begin
      if Image'Length <= 8 then
         Replace_Slice (Source => Result,
                        Low    => Result'Last - Image'Length + 1,
                        High   => Result'Last,
                        By     => Image);
      else
         Replace_Slice (Source => Result,
                        Low    => Result'Last - 8 + 1,
                        High   => Result'Last,
                        By     => Image (Image'Last - 8 + 1 .. Image'Last));
         Replace_Slice (Source => Result,
                        Low    => Result'Last - (Image'Length + 3) + 1,
                        High   => Result'Last - (8 + 3),
                        By     => Image (Image'First .. Image'Last - 8));
      end if;

      return Result;
   end To_DTS_Cell;

end DTS;
