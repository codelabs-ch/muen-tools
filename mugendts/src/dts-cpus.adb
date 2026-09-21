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

-- with Ada.Characters.Handling;

--with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Utils;

with String_Templates;

package body DTS.CPUs
is

   -------------------------------------------------------------------------

   procedure Add_CPU_Nodes
     (Template     : in out Mutools.Templates.Template_Type;
      Policy       :        Muxml.XML_Data_Type;
      Subject_Name : String)
   is
      Siblings : constant DOM.Core.Node_List
      := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject/sibling[@ref='" & Subject_Name & "']");

      CPUs_Buffer : Unbounded_String;
   begin
      for I in 1 .. DOM.Core.Nodes.Length (Siblings) loop
         Generate_CPU_Node (Index => Unsigned_64 (I),
                            Sibbling  => DOM.Core.Nodes.Item
                              (List  => Siblings,
                               Index => I),
                            Buffer => CPUs_Buffer);
      end loop;

      Block_Indent (Block     => CPUs_Buffer,
                    N         => 2,
                    Unit_Size => 4);

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__sibling_cpus__",
         Content  => To_String (CPUs_Buffer));

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__memreserve_cpu_spintable__",
         Content  => "/memreserve/ 0x10000 0x1000;"); -- TODO: plumb to policy
   end Add_CPU_Nodes;

   -------------------------------------------------------------------------

   procedure Generate_CPU_Node
     (Sibbling  :        DOM.Core.Node;
      Index     :        Unsigned_64;
      Buffer    : in out Unbounded_String)
   is
      pragma Unreferenced (Sibbling);
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.muen_cpu_dsl);
   begin
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__cpu_base__",
         Content  => Mutools.Utils.To_Hex
           (Number     => Index,
            Normalize  => False,
            Byte_Short => False));

      -- Note: cpu "reg" is matched with MPIDR Aff0-3 value MPIDR_HWID_BITMASK.
      -- See Documentation/devicetree/bindings/arm/cpus.yaml
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__cpu_registers__",
         Content  => "reg = <0x" &
                     Mutools.Utils.To_Hex (Number     => Index,
                                           Normalize  => False,
                                           Byte_Short => False)
                     & ">;");

      Append (Source => Buffer,
              New_Item => Mutools.Templates.To_String (Template));
   end Generate_CPU_Node;

end DTS.CPUs;
