--
--  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;
with Mutools.XML_Utils;

with String_Templates;

package body Spec.Skp_Mitigations_H
is

   use Ada.Strings.Unbounded;
   use McKae.XML.XPath.XIA;

   --  Append the selection defines of the given mitigations section.
   procedure Append_Defines
     (Mitigations : DOM.Core.Node;
      Section     : String;
      Defines     : in out Unbounded_String);

   --  Return the selection define of the given mitigation leaf (element
   --  and, for rsbStuffing, mode), e.g. "SK_MIT_RSB_STUFFING_FULLFILL32".
   function Selection_Define
     (Element : String;
      Mode    : String)
      return String;

   --  Convert a camelCase leaf name to UPPER_SNAKE, e.g. "bhbLoop" to
   --  "BHB_LOOP".
   function To_Upper_Snake (Name : String) return String;

   -------------------------------------------------------------------------

   procedure Append_Defines
     (Mitigations : DOM.Core.Node;
      Section     : String;
      Defines     : in out Unbounded_String)
   is
      Leaves : constant DOM.Core.Node_List := XPath_Query
        (N     => Mitigations,
         XPath => Section & "/*");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Leaves) - 1 loop
         declare
            Leaf    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Leaves,
                                      Index => I);
            Element : constant String
              := DOM.Core.Nodes.Node_Name (N => Leaf);
            Mode    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Leaf,
                 Name => "mode");
            Enabled : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Leaf,
                 Name => "enable");
         begin
            if Enabled = "true" then
               Defines := Defines & "#define "
                 & Selection_Define (Element => Element,
                                     Mode    => Mode)
                 & " 1" & ASCII.LF;
            end if;
         end;
      end loop;
   end Append_Defines;

   -------------------------------------------------------------------------

   function Selection_Define
     (Element : String;
      Mode    : String)
      return String
   is
   begin
      if Element = "rsbStuffing" then
         return "SK_MIT_RSB_STUFFING_"
           & Ada.Characters.Handling.To_Upper (Item => Mode);
      end if;
      return "SK_MIT_" & To_Upper_Snake (Element);
   end Selection_Define;

   -------------------------------------------------------------------------

   function To_Upper_Snake (Name : String) return String
   is
      Result : Unbounded_String;
   begin
      for I in Name'Range loop
         if I > Name'First and then Name (I) in 'A' .. 'Z' then
            Result := Result & "_";
         end if;
         Result := Result & Ada.Characters.Handling.To_Upper (Item => Name (I));
      end loop;
      return To_String (Result);
   end To_Upper_Snake;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Pkg : constant String := Output_Dir & "/sk-mitigations.h";

      Tmpl    : Mutools.Templates.Template_Type;
      Defines : Unbounded_String;
   begin
      if Mutools.XML_Utils.Is_Arm64 (Policy => Policy) then
         return;
      end if;

      Mulog.Log (Msg => "Writing mitigation selection header to '"
                 & Pkg & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.sk_mitigations_h);

      declare
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
      begin
         if DOM.Core.Elements.Get_Attribute
           (Elem => Mitigations,
            Name => "enable") /= "false"
         then
            Append_Defines (Mitigations => Mitigations,
                            Section     => "init",
                            Defines     => Defines);
            Append_Defines (Mitigations => Mitigations,
                            Section     => "vmEntry",
                            Defines     => Defines);
            Append_Defines (Mitigations => Mitigations,
                            Section     => "vmExit",
                            Defines     => Defines);
         end if;
      end;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__sk_mit_defines__",
         Content  => To_String (Defines));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Pkg);
   end Write;

end Spec.Skp_Mitigations_H;
