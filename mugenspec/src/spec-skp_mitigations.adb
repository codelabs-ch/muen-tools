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

with DOM.Core;
with DOM.Core.Elements;

with Muxml.Utils;
with Mutools.Templates;
with Mutools.XML_Utils;

with Mulog;

with String_Templates;

package body Spec.Skp_Mitigations
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Pkg : constant String := Output_Dir & "/skp-mitigations.ads";

      Tmpl        : Mutools.Templates.Template_Type;
      Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/processor/x86_64/mitigations");
   begin
      --  Not needed on ARMv8-A or when the mitigation policy is disabled.
      if Mutools.XML_Utils.Is_Arm64 (Policy => Policy)
        or else DOM.Core.Elements.Get_Attribute
          (Elem => Mitigations,
           Name => "enable") = "false"
      then
         return;
      end if;

      Mulog.Log (Msg => "Writing mitigation spec to '" & Pkg & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_mitigations_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__min_mcu_revision__",
         Content  => Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations/mcu",
            Name  => "minRevision"));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Pkg);
   end Write;

end Spec.Skp_Mitigations;
