--
--  Copyright (C) 2014-2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mulog;
with Mutools.Templates;
with Mutools.Types;
with Mutools.XML_Utils;

with Spec.Utils;
with String_Templates;

package body Spec.Policy_Gpr
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Filename : constant String := Output_Dir & "/" & "policy.gpr";
      Tmpl     : Mutools.Templates.Template_Type;

      --  Returns the configured debug device type as string.
      function Get_Debug_Device_Type return String;

      --  Returns all valid diagnostics device kinds as string.
      function Get_Diagnostics_Kind return String;

      --  Returns the string "True" if the enabled mitigation policy requires
      --  IA32_ARCH_CAPABILITIES bits (non-empty <archCapabilities/>) and
      --  "False" otherwise (the ARCH_CAP kernel check is then compiled out).
      function Has_Arch_Cap_Check return String;

      --  Returns the string "True" if the microcode update facility should be
      --  active and "False" otherwise.
      function Has_MCU return String;

      --  Returns the string "True" if the transient-execution mitigation
      --  policy is enabled and "False" otherwise.
      function Has_Mitigations return String;

      --  Returns the string "True" if the scheduling plan has multiple major
      --  frames and "False" otherwise.
      function Has_Multiple_Major_Frames return String;

      --  Returns the string "True" if the given vmEntry mitigation leaf is
      --  enabled and "False" otherwise.
      function Has_VM_Entry_Leaf (Name : String) return String;

      --  Returns the string "True" if any vmEntry mitigation leaf is
      --  enabled (the VM-entry mitigation primitives are then compiled in)
      --  and "False" otherwise.
      function Has_VM_Entry_Mitigations return String;

      --  Returns the string "True" if the given vmExit mitigation leaf is
      --  enabled and "False" otherwise.
      function Has_VM_Exit_Leaf (Name : String) return String;

      ----------------------------------------------------------------------

      function Get_Debug_Device_Type return String
      is
         Prefix : constant String := "Debug_Type : Diagnostics_Kind := """;
         Diagnostics_Type : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/platform/kernelDiagnostics",
              Name  => "type");
      begin
         return Indent & Prefix & Ada.Characters.Handling.To_Lower
           (Item => Diagnostics_Type) & """;" & ASCII.LF;
      end Get_Debug_Device_Type;

      ----------------------------------------------------------------------

      function Get_Diagnostics_Kind return String
      is
         use Ada.Strings.Unbounded;
         use type Mutools.Types.Kernel_Diagnostics_Kind;

         Buf : Unbounded_String;
      begin
         for K in Mutools.Types.Kernel_Diagnostics_Kind loop
            if K /= Mutools.Types.Kernel_Diagnostics_Kind'First then
               Buf := Buf & ",";
            end if;
            Buf := Buf & """" & Ada.Characters.Handling.To_Lower (K'Img)
              & """";
         end loop;

         return To_String (Source => Buf);
      end Get_Diagnostics_Kind;

      ----------------------------------------------------------------------

      function Has_Arch_Cap_Check return String
      is
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
         Requires : constant DOM.Core.Node_List := XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations/"
              & "archCapabilities/require");
      begin
         --  Disabled policy or no require bits: no ARCH_CAP check.
         if DOM.Core.Elements.Get_Attribute
             (Elem => Mitigations,
              Name => "enable") = "false"
           or else DOM.Core.Nodes.Length (List => Requires) = 0
         then
            return "False";
         end if;
         return "True";
      end Has_Arch_Cap_Check;

      ----------------------------------------------------------------------

      function Has_MCU return String
      is
      begin
         return
           Mutools.Utils.To_Ada_Identifier
             (Str => Utils.Requires_MCU (Policy => Policy)'Img);
      end Has_MCU;

      ----------------------------------------------------------------------

      function Has_Mitigations return String
      is
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
      begin
         if DOM.Core.Elements.Get_Attribute
             (Elem => Mitigations,
              Name => "enable") = "false"
         then
            return "False";
         end if;
         return "True";
      end Has_Mitigations;

      ----------------------------------------------------------------------

      function Has_Multiple_Major_Frames return String
      is
      begin
         return Mutools.Utils.To_Ada_Identifier
           (Str => Mutools.XML_Utils.Has_Multiple_Major_Frames
              (Data => Policy)'Img);
      end Has_Multiple_Major_Frames;

      ----------------------------------------------------------------------

      function Has_VM_Entry_Leaf (Name : String) return String
      is
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
         Leaf : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations/"
              & "vmEntry/" & Name);
         Enabled : constant Boolean :=
           DOM.Core.Elements.Get_Attribute
             (Elem => Mitigations,
              Name => "enable") /= "false"
           and then DOM.Core.Elements.Get_Attribute
             (Elem => Leaf,
              Name => "enable") = "true";
      begin
         return Mutools.Utils.To_Ada_Identifier
           (Str => Enabled'Img);
      end Has_VM_Entry_Leaf;

      ----------------------------------------------------------------------

      function Has_VM_Entry_Mitigations return String
      is
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
         Leaves : constant DOM.Core.Node_List := XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations/"
              & "vmEntry/*");
      begin
         if DOM.Core.Elements.Get_Attribute
             (Elem => Mitigations,
              Name => "enable") = "false"
         then
            return "False";
         end if;
         for I in 0 .. DOM.Core.Nodes.Length (List => Leaves) - 1 loop
            if DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item (List  => Leaves,
                                            Index => I),
               Name => "enable") = "true"
            then
               return "True";
            end if;
         end loop;
         return "False";
      end Has_VM_Entry_Mitigations;

      ----------------------------------------------------------------------

      function Has_VM_Exit_Leaf (Name : String) return String
      is
         Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations");
         Leaf : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/processor/x86_64/mitigations/"
              & "vmExit/" & Name);
         Enabled : constant Boolean :=
           DOM.Core.Elements.Get_Attribute
             (Elem => Mitigations,
              Name => "enable") /= "false"
           and then DOM.Core.Elements.Get_Attribute
             (Elem => Leaf,
              Name => "enable") = "true";
      begin
         return Mutools.Utils.To_Ada_Identifier
           (Str => Enabled'Img);
      end Has_VM_Exit_Leaf;
   begin
      Mulog.Log (Msg => "Writing policy project file to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.policy_gpr);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__diagnostics_kind__",
         Content  => Get_Diagnostics_Kind);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__debug_device_type__",
         Content  => Get_Debug_Device_Type);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__mcu_enabled__",
         Content  => Has_MCU);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__mitigations_enabled__",
         Content  => Has_Mitigations);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__archcap_check_enabled__",
         Content  => Has_Arch_Cap_Check);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vm_entry_enabled__",
         Content  => Has_VM_Entry_Mitigations);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vm_entry_ibpb_enabled__",
         Content  => Has_VM_Entry_Leaf (Name => "ibpb"));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vm_entry_verw_enabled__",
         Content  => Has_VM_Entry_Leaf (Name => "verw"));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vm_entry_l1dflush_enabled__",
         Content  => Has_VM_Entry_Leaf (Name => "l1dFlush"));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vm_exit_ibrslegacy_enabled__",
         Content  => Has_VM_Exit_Leaf (Name => "ibrsLegacy"));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__multiple_major_frames__",
         Content  => Has_Multiple_Major_Frames);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write;

end Spec.Policy_Gpr;
