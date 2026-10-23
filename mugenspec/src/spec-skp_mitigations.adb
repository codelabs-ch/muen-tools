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
with DOM.Core.Nodes;

with Interfaces;

with McKae.XML.XPath.XIA;

with Muxml.Utils;
with Mutools.Templates;
with Mutools.Utils;
with Mutools.XML_Utils;

with Mulog;

with String_Templates;

package body Spec.Skp_Mitigations
is

   use Interfaces;
   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   --  Return the bit number of the given IA32_ARCH_CAPABILITIES bit name,
   --  e.g. "RDCL_NO" to 0, see Intel SDM Vol. 4, Table 2-2.
   function Arch_Cap_Bit (Name : String) return Natural;

   --  Compute the IA32_ARCH_CAPABILITIES check mask and expected value from
   --  the <archCapabilities><require> bits. Every require bit is required-1
   --  (mask and expected); there is no required-0 / prerequisite contribution
   --  - the require set is measured at collection time and the mask is empty
   --  (check disabled) if there are no require bits.
   procedure Compute_Arch_Cap_Check
     (Mitigations : DOM.Core.Node;
      Mask        : out Interfaces.Unsigned_64;
      Expected    : out Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   function Arch_Cap_Bit (Name : String) return Natural
   is
   begin
      if Name = "RDCL_NO" then
         return 0;
      elsif Name = "IBRS_ALL" then
         return 1;
      elsif Name = "SKIP_L1DFL_VMENTRY" then
         return 3;
      elsif Name = "SSB_NO" then
         return 4;
      elsif Name = "MDS_NO" then
         return 5;
      elsif Name = "IF_PSCHANGE_MC_NO" then
         return 6;
      elsif Name = "TAA_NO" then
         return 8;
      elsif Name = "SBDR_SSDP_NO" then
         return 13;
      elsif Name = "FBSDP_NO" then
         return 14;
      elsif Name = "PSDP_NO" then
         return 15;
      elsif Name = "FB_CLEAR" then
         return 17;
      elsif Name = "BHI_NO" then
         return 20;
      elsif Name = "PBRSB_NO" then
         return 24;
      elsif Name = "GDS_CTRL" then
         return 25;
      elsif Name = "GDS_NO" then
         return 26;
      elsif Name = "RFDS_NO" then
         return 27;
      elsif Name = "RFDS_CLEAR" then
         return 28;
      end if;
      raise Constraint_Error with "Unknown archCapabilities bit: " & Name;
   end Arch_Cap_Bit;

   -------------------------------------------------------------------------

   procedure Compute_Arch_Cap_Check
     (Mitigations : DOM.Core.Node;
      Mask        : out Interfaces.Unsigned_64;
      Expected    : out Interfaces.Unsigned_64)
   is
   begin
      Mask     := 0;
      Expected := 0;

      declare
         Requires : constant DOM.Core.Node_List := XPath_Query
           (N     => Mitigations,
            XPath => "archCapabilities/require");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Requires) - 1 loop
            declare
               Node   : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Requires,
                                         Index => I);
               Name   : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "bit");
               Bit    : constant Interfaces.Unsigned_64
                 := Shift_Left (Value  => 1,
                                Amount => Arch_Cap_Bit (Name));
            begin
               Mask     := Mask or Bit;
               Expected := Expected or Bit;
            end;
         end loop;
      end;
   end Compute_Arch_Cap_Check;

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

      declare
         Mask     : Interfaces.Unsigned_64;
         Expected : Interfaces.Unsigned_64;
      begin
         Compute_Arch_Cap_Check
           (Mitigations => Mitigations,
            Mask        => Mask,
            Expected    => Expected);

         Tmpl := Mutools.Templates.Create
           (Content => String_Templates.skp_mitigations_ads);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__min_mcu_revision__",
            Content  => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/hardware/processor/x86_64/mitigations/mcu",
               Name  => "minRevision"));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__arch_cap_mask__",
            Content  => Mutools.Utils.To_Hex (Number => Mask));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__arch_cap_expected__",
            Content  => Mutools.Utils.To_Hex (Number => Expected));

         Mutools.Templates.Write
           (Template => Tmpl,
            Filename => Pkg);
      end;
   end Write;

end Spec.Skp_Mitigations;
