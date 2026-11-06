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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

with Mucfgcheck.Validation_Errors;

package body Mucfgcheck.Mitigations
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Check_Implementation (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Mitigations : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => XML_Data.Doc,
         XPath => "/system/hardware/processor/x86_64/mitigations");

      --  Report an error if the given mitigation leaf is enabled but not
      --  implemented by the kernel.
      procedure Check_Leaf (Leaf : DOM.Core.Node);

      --  Check all mitigation leaves of the given section.
      procedure Check_Leaves (Section : String);

      --  Return whether the given mitigation sequence is implemented.
      function Is_Implemented
        (Mitigation : String;
         Mode       : String := "")
         return Boolean;

      ---------------------------------------------------------------------

      procedure Check_Leaf (Leaf : DOM.Core.Node)
      is
         Element     : constant String
           := DOM.Core.Nodes.Node_Name (N => Leaf);
         Mode        : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Leaf,
                                               Name => "mode");
         Enabled_Str : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Leaf,
                                               Name => "enable");
      begin
         if Enabled_Str = "true" then
            Mulog.Log (Msg => "Checking mitigation '" & Element & "'");
            if not Is_Implemented (Mitigation => Element,
                                   Mode       => Mode)
            then
               Validation_Errors.Insert
                 (Msg => "Mitigation '" & Element & "'"
                  & (if Mode'Length > 0
                     then " (mode '" & Mode & "')"
                     else "")
                  & " is enabled but not implemented for the x86_64 "
                  & "target kernel");
            end if;
         end if;
      end Check_Leaf;

      ---------------------------------------------------------------------

      procedure Check_Leaves (Section : String)
      is
         Leaves : constant DOM.Core.Node_List := XPath_Query
           (N     => Mitigations,
            XPath => Section & "/*");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Leaves) - 1 loop
            Check_Leaf (Leaf => DOM.Core.Nodes.Item (List  => Leaves,
                                                     Index => I));
         end loop;
      end Check_Leaves;

      ---------------------------------------------------------------------

      function Is_Implemented
        (Mitigation : String;
         Mode       : String := "")
         return Boolean
      is
      begin
         --  Implemented sequences, extended in lockstep with the kernel
         --  primitives (kernel/src/asm/mitigations.S, and the Ada VM-entry
         --  primitives of SK.Mitigations.VM_Entry).
         return (Mitigation = "rsbStuffing" and then Mode = "fullFill32")
           or else Mitigation = "bhbLoop"
           or else Mitigation = "ibpb"
           or else Mitigation = "verw"
           or else Mitigation = "l1dFlush";
      end Is_Implemented;
   begin
      if DOM.Core.Elements.Get_Attribute
        (Elem => Mitigations,
         Name => "enable") = "false"
      then
         --  The mitigation policy is disabled; nothing is checked or
         --  enforced.
         return;
      end if;

      Mulog.Log (Msg => "Checking mitigation implementations");
      Check_Leaves (Section => "init");
      Check_Leaves (Section => "vmEntry");
      Check_Leaves (Section => "vmExit");
   end Check_Implementation;

end Mucfgcheck.Mitigations;
