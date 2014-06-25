--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Containers.Ordered_Sets;

with DOM.Core;

with Muxml;

package Mutools.XML_Utils
is

   --  Add physical memory region element with given parameters to policy.
   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String);

   --  Add file-backed physical memory region element with given parameters to
   --  policy.
   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String;
      File_Name   :        String;
      File_Offset :        String);

   --  Create memory node element with given parameters.
   function Create_Memory_Node
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String)
      return DOM.Core.Node;

   --  Returns True if the given VMX controls specify that the DEBUGCTL MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_DEBUGCTL (Controls : DOM.Core.Node) return Boolean;

   --  Returns True if the given VMX controls specify that the
   --  PERFGLOBALCTRL MSR is loaded automatically on VM-entries.
   function Has_Managed_PERFGLOBALCTRL
     (Controls : DOM.Core.Node)
      return Boolean;

   --  Returns True if the given VMX controls specify that the PAT MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_PAT (Controls : DOM.Core.Node) return Boolean;

   --  Returns True if the given VMX controls specify that the EFER MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_EFER (Controls : DOM.Core.Node) return Boolean;

   --  Returns the number of Model-Specific registers that must be managed by
   --  the MSR store mechanism given the list of MSR nodes and considering the
   --  specified control flags.
   function Calculate_MSR_Count
     (MSRs                   : DOM.Core.Node_List;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
      return Natural;

   type PCI_Bus_Range is range 0 .. 255;

   package PCI_Bus_Set is new Ada.Containers.Ordered_Sets
     (Element_Type => PCI_Bus_Range);

   --  Return set of occupied PCI bus numbers for given system policy.
   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set;

end Mutools.XML_Utils;
