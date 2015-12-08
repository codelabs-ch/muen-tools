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

with Muxml;

package Expanders.Platform
is

   --  Add PCI config space memory regions and mappings for PCI devices.
   procedure Add_PCI_Config_Space (Data : in out Muxml.XML_Data_Type);

   --  Add default IOMMU device capabilities if not specified explicitly.
   procedure Add_IOMMU_Default_Caps (Data : in out Muxml.XML_Data_Type);

   --  Remove reserved memory regions from policy.
   procedure Remove_Reserved_Mem_Regions (Data : in out Muxml.XML_Data_Type);

   --  Remove reserved memory region references from policy.
   procedure Remove_Reserved_Mem_References
     (Data : in out Muxml.XML_Data_Type);

end Expanders.Platform;
