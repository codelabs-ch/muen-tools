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

package Mucfgcheck.Device_Domains
is

   --  Validate that domain device references are unique.
   procedure Device_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that IOMMU device is present if domains are specified.
   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no virtual memory regions of a domain overlap.
   procedure Domain_Memory_Overlap (XML_Data : Muxml.XML_Data_Type);

   --  Validate that domain memory references are unique.
   procedure Memory_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that domain memory referenced by subjects is mapped at the same
   --  virtual address.
   procedure Memory_Mapping_Address_Equality (XML_Data : Muxml.XML_Data_Type);

   --  Validate that each PCI device referenced by a subject is assigned to a
   --  device domain.
   procedure PCI_Device_Domain_Assignment (XML_Data : Muxml.XML_Data_Type);

   --  Validate memory type of physical memory referenced by domains.
   procedure Domain_Memory_Type (XML_Data : Muxml.XML_Data_Type);

   --  Validate that each device referenced by a device domain is a PCI device.
   procedure PCI_Device_References (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Device_Domains;
