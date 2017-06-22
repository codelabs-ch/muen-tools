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

package Mucfgcheck.Subject
is

   --  Validate subject name uniqueness
   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate subject CPU ID.
   procedure CPU_ID (XML_Data : Muxml.XML_Data_Type);

   --  Validate uniqueness of global subject IDs.
   procedure Global_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate uniqueness of local subject IDs.
   procedure Local_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate memory types of memory mappings.
   procedure Memory_Types (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no subject references an IOMMU device.
   procedure No_IOMMU_Device_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all subjects are runnable, i.e. directly referenced in
   --  the scheduling plan or target of a switch event of a subject that is
   --  itself scheduled.
   procedure Runnability (XML_Data : Muxml.XML_Data_Type);

   --  Validate that IRQ vector numbers of PCI device references with MSI
   --  enabled are consecutive.
   procedure Logical_IRQ_MSI_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no virtual memory regions of a subject overlap.
   procedure Virtual_Memory_Overlap (XML_Data : Muxml.XML_Data_Type);

   --  Validate that multiple initramfs regions are consecutive.
   procedure Initramfs_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no subject has write access to the crash audit region.
   procedure Crash_Audit_Write_Access (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Subject;
