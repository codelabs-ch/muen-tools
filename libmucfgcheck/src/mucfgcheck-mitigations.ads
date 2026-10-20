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

with Muxml;

package Mucfgcheck.Mitigations
is
   --D @Section Id => validation-mitigations, Label => Mitigations, Parent => validation
   --D @Text Section => validation-mitigations
   --D The following check is performed to verify the transient-execution
   --D mitigation policy of the system.
   --D @UL Id => validators_mitigations, Section => validation-mitigations

   --D @Item List => validators_mitigations
   --D Validate that every enabled mitigation sequence is implemented by the
   --D target kernel. The MCU revision floor and the ARCH_CAPABILITIES boot
   --D check are permanent kernel infrastructure and are exempt from this
   --D check.
   procedure Check_Implementation (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Mitigations;
