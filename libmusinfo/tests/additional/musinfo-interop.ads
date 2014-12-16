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

--  Template package for additional C interop tests.
package Musinfo.Interop
is

   --  Verify name type interoperability.
   procedure Name_To_C;

   --  Verify channel type interoperability.
   procedure Channel_To_C;

   --  Verify resource type interoperability.
   procedure Resource_To_C;

   --  Verify subject info type interoperability.
   procedure Subject_Info_To_C;

   --  Verify name type equivalence.
   procedure Check_Name_Type;

   --  Verify channel type equivalence.
   procedure Check_Channel_Type;

   --  Verify resource type equivalence.
   procedure Check_Resource_Type;

   --  Verify subject info type equivalence.
   procedure Check_Subject_Info_Type;

end Musinfo.Interop;
