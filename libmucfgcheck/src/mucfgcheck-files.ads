--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with Muxml;

package Mucfgcheck.Files
is

   --D @Section Id => validation-files, Label => Files, Parent => validation, Priority => 0
   --D @Text Section => validation-files, Priority => 0
   --D The following file-specific checks are performed.
   --D @UL Id => validators_files, Section => validation-files, Priority => 0

   --  Set input directory required for file checks.
   procedure Set_Input_Directory (Dir : String);

   --  Return current input directory.
   function Get_Input_Directory return String;

   --D @Item List => validators_files, Priority => 0
   --D Check existence of files referenced in XML policy.
   procedure Files_Exist (Data : Muxml.XML_Data_Type);

   --D @Item List => validators_files, Priority => 0
   --D Check if files fit into corresponding memory region.
   procedure Files_Size (Data : Muxml.XML_Data_Type);

private

   Input_Dir : Ada.Strings.Unbounded.Unbounded_String;

end Mucfgcheck.Files;
