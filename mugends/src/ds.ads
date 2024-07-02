--
--  Copyright (C) 2023-2024  Tobias Brunner <tobias@codelabs.ch>
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

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with Interfaces;

package DS
is

   --  Backends can use either the already padded file in Filename_Padded or
   --  the padding information to implement their own padding.
   --  For fill patterns, Filename is set to Null_Unbounded_String.
   type Loadable_File is record
      Physical        : Ada.Strings.Unbounded.Unbounded_String;
      Filename        : Ada.Strings.Unbounded.Unbounded_String;
      Address         : Interfaces.Unsigned_64;
      Kernel          : Boolean;
      Filename_Padded : Ada.Strings.Unbounded.Unbounded_String;
      Padding_Address : Interfaces.Unsigned_64;
      Padding_Length  : Interfaces.Unsigned_64;
      Padding_Pattern : Interfaces.Unsigned_8;
   end record;

   function "<"
     (Left, Right : Loadable_File)
      return Boolean;

   -------------------------------------------------------------------------

   package File_Vector_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Loadable_File);

   package File_Vector_Sorting_Package is new File_Vector_Package.Generic_Sorting;

   package CPU_Kernel_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type        => Natural,
      Element_Type    => Loadable_File);

end DS;
