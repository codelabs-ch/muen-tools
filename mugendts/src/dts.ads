--
--  Copyright (C) 2023, 2023  David Loosli <david@codelabs.ch>
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

with Interfaces; use Interfaces;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DTS
is

   type DTS_Range_Type
   is record
      Base : Unsigned_64;
      Size : Unsigned_64;
   end record;

   function To_DTS_Cell
     (Value : Unsigned_64)
      return String;

   procedure Block_Indent
     (Block     : in out Unbounded_String;
      N         :        Positive := 1;
      Unit_Size :        Positive := 4);

end DTS;
