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

with Ada.Strings.Fixed;

with Mutools.Utils;

package body DTS
is

   -------------------
   --  To_DTS_Cell  --
   -------------------
   function To_DTS_Cell
     (Value : Unsigned_64)
      return String
   is
      use Ada.Strings.Fixed;

      Image  : constant String
        := Mutools.Utils.To_Hex (Number     => Value,
                                 Normalize  => False,
                                 Byte_Short => False);
      Result : String := "0x00000000 0x00000000";
   begin
      if Image'Length <= 8 then
         Replace_Slice (Source => Result,
                        Low    => Result'Last - Image'Length + 1,
                        High   => Result'Last,
                        By     => Image);
      else
         Replace_Slice (Source => Result,
                        Low    => Result'Last - 8 + 1,
                        High   => Result'Last,
                        By     => Image (Image'Last - 8 + 1 .. Image'Last));
         Replace_Slice (Source => Result,
                        Low    => Result'Last - (Image'Length + 3) + 1,
                        High   => Result'Last - (8 + 3),
                        By     => Image (Image'First .. Image'Last - 8));
      end if;

      return Result;
   end To_DTS_Cell;

end DTS;
