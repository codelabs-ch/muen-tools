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

package body Paging
is

   use type Interfaces.Unsigned_64;

   --  PML4 index is bits 39 .. 47 of the linear address.
   PML4_Index_Mask : constant Interfaces.Unsigned_64 := 16#0000ff8000000000#;

   --  PDPT index is bits 30 .. 38 of the linear address.
   PDPT_Index_Mask : constant Interfaces.Unsigned_64 := 16#0000007fc0000000#;

   --  PD index is bits 21 .. 29 of the linear address.
   PD_Index_Mask : constant Interfaces.Unsigned_64 := 16#000000003fe00000#;

   --  PT index is bits 12 .. 20 of the linear address.
   PT_Index_Mask : constant Interfaces.Unsigned_64 := 16#00000000001ff000#;

   type Level_Info_Type is record
      Mask : Interfaces.Unsigned_64;
      Size : Interfaces.Unsigned_64;
   end record;

   Level_Map : constant array (Paging_Level) of Level_Info_Type
     := (1 => (Mask => PML4_Index_Mask,
               Size => 2 ** 39),
         2 => (Mask => PDPT_Index_Mask,
               Size => PDPT_Page_Size),
         3 => (Mask => PD_Index_Mask,
               Size => PD_Page_Size),
         4 => (Mask => PT_Index_Mask,
               Size => Page_Size));

   -------------------------------------------------------------------------

   function Get_Index
     (Address : Interfaces.Unsigned_64;
      Level   : Paging_Level)
      return Entry_Range
   is
   begin
      return Entry_Range
        ((Address and Level_Map (Level).Mask) / Level_Map (Level).Size);
   end Get_Index;

   -------------------------------------------------------------------------

   procedure Get_Indexes
     (Address :     Interfaces.Unsigned_64;
      Indexes : out Table_Index_Array)
   is
      Cur_Lvl : Natural := 4;
   begin
      for Idx of reverse Indexes loop
         Idx := Get_Index (Address => Address,
                           Level   => Paging_Level (Cur_Lvl));
         Cur_Lvl := Cur_Lvl - 1;
      end loop;
   end Get_Indexes;

end Paging;
