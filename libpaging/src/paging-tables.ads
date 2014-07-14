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

with Interfaces;

private with Ada.Containers.Bounded_Ordered_Maps;

with Paging.Entries;

package Paging.Tables
is

   type Page_Table_Type is private;

   Null_Table : constant Page_Table_Type;

   --  Add given entry to pagetable.
   procedure Add_Entry
     (Table : in out Page_Table_Type;
      Index :        Table_Range;
      E     :        Entries.Table_Entry_Type);

   --  Returns the number of entries present in the table.
   function Count (Table : Page_Table_Type) return Table_Range;

   --  Returns true if an entry with given index exists.
   function Contains
     (Table : Page_Table_Type;
      Index : Table_Range)
      return Boolean;

   --  Returns the physical memory address of the pagetable.
   function Get_Physical_Address
     (Table : Page_Table_Type)
      return Interfaces.Unsigned_64;

   --  Sets the physical memory address of the pagetable.
   procedure Set_Physical_Address
     (Table   : in out Page_Table_Type;
      Address :        Interfaces.Unsigned_64);

   --  Iterate over given page table and call given process procedure for each
   --  entry.
   procedure Iterate
     (Table   : Page_Table_Type;
      Process : not null access procedure
        (Index  : Table_Range;
         TEntry : Entries.Table_Entry_Type));

   --  Iterate over specified page table and call given process procedure for
   --  each entry. The table entry is modifiable.
   procedure Update
     (Table   : in out Page_Table_Type;
      Process : not null access procedure
        (Index  :        Table_Range;
         TEntry : in out Entries.Table_Entry_Type));

   --  Clear page table entries.
   procedure Clear (Table : in out Page_Table_Type);

   Duplicate_Entry : exception;

private

   use type Entries.Table_Entry_Type;
   use Ada.Containers;

   package Entries_Map_Package is new Ada.Containers.Bounded_Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Entries.Table_Entry_Type);

   type Page_Table_Type is record
      Address : Interfaces.Unsigned_64;
      Data    : Entries_Map_Package.Map (Count_Type (Table_Range'Last) + 1);
   end record;

   Null_Table : constant Page_Table_Type
     := (Address => 0,
         Data    => <>);

end Paging.Tables;
