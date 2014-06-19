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

with Ada.Streams;

with Paging.Tables;
with Paging.Pagetables;

package Paging.EPT
is

   --  Implementation of EPT paging structures, as specified by Intel SDM
   --  Vol. 3C, section 28.3.

   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PML4   : Tables.PML4.Page_Table_Type);

   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PDPT   : Tables.PDPT.Page_Table_Type);

   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PD     : Tables.PD.Page_Table_Type);

   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PT     : Tables.PT.Page_Table_Type);

   procedure Serialize_PML4
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Pagetables.Page_Table_Type);

end Paging.EPT;
