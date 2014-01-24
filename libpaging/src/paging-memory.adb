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

with Paging.Entries;

package body Paging.Memory
is

   ----------------------------------------------------------------------

   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean)
   is
      use type Interfaces.Unsigned_64;
      use Paging.Tables;

      Is_PDPT_Page : constant Boolean := Size mod PDPT_Page_Size = 0;
      Is_PD_Page   : constant Boolean := Size mod PD_Page_Size   = 0;
      Virt_End     : constant Interfaces.Unsigned_64
        := Virtual_Address + Size - 1;

      PML4_Idx_Start, PML4_Idx_End : Table_Range;
      PDPT_Idx_Start, PDPT_Idx_End : Table_Range;
      PD_Idx_Start, PD_Idx_End     : Table_Range;
      PT_Idx_Start, PT_Idx_End     : Table_Range;

      --  Physical start address of PDPT paging structure(s).
      PDPT_Addr : Interfaces.Unsigned_64;
      --  Physical start address of PD paging structure(s).
      PD_Addr   : Interfaces.Unsigned_64;
      --  Physical start address of PT paging structure(s).
      PT_Addr   : Interfaces.Unsigned_64;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
   begin
      Get_Indexes (Address    => Virtual_Address,
                   PML4_Index => PML4_Idx_Start,
                   PDPT_Index => PDPT_Idx_Start,
                   PD_Index   => PD_Idx_Start,
                   PT_Index   => PT_Idx_Start);
      Get_Indexes (Address    => Virt_End,
                   PML4_Index => PML4_Idx_End,
                   PDPT_Index => PDPT_Idx_End,
                   PD_Index   => PD_Idx_End,
                   PT_Index   => PT_Idx_End);

      PDPT_Addr := PML4.Get_Physical_Address (Table => Mem_Layout.PML4) +
        (Interfaces.Unsigned_64 (PML4_Idx_End) + 1) * Page_Size;
      PD_Addr   := PDPT_Addr +
        (Interfaces.Unsigned_64 (PDPT_Idx_End) + 1) * Page_Size;
      PT_Addr   := PD_Addr +
        (Interfaces.Unsigned_64 (PD_Idx_End) + 1) * Page_Size;

      for PML4_Idx in Table_Range range PML4_Idx_Start .. PML4_Idx_End loop
         if not Tables.PML4.Contains (Table => Mem_Layout.PML4,
                                      Index => PML4_Idx)
         then
            Tables.PML4.Add_Entry
              (Table => Mem_Layout.PML4,
               Index => PML4_Idx,
               E     => Entries.Create
                 (Dst_Offset  => PML4_Idx,
                  Dst_Address => PDPT_Addr +
                    Interfaces.Unsigned_64 (PML4_Idx) * Page_Size,
                  Readable    => True,
                  Writable    => True,
                  Executable  => True,
                  Maps_Page   => False,
                  Global      => False,
                  Caching     => WC));
         end if;
      end loop;

      for PDPT_Idx in Table_Range range PDPT_Idx_Start .. PDPT_Idx_End loop
         if Is_PDPT_Page then
            PD_Addr := Physical_Addr + Interfaces.Unsigned_64
              (PDPT_Idx - PDPT_Idx_Start) * PDPT_Page_Size;
         else
            PD_Addr := PD_Addr + Interfaces.Unsigned_64
              (PDPT_Idx - PDPT_Idx_Start) * Page_Size;
         end if;

         if not PDPT.Contains
           (Map          => Mem_Layout.PDPTs,
            Table_Number => 0,
            Entry_Index  => PDPT_Idx)
         then
            PDPT.Add_Entry
              (Map          => Mem_Layout.PDPTs,
               Table_Number => 0,
               Entry_Index  => PDPT_Idx,
               Table_Entry  => Entries.Create
                 (Dst_Offset  => PDPT_Idx,
                  Dst_Address => PD_Addr,
                  Readable    => True,
                  Writable    => not Is_PDPT_Page or Writable,
                  Executable  => not Is_PDPT_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;
      end loop;

      if Is_PDPT_Page then
         return;
      end if;

      for PD_Idx in Table_Range range PD_Idx_Start .. PD_Idx_End loop
         if Is_PD_Page then
            PT_Addr := Physical_Addr + Interfaces.Unsigned_64
              (PD_Idx - PD_Idx_Start) * PD_Page_Size;
         else
            PT_Addr := PT_Addr + Interfaces.Unsigned_64
              (PD_Idx - PD_Idx_Start) * Page_Size;
         end if;

         if not PD.Contains
           (Map          => Mem_Layout.PDs,
            Table_Number => 0,
            Entry_Index  => PD_Idx)
         then
            PD.Add_Entry
              (Map          => Mem_Layout.PDs,
               Table_Number => 0,
               Entry_Index  => PD_Idx,
               Table_Entry  => Entries.Create
                 (Dst_Offset  => PD_Idx,
                  Dst_Address => PT_Addr,
                  Readable    => True,
                  Writable    => not Is_PD_Page or Writable,
                  Executable  => not Is_PD_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;
      end loop;

      if Is_PD_Page then
         return;
      end if;

      for PT_Idx in Table_Range range PT_Idx_Start .. PT_Idx_End loop
         if not PT.Contains
           (Map          => Mem_Layout.PTs,
            Table_Number => 0,
            Entry_Index  => PT_Idx)
         then
            PT.Add_Entry
              (Map          => Mem_Layout.PTs,
               Table_Number => 0,
               Entry_Index  => PT_Idx,
               Table_Entry  => Entries.Create
                 (Dst_Offset  => PT_Idx,
                  Dst_Address => Physical_Addr,
                  Readable    => True,
                  Writable    => Writable,
                  Executable  => Executable,
                  Maps_Page   => True,
                  Global      => False,
                  Caching     => Caching));
         end if;

         Physical_Addr := Physical_Addr + Page_Size;
      end loop;
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   function Exists
     (Mem_Layout : Memory_Layout_Type;
      PML4_Index : Table_Range)
      return Boolean
   is
   begin
      return Tables.PML4.Contains
        (Table => Mem_Layout.PML4,
         Index => PML4_Index);
   end Exists;

   -------------------------------------------------------------------------

   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Tables.PML4.Get_Physical_Address (Table => Mem_Layout.PML4);
   end Get_Address;

   -------------------------------------------------------------------------

   procedure Get_Table_Count
     (Mem_Layout :     Memory_Layout_Type;
      PML4_Count : out Natural;
      PDPT_Count : out Natural;
      PD_Count   : out Natural;
      PT_Count   : out Natural)
   is
   begin

      --  There is max. one PML4 table.

      if Tables.PML4.Count (Table => Mem_Layout.PML4) > 0 then
         PML4_Count := 1;
      else
         PML4_Count := 0;
      end if;

      PDPT_Count := Tables.PDPT.Length (Map => Mem_Layout.PDPTs);
      PD_Count   := Tables.PD.Length (Map => Mem_Layout.PDs);
      PT_Count   := Tables.PT.Length (Map => Mem_Layout.PTs);
   end Get_Table_Count;

   -------------------------------------------------------------------------

   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64)
   is
   begin
      Tables.PML4.Set_Physical_Address
        (Table   => Mem_Layout.PML4,
         Address => Address);
   end Set_Address;

end Paging.Memory;
