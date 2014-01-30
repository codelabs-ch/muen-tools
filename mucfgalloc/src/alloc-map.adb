--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

package body Alloc.Map
is

   procedure Allocate_Fixed
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;
      Curr : Cursor := First (Map.Data);

   begin
      while Curr /= No_Element
      loop
         if Element (Curr).First_Address > Last_Address then
            raise Invalid_Fixed_Allocation;
         end if;
         exit when Element (Curr).Kind = Empty and
                   Element (Curr).First_Address <= First_Address and
                   Last_Address <= Element (Curr).Last_Address;
         Next (Curr);
      end loop;

      if Curr = No_Element then
         raise Invalid_Fixed_Allocation;
      end if;

      Reserve
         (Map           => Map,
          Curr          => Curr,
          Name          => Ada.Strings.Unbounded.Null_Unbounded_String,
          First_Address => First_Address,
          Last_Address  => Last_Address);

   end Allocate_Fixed;

   ----------------------------------------------------------------------------

   procedure Allocate_Variable
      (Map       : in out Map_Type;
       Name      :        Ada.Strings.Unbounded.Unbounded_String;
       Size      :        Interfaces.Unsigned_64;
       Alignment :        Interfaces.Unsigned_64 := 1)
   is
      use Region_List_Package;
      Curr            : Cursor := First (Map.Data);
      First_Multiple  : Interfaces.Unsigned_64;
   begin
      while Curr /= No_Element
      loop
         First_Multiple :=
            ((Element (Curr).First_Address + Alignment - 1) /
             Alignment) * Alignment;

         exit when Element (Curr).Kind = Empty and
                   First_Multiple + Size - 1 <= Element (Curr).Last_Address;
         Next (Curr);
      end loop;

      if Curr = No_Element then
         raise Out_Of_Memory;
      end if;

      Reserve
         (Map           => Map,
          Curr          => Curr,
          Name          => Name,
          First_Address => First_Multiple,
          Last_Address  => First_Multiple + Size - 1);

   end Allocate_Variable;

   ----------------------------------------------------------------------------

   procedure Insert_Empty_Region
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;
      Curr, Prev : Cursor;

      --  Update the First_Address field of a region with First_Address
      procedure Set_First_Address (Element : in out Region_Type);
      procedure Set_First_Address (Element : in out Region_Type)
      is
      begin
         Element.First_Address := First_Address;
      end Set_First_Address;

      --  Update the Last_Address field of a region with Last_Address
      procedure Set_Last_Address (Element : in out Region_Type);
      procedure Set_Last_Address (Element : in out Region_Type)
      is
      begin
         Element.Last_Address := Last_Address;
      end Set_Last_Address;
   begin
      Curr := First (Map.Data);
      while Curr /= No_Element and then
            First_Address > Element (Curr).Last_Address
      loop
         Prev := Curr;
         Curr := Next (Curr);

         exit when
            Element (Prev).Last_Address < First_Address and
            (Curr = No_Element or else
             Last_Address < Element (Curr).First_Address);
      end loop;

      --  Overlap check
      if Curr /= No_Element and then
         Element (Curr).First_Address <= Last_Address
      then
         raise Overlapping_Empty_Region;
      end if;

      --  Check for adjacent areas and merge them. There can be 4 scenarios:
      --
      --    (1) Last_Address + 1 = Curr.First_Address and
      --        Prev.Last_Address + 1 = First_Address =>
      --        Prev.Last_Address := Curr.Last_Address, Remove Curr.
      --    (2) Last_Address + 1 = Curr.First_Address =>
      --        Curr.First_Address := First_Address
      --    (3) Prev.Last_Address + 1 = First_Address =>
      --        Prev.Last_Address := Last_Address
      --    (4) Otherwise =>
      --        Insert new element before Curr

      declare
         Adjacent_Right : constant Boolean := Curr /= No_Element and then
            Last_Address + 1 = Element (Curr).First_Address;
         Adjacent_Left : constant Boolean := Prev /= No_Element and then
            Element (Prev).Last_Address + 1 = First_Address;
      begin
         if Adjacent_Right and Adjacent_Left then
            --  (1)
            Update_Element (Map.Data, Prev, Set_Last_Address'Access);
            Delete (Map.Data, Curr);
         elsif Adjacent_Right then
            --  (2)
            Update_Element (Map.Data, Curr, Set_First_Address'Access);
         elsif Adjacent_Left then
            --  (3)
            Update_Element (Map.Data, Prev, Set_Last_Address'Access);
         else
            --  (4)
            Insert
               (Container => Map.Data,
                Before    => Curr,
                New_Item  => Region_Type'
                              (Kind          => Empty,
                               Name          =>
                                  Ada.Strings.Unbounded.Null_Unbounded_String,
                               First_Address => First_Address,
                               Last_Address  => Last_Address));
         end if;
      end;

   end Insert_Empty_Region;

   ----------------------------------------------------------------------------

   procedure Iterate
      (Map     : Map_Type;
       Process : not null access procedure (Region : Region_Type))
   is
      use Region_List_Package;

      procedure P (Position : Cursor);
      procedure P (Position : Cursor)
      is
      begin
         Process (Element (Position));
      end P;
   begin
      Iterate (Map.Data, P'Access);
   end Iterate;

   ----------------------------------------------------------------------------

   procedure Reserve
      (Map           : in out Map_Type;
       Curr          :        Region_List_Package.Cursor;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;

      Match_First : constant Boolean :=
         Element (Curr).First_Address = First_Address;
      Match_Last : constant Boolean :=
         Element (Curr).Last_Address = Last_Address;

      --  Update the Kind field with 'Allocated'
      procedure Allocate (Element : in out Region_Type);
      procedure Allocate (Element : in out Region_Type)
      is
      begin
         Element.Kind := Allocated;
      end Allocate;

      --  Update the First_Address field of a region with Last_Address + 1
      procedure Set_First_After_Last (Element : in out Region_Type);
      procedure Set_First_After_Last (Element : in out Region_Type)
      is
      begin
         Element.First_Address := Last_Address + 1;
      end Set_First_After_Last;

      --  Update the First_Address field of a region with Last_Address + 1
      procedure Set_First_Past_Last (Element : in out Region_Type);
      procedure Set_First_Past_Last (Element : in out Region_Type)
      is
      begin
         Element.First_Address := Last_Address + 1;
      end Set_First_Past_Last;

      --  Update the First_Address field of a region with First_Address - 1
      procedure Set_First_To_First (Element : in out Region_Type);
      procedure Set_First_To_First (Element : in out Region_Type)
      is
      begin
         Element.First_Address := First_Address;
      end Set_First_To_First;

      --  Update the Name field of a region
      procedure Set_Name (Element : in out Region_Type);
      procedure Set_Name (Element : in out Region_Type)
      is
      begin
         Element.Name := Name;
      end Set_Name;

   begin

      --  Allocate part of a given empty region
      --
      --  (1) Curr.First_Address = First_Address and
      --      Curr.Last_Address = Last_Address =>
      --      Curr.Kind := Allocated;
      --  (2) Curr.First_Address = First_Address =>
      --      Curr.First_Address := Last_Address + 1;
      --      Insert (First_Address, Last_Address, Allocated) before Curr
      --  (3) Curr.Last_Address = Last_Address =>
      --      Insert (Curr.First_Address, First_Address - 1, Empty) before Curr
      --      Curr.First_Address := First_Address;
      --      Curr.Kind := Allocated;
      --  (4) Otherwise =>
      --      Insert (Curr.First_Address, First_Address - 1, Empty) before Curr
      --      Insert (First_Address, Last_Address, Allocated) before Curr
      --      Curr.First_Address := Last_Address + 1

      if Match_First and Match_Last then
         --  (1)
         Update_Element (Map.Data, Curr, Allocate'Access);
         Update_Element (Map.Data, Curr, Set_Name'Access);
      elsif Match_First then
         --  (2)
         Update_Element (Map.Data, Curr, Set_First_After_Last'Access);
         Insert
            (Container => Map.Data,
             Before    => Curr,
             New_Item  => Region_Type'
               (First_Address => First_Address,
                Last_Address  => Last_Address,
                Name          => Name,
                Kind          => Allocated));
      elsif Match_Last then
         --  (3)
         Insert
            (Container => Map.Data,
             Before    => Curr,
             New_Item  => Region_Type'
               (First_Address => Element (Curr).First_Address,
                Last_Address  => First_Address - 1,
                Name          => Ada.Strings.Unbounded.Null_Unbounded_String,
                Kind          => Empty));
         Update_Element (Map.Data, Curr, Set_First_To_First'Access);
         Update_Element (Map.Data, Curr, Set_Name'Access);
         Update_Element (Map.Data, Curr, Allocate'Access);
      else
         --  (4)
         Insert
            (Container => Map.Data,
             Before    => Curr,
             New_Item  => Region_Type'
               (First_Address => Element (Curr).First_Address,
                Last_Address  => First_Address - 1,
                Name          => Ada.Strings.Unbounded.Null_Unbounded_String,
                Kind          => Empty));
         Insert
            (Container => Map.Data,
             Before    => Curr,
             New_Item  => Region_Type'
               (First_Address => First_Address,
                Last_Address  => Last_Address,
                Name          => Name,
                Kind          => Allocated));
         Update_Element (Map.Data, Curr, Set_First_Past_Last'Access);
      end if;

   end Reserve;

end Alloc.Map;
