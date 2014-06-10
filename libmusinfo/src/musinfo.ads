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

package Musinfo
is

   --  Unique identifier used to designate a subject info memory region.
   --  "muinfo" with highest 2 bytes for counter.
   Muen_Subject_Info_Magic : constant := 16#0100_6f66_6e69_756d#;

   type Bit_Type is range 0 .. 1
     with
       Size => 1;

   type Bit_Array is array (Positive range <>) of Bit_Type
     with
       Pack;

   type Name_Size_Type is range 0 .. 63
     with
       Size => 6;

   subtype Name_Index_Type is Positive range 1 .. 63;

   type Name_Data_Type is new String
     (Name_Index_Type'First .. Name_Index_Type'Last)
     with
       Size => Name_Index_Type'Last * 8;

   --  A name is a string with an explicit length field and maximum size of 63
   --  characters.
   type Name_Type is record
      Length  : Name_Size_Type;
      Padding : Bit_Array (1 .. 2);
      Data    : Name_Data_Type;
   end record
     with
       Size => (1 + Name_Index_Type'Last) * 8;

   for Name_Type use record
      Length  at 0 range 0 .. 5;
      Padding at 0 range 6 .. 7;
      Data    at 1 range 0 .. 503;
   end record;

   Null_Name : constant Name_Type
     := (Length  => 0,
         Padding => (others => 0),
         Data    => (others => ASCII.NUL));

   type Channel_Flags_Type is record
      Writable   : Boolean;
      Has_Event  : Boolean;
      Has_Vector : Boolean;
      Padding    : Bit_Array (1 .. 5);
   end record
     with Size => 8;

   for Channel_Flags_Type use record
      Writable   at 0 range 0 .. 0;
      Has_Event  at 0 range 1 .. 1;
      Has_Vector at 0 range 2 .. 2;
      Padding    at 0 range 3 .. 7;
   end record;

   Null_Flags : constant Channel_Flags_Type
     := (Writable   => False,
         Has_Event  => False,
         Has_Vector => False,
         Padding    => (others => 0));

   type Event_Number_Range is range 0 .. 255
     with
       Size => 8;

   type Vector_Range is range 0 .. 255
     with
       Size => 8;

   Channel_Type_Size : constant := 88;

   --  A channel is described by its name, memory address and size. The
   --  Writable flag specifies if the memory region can be written to. Channels
   --  can optionally have an assigned vector or event which is indicated by
   --  the Has_Event/Vector flags.
   type Channel_Type is record
      Name    : Name_Type;
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64;
      Flags   : Channel_Flags_Type;
      Event   : Event_Number_Range;
      Vector  : Vector_Range;
      Padding : Bit_Array (1 .. 40);
   end record
     with
       Alignment => 8,
       Size      => Channel_Type_Size * 8;

   for Channel_Type use record
      Name    at  0 range 0 .. 511;
      Address at 64 range 0 .. 63;
      Size    at 72 range 0 .. 63;
      Flags   at 80 range 0 .. 7;
      Event   at 81 range 0 .. 7;
      Vector  at 82 range 0 .. 7;
      Padding at 83 range 0 .. 39;
   end record;

   Null_Channel : constant Channel_Type
     := (Name    => Null_Name,
         Address => 0,
         Size    => 0,
         Flags   => Null_Flags,
         Event   => 0,
         Vector  => 0,
         Padding => (others => 0));

   type Channel_Count_Type is range 0 .. 255
     with
       Size => 8;

   subtype Channel_Index_Type is Channel_Count_Type range
     1 .. Channel_Count_Type'Last;

   Channel_Array_Size : constant := Channel_Index_Type'Last
     * Channel_Type_Size;

   type Channel_Array is array (Channel_Index_Type) of Channel_Type
     with
       Pack,
       Alignment => 8;

   Subject_Info_Type_Size : constant := 8 + 1 + 7 + Channel_Array_Size;

   --  Subject info records enable subjects to determine what resources are
   --  provided to them at runtime.
   type Subject_Info_Type is record
      Magic         : Interfaces.Unsigned_64;
      Channel_Count : Channel_Count_Type;
      Padding       : Bit_Array (1 .. 56);
      Channels      : Channel_Array;
   end record
     with
       Size => Subject_Info_Type_Size * 8;

   for Subject_Info_Type use record
      Magic         at 0  range 0 .. 63;
      Channel_Count at 8  range 0 .. 7;
      Padding       at 9  range 0 .. 55;
      Channels      at 16 range 0 .. (Channel_Array_Size * 8) - 1;
   end record;

   Null_Subject_Info : constant Subject_Info_Type
     := (Magic         => Muen_Subject_Info_Magic,
         Channel_Count => Channel_Count_Type'First,
         Padding       => (others => 0),
         Channels      => (others => Null_Channel));

end Musinfo;
