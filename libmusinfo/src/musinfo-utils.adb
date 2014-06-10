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

package body Musinfo.Utils
is

   -------------------------------------------------------------------------

   procedure Append_Channel
     (Info    : in out Subject_Info_Type;
      Channel :        Channel_Type)
   is
   begin
      Info.Channel_Count                 := Info.Channel_Count + 1;
      Info.Channels (Info.Channel_Count) := Channel;
   end Append_Channel;

   -------------------------------------------------------------------------

   function Create_Channel
     (Name       : Name_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Writable   : Boolean;
      Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Type
   is
   begin
      return Channel : Channel_Type := Null_Channel do
         Channel.Name             := Name;
         Channel.Address          := Address;
         Channel.Size             := Size;
         Channel.Flags.Writable   := Writable;
         Channel.Flags.Has_Event  := Has_Event;
         Channel.Flags.Has_Vector := Has_Vector;
         Channel.Event            := Event;
         Channel.Vector           := Vector;
      end return;
   end Create_Channel;

   -------------------------------------------------------------------------

   function Create_Name (Str : String) return Name_Type
   is
      Name    : Name_Type := Null_Name;
      Cur_Idx : Positive  := Name_Index_Type'First;
   begin
      Name.Length := Str'Length;

      for Char of Str loop
         Name.Data (Cur_Idx) := Char;
         Cur_Idx             := Cur_Idx + 1;
      end loop;

      return Name;
   end Create_Name;

end Musinfo.Utils;
