--
--  Copyright (C) 2017  secunet Security Networks AG
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

with DOM.Core;

with Muxml;
with Mutools.Types;

package Bin_Split.Spec
is

   --  Add an entry to the component specification "Spec", corresponding to a
   --  section with name "Logical", to be filled with a bit pattern
   --  "Fill_Pattern".
   procedure Add_Fill_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Fill_Pattern    :        Interfaces.Unsigned_8 := 0;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64);

   --  Add an entry to the component specification "Spec", corresponding to a
   --  section to be read from the binary file named "File_Name". An optional
   --  hash value may be specified.
   procedure Add_File_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      File_Name       :        String;
      Hash            :        String := "";
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64);

   --  Returns the entry point as string from the given component spec with the
   --  specified architecture. If no entry point is present in the spec, then
   --  an empty string is returned.
   function Get_Entry_Point
     (Spec : Muxml.XML_Data_Type;
      Arch : Mutools.Types.Arch_Type)
      return String;

   --  Set RIP of component with the specified architecture to given entry
   --  point value.
   procedure Set_RIP
     (Spec        : in out Muxml.XML_Data_Type;
      Arch        :        Mutools.Types.Arch_Type;
      Entry_Point :        Interfaces.Unsigned_64);

private

   function Create_Memory_Node
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64)
      return DOM.Core.Element;

end Bin_Split.Spec;
