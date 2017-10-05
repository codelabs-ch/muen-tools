--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

with Mutools.Files;

with Mulog;

with Binary.Sections;

package body Binary.Files is

   procedure Open
     (Filename   :     String;
      Descriptor : out File_Type)
   is
   begin
      Bfd.Files.Open (File => Bfd.Files.File_Type (Descriptor),
                      Name => Filename);
      if not Bfd.Files.Check_Format
        (File   => Bfd.Files.File_Type (Descriptor),
         Expect => Bfd.Files.OBJECT)
      then
         raise Types.Bin_Split_Error
           with "File '" & Filename & "' is not a binary object file";
      end if;

   exception
      when Bfd.OPEN_ERROR =>
         raise Types.Bin_Split_Error
           with "Unable to open file '" & Filename & "'";
   end Open;

   --------------------------------------------------------------------------

   procedure Write_Compound_Section
     (Info             : Types.Compound_Section_Info;
      Output_File_Name : String;
      Descriptor       : Binary.Files.File_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Out_File: Ada.Streams.Stream_IO.File_Type;
   begin

      Mutools.Files.Open
        (Filename => Output_File_Name,
         File     => Out_File);

      for SI of Info.Infos.all loop
         if SI.Write_To_File then
            declare
               Sec : constant Binary.Sections.Section
                 := Binary.Sections.Get_Section
                   (Descriptor   => Descriptor,
                    Section_Name => Ada.Strings.Unbounded.To_String (SI.Name));
               Buf : Ada.Streams.Stream_Element_Array
                 (1 .. Ada.Streams.Stream_Element_Offset
                         (Binary.Sections.Get_Size (Sec)));
               Last : Ada.Streams.Stream_Element_Offset;
            begin
               Binary.Sections.Get_Section_Contents
                 (File => Descriptor,
                  S    => Sec,
                  Item => Buf,
                  Last => Last);

               declare
                  Bytes_Read : constant Ada.Streams.Stream_Element_Offset
                    := Last - Buf'First + 1;
               begin
                  Mulog.Log (Level => Mulog.Debug,
                             Msg   => "Read " & Bytes_Read'Img & " bytes.");
               end;

               Ada.Streams.Stream_IO.Write
                 (File => Out_File,
                  Item => Ada.Streams.Stream_Element_Array (Buf));

               Mulog.Log (Level => Mulog.Debug,
                          Msg   =>
                            "Written section '"
                              & Ada.Strings.Unbounded.To_String (SI.Name)
                              & "' to file '" & Output_File_Name & "'.");
            end;
         end if;
      end loop;

      Ada.Streams.Stream_IO.Close (Out_File);

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (Out_File);
         raise;

   end Write_Compound_Section;

end Binary.Files;
