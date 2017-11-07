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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Interfaces;

with Mulog;

with Muxml;

with Mutools.Utils;
with Mutools.Constants;

with Bin_Split.Utils;
with Bin_Split.Spec;
with Bin_Split.Files;

package body Bin_Split.Run
is

   function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   --------------------------------------------------------------------------

   procedure Check_Alignment (Section : Bfd.Sections.Section)
   is
      use type Bfd.Unsigned_64;

      package MC renames Mutools.Constants;
   begin
      if Section.Vma mod MC.Page_Size /= 0 then
         raise Bin_Split_Error
           with "Section '" & Bfd.Sections.Get_Name (Section)
             & "' is not page-aligned";
      end if;

      if Section.Vma /= Section.Lma then
         raise Bin_Split_Error
           with "LMA address of section '"
             & Bfd.Sections.Get_Name (Section)
             & "' is not equal to its VMA address";
      end if;
   end Check_Alignment;

   --------------------------------------------------------------------------

   procedure Check_Flags
     (Sec_Info   : Types.Section_Info;
      Descriptor : Bfd.Files.File_Type)
   is
      Sec : constant Bfd.Sections.Section
        := Bfd.Sections.Find_Section (File => Descriptor,
                                      Name => S (Sec_Info.Name));
   begin
      if Sec.Flags /= Sec_Info.Flags then
         raise Bin_Split_Error
           with "Unexpected flags for section '"
             & Bfd.Sections.Get_Name (Sec)
             & "': "
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64 (Sec_Info.Flags))
             & " /= "
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64
                   (Sec.Flags));
      end if;

      if (Sec.Flags and Bfd.Constants.SEC_DEBUGGING) /= 0 then
         raise Bin_Split_Error
           with "Section '"
             & Bfd.Sections.Get_Name (Sec)
             & "' is not expected to carry a debugging flag";
      end if;
   end Check_Flags;

   --------------------------------------------------------------------------

   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type)
   is
      package BS renames Bfd.Sections;

      Sect_It : BS.Section_Iterator
        := Bfd.Sections.Get_Sections (Descriptor);
   begin
      while BS.Has_Element (Sect_It) loop
         declare
            Sec : constant Bfd.Sections.Section
              := BS.Element (Sect_It);
         begin
            if (Sec.Flags and Bfd.Constants.SEC_DEBUGGING) /= 0 then
               Mulog.Log
                 (Level => Mulog.Debug,
                  Msg   => "Ignoring debugging section '"
                    & Bfd.Sections.Get_Name (Sec) & "'.");
            elsif not Is_Valid_Section
              (Section_Name  => Bfd.Sections.Get_Name (Sec),
               Section_Infos => Section_Infos)
            then
               raise Bin_Split_Error
                 with "Unexpected section name '"
                   & Bfd.Sections.Get_Name (Sec) & "'";
            end if;
         end;

         BS.Next (Sect_It);
      end loop;
   end Check_Section_Names;

   --------------------------------------------------------------------------

   function Is_Valid_Section
     (Section_Name  : String;
      Section_Infos : Types.SI_Array)
      return Boolean
   is
      Found_Section : Boolean := False;
   begin
      Loop_Section_Infos:
      for SI of Section_Infos loop
         if S (SI.Name) = Section_Name then
            Found_Section := True;
            exit Loop_Section_Infos;
         end if;
      end loop Loop_Section_Infos;

      return Found_Section;
   end Is_Valid_Section;

   --------------------------------------------------------------------------

   procedure Run
     (Spec_File        : String;
      Binary_File      : String;
      Output_Spec_File : String;
      Output_Dir       : String := "")
   is
      package BS renames Bfd.Sections;

      Spec       : Muxml.XML_Data_Type;
      Descriptor : Bfd.Files.File_Type;

      Base_Name : constant String := Ada.Directories.Base_Name (Binary_File);
   begin
      Utils.Make_Output_Directory (Dir_Name => Output_Dir);

      Files.Open (Filename => Binary_File, Descriptor => Descriptor);

      Mulog.Log (Msg => "Processing cspec file '" & Spec_File & "'");
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,  --  TODO: set to correct schema.
                   File => Spec_File);

      Check_Section_Names (Descriptor => Descriptor);

      for SI of Section_Infos loop
         declare
            Sec : constant BS.Section
              := BS.Find_Section (File => Descriptor,
                                  Name => S (SI.Name));
            Section_Name : constant String
              := Ada.Strings.Fixed.Trim
                (Source => S (SI.Name),
                 Left   => Ada.Strings.Maps.To_Set (Singleton => '.'),
                 Right  => Ada.Strings.Maps.Null_Set);
            Output_File_Name : constant String
              := Base_Name & "_" & Section_Name;
            Size : constant Interfaces.Unsigned_64
              := Utils.Round_Up (Address => Interfaces.Unsigned_64 (Sec.Size));
         begin
            Check_Alignment (Section => Sec);

            Check_Flags (Sec_Info => SI,
                         Descriptor => Descriptor);

            Mulog.Log (Msg => "Found Section '" & BS.Get_Name (Sec)
                         & "' with size "
                         & Mutools.Utils.To_Hex
                           (Number => Interfaces.Unsigned_64 (Sec.Size))
                         & " @ "
                         & Mutools.Utils.To_Hex
                           (Number => Interfaces.Unsigned_64 (Sec.Lma))
                         & ".");

            if SI.Fill then
               Bin_Split.Spec.Add_Fill_Entry
                 (Spec            => Spec,
                  Logical         => Section_Name,
                  Size            => Size,
                  Virtual_Address => Interfaces.Unsigned_64 (Sec.Vma),
                  Writable        => SI.Writable,
                  Executable      => SI.Executable,
                  Fill_Pattern    => 16#0#);
            else
               Bin_Split.Spec.Add_File_Entry
                 (Spec            => Spec,
                  Logical         => Section_Name,
                  Size            => Size,
                  Virtual_Address => Interfaces.Unsigned_64 (Sec.Vma),
                  File_Name       => Output_File_Name,
                  Writable        => SI.Writable,
                  Executable      => SI.Executable);

                  Files.Write_Section
                    (Info             => SI,
                     Output_File_Name => Output_Dir & "/" & Output_File_Name,
                     Descriptor       => Descriptor);
            end if;
         end;
      end loop;

      Muxml.Write
        (Data => Spec,
         Kind => Muxml.None,
         File => Output_Dir & "/" & Output_Spec_File);

   exception
      when others =>
         Bfd.Files.Close (File => Descriptor);
         raise;
   end Run;

end Bin_Split.Run;
