--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Mulog;

with Stackcheck.Files;
with Stackcheck.Input;
with Stackcheck.Types;
with Stackcheck.Utils;

package body Stackcheck
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Run
     (Project_File : String;
      Limit        : Natural)
   is
      Paths     : constant Files.Path_Names
        := Files.Get_Object_Dirs (GPR_File => Project_File);
      CFG       : Types.Control_Flow_Graph_Type;
      Max_Usage : Natural := 0;
      Max_User  : Unbounded_String;

      --  Parse control flow information in given file.
      procedure Parse_File (File : Ada.Text_IO.File_Type);

      --  Check given subprogram node if its worst-case stack usage is larger
      --  than the maximum specified by the user.
      procedure Check_Stack_Usage (Node : in out Types.Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Check_Stack_Usage (Node : in out Types.Subprogram_Type)
      is
         Cur_Usage : constant Natural
           := Types.Get_Max_Stack_Usage (Subprogram => Node);
      begin
         if Cur_Usage  > Max_Usage then
            Max_Usage := Types.Get_Max_Stack_Usage (Subprogram => Node);
            Max_User  := To_Unbounded_String
              (Types.Get_Name (Subprogram => Node));
         end if;

         if Cur_Usage > Limit then
            Mulog.Log (Msg => "Stack limit exceeded by "
                       & Utils.Entity_To_Ada_Name
                         (Str => Types.Get_Name (Subprogram => Node))
                       & ":" & Cur_Usage'Img & " bytes");
         end if;
      end Check_Stack_Usage;

      ----------------------------------------------------------------------

      procedure Parse_File (File : Ada.Text_IO.File_Type)
      is
      begin
         while not Ada.Text_IO.End_Of_File (File => File) loop
            declare
               Cur_Line : constant String
                 := Ada.Text_IO.Get_Line (File => File);
            begin
               Input.Parse_Line (Data  => Cur_Line,
                                 Graph => CFG);
            end;
         end loop;
      end Parse_File;
   begin
      Mulog.Log (Msg => "Processing project file '" & Project_File & "'");

      for Path of Paths loop
         Mulog.Log (Msg => "Processing directory '" & To_String (Path) & "'");
         Files.For_Each_File (Path    => To_String (Path),
                              Pattern => "*.ci",
                              Process => Parse_File'Access);
      end loop;

      --  Add stack usage information for memcmp subprogram provided by RTS.

      Types.Add_Node (Graph      => CFG,
                      Subprogram => Types.Create (Name        => "memcmp",
                                                  Stack_Usage => 0));
      Types.Calculate_Stack_Usage (Graph => CFG);

      Mulog.Log (Msg => "Stack limit set to" & Limit'Img & " bytes");

      Types.Iterate (Graph   => CFG,
                     Process => Check_Stack_Usage'Access);

      if Max_User /= Null_Unbounded_String then
         Mulog.Log (Msg => "Largest stack usage by "
                    & Utils.Entity_To_Ada_Name (To_String (Max_User))
                    & " with" & Max_Usage'Img & " bytes");
      end if;
   end Run;

end Stackcheck;
