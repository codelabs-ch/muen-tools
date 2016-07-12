--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutools.Cmd_Line;

package body Merge.Cmd_Line
is

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Get_Config_File return String
   is
   begin
      return S (Config_File);
   end Get_Config_File;

   -------------------------------------------------------------------------

   function Get_Include_Path return String
   is
   begin
      return S (Include_Path);
   end Get_Include_Path;

   -------------------------------------------------------------------------

   function Get_Output_File return String
   is
   begin
      return S (Output_File);
   end Get_Output_File;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline : Mutools.Cmd_Line.Config_Type;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <config_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Config_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Output_File = Null_Unbounded_String
        or Config_File = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Merge.Cmd_Line;
