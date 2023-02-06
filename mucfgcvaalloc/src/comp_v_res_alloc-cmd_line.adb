--
--  Copyright (C) 2023 secunet Security Networks AG
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

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Comp_V_Res_Alloc.Cmd_Line
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

   function Get_Include_Path return String
   is
   begin
      return S (Include_Path);
   end Get_Include_Path;

   -------------------------------------------------------------------------

   function Get_Input_Spec return String
   is
   begin
      return S (Input_Spec);
   end Get_Input_Spec;

   -------------------------------------------------------------------------

   function Get_Output_Filename return String
   is
   begin
      return S (Output_Filename);
   end Get_Output_Filename;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline     : Mutools.Cmd_Line.Config_Type;
      Include_Dir : aliased GNAT.Strings.String_Access;

   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <input_spec_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Include_Dir'Access,
         Switch      => "-I:",
         Long_Switch => "--include-path:",
         Help        => "Colon-separated list of include paths");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);

         if Include_Dir'Length /= 0 then
            Include_Path := U (Include_Dir.all);
         end if;
         GNAT.Strings.Free (X => Include_Dir);

      exception
         when GNAT.Command_Line.Invalid_Switch |
           GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Input_Spec := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_Filename := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Input_Spec = Null_Unbounded_String
        or Output_Filename = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Comp_V_Res_Alloc.Cmd_Line;
