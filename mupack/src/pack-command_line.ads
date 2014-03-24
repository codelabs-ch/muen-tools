--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Pack.Command_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return policy filename.
   function Get_Policy return String;

   --  Return output directory.
   function Get_Output_Dir return String;

   --  Retun input directory.
   function Get_Input_Dir return String;

private

   Policy                : Ada.Strings.Unbounded.Unbounded_String;
   Output_Dir, Input_Dir : Ada.Strings.Unbounded.Unbounded_String
     := Ada.Strings.Unbounded.To_Unbounded_String
       (Ada.Directories.Current_Directory);

end Pack.Command_Line;
