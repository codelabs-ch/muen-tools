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

with Ada.Command_Line;
with Ada.Exceptions;

with Mulog;
with Muxml;
with Mutools.Utils;
with Mucfgcheck.Validation_Errors;

with Comp_Vres_Alloc;
with Comp_Vres_Alloc.Cmd_Line;

procedure Mucfgcvresalloc
is
begin
   Comp_Vres_Alloc.Cmd_Line.Init
      (Description     => "Muen component virtual address allocator");
   Comp_Vres_Alloc.Run
     (Input_Spec       => Comp_Vres_Alloc.Cmd_Line.Get_Input_Spec,
      Include_Path     => Comp_Vres_Alloc.Cmd_Line.Get_Include_Path,
      Output_File_Name => Comp_Vres_Alloc.Cmd_Line.Get_Output_Filename);

exception
   when Comp_Vres_Alloc.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Mutools.Utils.File_Not_Found
      | Muxml.Validation_Error =>
      -- TODO mmmDEBUG: maybe add    Comp_Vres_Alloc.Checks.Validation_Error
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Atomatic allocation of virtual addresses failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when Mucfgcheck.Validation_Errors.Validation_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Semantic check failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Mucfgcheck.Validation_Errors.Get_Error_Message);
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucfgcvresalloc;
