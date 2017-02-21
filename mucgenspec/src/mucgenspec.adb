--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;
with Ada.Command_Line;

with Mulog;
with Muxml;

with Cspec.Utils;
with Cspec.Cmd_Line;

procedure Mucgenspec
is
begin
   Cspec.Cmd_Line.Init
     (Description => "Component logical resource constants generator");
   Cspec.Run (Component_Spec   => Cspec.Cmd_Line.Get_Component_Spec,
              Output_Directory => Cspec.Cmd_Line.Get_Output_Dir,
              Include_Path     => Cspec.Cmd_Line.Get_Include_Path);

exception
   when Cspec.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Muxml.Validation_Error
      | Cspec.Component_Not_Found
      | Cspec.Utils.Attribute_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Spec generation failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucgenspec;
