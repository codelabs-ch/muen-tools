--
--  Copyright (C) 2013, 2014  Alexander Senier <mail@senier.net>
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
with Mutools.Cmd_Line.Infile_Outdir;

with Alloc.Allocator;

procedure Mucfgalloc
is
begin
   Mutools.Cmd_Line.Infile_Outdir.Init
     (Description => "Assign physical addresses to all global memory " &
        "elements");
   Mutools.Cmd_Line.Infile_Outdir.Run
     (Kind    => Muxml.Format_A,
      Process => Alloc.Allocator.Write'Access);

exception
   when E : Muxml.Processing_Error
          | Alloc.Allocator.Out_Of_Memory =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Processing failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucfgalloc;
