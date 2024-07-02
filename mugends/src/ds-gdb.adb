--
--  Copyright (C) 2023-2024  Tobias Brunner <tobias@codelabs.ch>
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

with Mulog;
with Mutools.Templates;
with Mutools.Utils;

with String_Templates;

package body DS.GDB is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Write
     (CPUs       : CPU_Kernel_Map_Package.Map;
      Files      : File_Vector_Package.Vector;
      Output_Dir : String)
   is
      Output_File : constant String
        := Output_Dir & "/gdbinit.config";

      Template    : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.gdbinit_config_dsl);
      Entries     : Unbounded_String;
   begin
      --  Restore the files in decreasing address order.
      for F of reverse Files loop
         declare
            Filename : constant Unbounded_String
              := (if F.Filename_Padded /= Null_Unbounded_String then
                    F.Filename_Padded else F.Filename);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.gdbinit_entry_dsl);
         begin
            Mulog.Log (Msg => "Write restore instruction for '"
                               & To_String (Filename) & "' at "
                               & Mutools.Utils.To_Hex (Number => F.Address)
                               & " to '" & Output_File & "'");

            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__addr__",
               Content  => Mutools.Utils.To_Hex
                  (Number => F.Address,
                   Normalize  => False));
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__file__",
               Content  => To_String (Filename));

            Append
              (Source   => Entries,
               New_Item => Mutools.Templates.To_String (Template => Template));
         end;
      end loop;

      --  Configure threads in GDB in decreasing order.
      for CPU in reverse CPUs.Iterate loop
         declare
            ID        : constant Natural := CPU_Kernel_Map_Package.Key (CPU);
            Thread_ID : constant Natural := ID + 1;
            File      : constant Loadable_File := CPUs (CPU);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.gdbinit_entry_kernel_dsl);
         begin
            Mulog.Log (Msg => "Write setup for thread" & Thread_ID'Img & "/CPU"
                               & ID'Img & " to '" & Output_File & "'");

            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__thread_id__",
               Content  => Thread_ID'Img (Thread_ID'Img'First + 1 .. Thread_ID'Img'Last));
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__addr__",
               Content  => Mutools.Utils.To_Hex
                  (Number => File.Address,
                   Normalize  => False));

            Append
              (Source   => Entries,
               New_Item => Mutools.Templates.To_String (Template => Template));
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__entries__",
         Content  => To_String (Entries));

      Mutools.Templates.Write
        (Template => Template,
         Filename => Output_File);
   end Write;

end DS.GDB;
