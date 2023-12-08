--
--  Copyright (C) 2023  Tobias Brunner <tobias@codelabs.ch>
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

package body DS.UBOOT is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Write
     (CPUs       : CPU_Kernel_Map_Package.Map;
      Files      : File_Vector_Package.Vector;
      Output_Dir : String)
   is
      Output_File : constant String
        := Output_Dir & "/bootscript.cmd";

      Template    : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.bootscript_cmd_dsl);
      Entries     : Unbounded_String;
   begin
      for F of Files loop
         declare
            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.bootscript_entry_dsl);
         begin
            Mulog.Log (Msg => "Write TFTP load instruction for '"
                               & To_String (F.Filename) & "' at "
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
               Content  => To_String (F.Filename));

            Append
              (Source   => Entries,
               New_Item => Mutools.Templates.To_String (Template => Template));
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__load_entries__",
         Content  => To_String (Entries));

      Entries := To_Unbounded_String ("");

      for CPU in reverse CPUs.Iterate loop
         declare
            ID   : constant Natural := CPU_Kernel_Map_Package.Key (CPU);
            File : constant Loadable_File := CPUs (CPU);

            Release_Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.bootscript_entry_kernel_release_dsl);
            Go_Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.bootscript_entry_kernel_go_dsl);
         begin
            if ID > 0 then
               Mulog.Log (Msg => "Write release instruction for CPU" & ID'Img & " at "
                                  & Mutools.Utils.To_Hex (Number => File.Address)
                                  & " to '" & Output_File & "'");

               Mutools.Templates.Replace
                 (Template => Release_Template,
                  Pattern  => "__cpu__",
                  Content  => ID'Img (ID'Img'First + 1 .. ID'Img'Last));

               Mutools.Templates.Replace
                 (Template => Release_Template,
                  Pattern  => "__addr__",
                  Content  => Mutools.Utils.To_Hex
                     (Number => File.Address,
                      Normalize  => False));

               Append
                 (Source   => Entries,
                  New_Item => Mutools.Templates.To_String
                    (Template => Release_Template));
            else
               Mulog.Log (Msg => "Write go instruction for CPU" & ID'Img & " at "
                                  & Mutools.Utils.To_Hex (Number => File.Address)
                                  & " to '" & Output_File & "'");

               Mutools.Templates.Replace
                 (Template => Go_Template,
                  Pattern  => "__addr__",
                  Content  => Mutools.Utils.To_Hex
                     (Number => File.Address,
                      Normalize  => False));

               Append
                 (Source   => Entries,
                  New_Item => Mutools.Templates.To_String
                    (Template => Go_Template));
            end if;
         end;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__kernel_entries__",
         Content  => To_String (Entries));

      Mutools.Templates.Write
        (Template => Template,
         Filename => Output_File);
   end Write;

end DS.UBOOT;
