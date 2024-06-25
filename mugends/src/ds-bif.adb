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

package body DS.BIF is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Write
     (CPUs       : CPU_Kernel_Map_Package.Map;
      Files      : File_Vector_Package.Vector;
      Output_Dir : String)
   is
      Output_File : constant String
        := Output_Dir & "/bootgen.config";

      Template    : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.bootgen_config_dsl);
      Entries     : Unbounded_String;
   begin
      for CPU in CPUs.Iterate loop
         declare
            ID   : constant Natural := CPU_Kernel_Map_Package.Key (CPU);
            File : constant Loadable_File := CPUs (CPU);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.bootgen_entry_kernel_dsl);
         begin
            Mulog.Log (Msg => "Write load/startup instruction for CPU" & ID'Img
                               & " '" & To_String (File.Filename) & "' at "
                               & Mutools.Utils.To_Hex (Number => File.Address)
                               & " to '" & Output_File & "'");

            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__cpu__",
               Content  => ID'Img (ID'Img'First + 1 .. ID'Img'Last));
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__addr__",
               Content  => Mutools.Utils.To_Hex
                  (Number => File.Address,
                   Normalize  => False));
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__file__",
               Content  => To_String (File.Filename));

            Append
              (Source   => Entries,
               New_Item => Mutools.Templates.To_String (Template => Template));
         end;
      end loop;

      for F of Files loop
         -- Kernel text image is already loaded above.
         if not F.Kernel then
            declare
               Template : Mutools.Templates.Template_Type
                 := Mutools.Templates.Create
                   (Content => String_Templates.bootgen_entry_dsl);
            begin
               Mulog.Log (Msg => "Write load instruction for '"
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
         end if;
      end loop;

      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__entries__",
         Content  => To_String (Entries));

      Mutools.Templates.Write
        (Template => Template,
         Filename => Output_File);
   end Write;

end DS.BIF;
