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

package body DS.XSCT is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Write
     (CPUs       : CPU_Kernel_Map_Package.Map;
      Files      : File_Vector_Package.Vector;
      Output_Dir : String)
   is
      Output_File : constant String
        := Output_Dir & "/xsct.tcl";

      Template    : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.xsct_tcl);
      Rst_Entries, File_Entries, Run_Entries : Unbounded_String;
   begin
      for CPU in CPUs.Iterate loop
         declare
            ID : constant Natural := CPU_Kernel_Map_Package.Key (CPU);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.xsct_entry_rst_tcl);
         begin
            --  CPU 0 runs FSBL and is reset implicitly.
            if ID > 0 then
               Mulog.Log (Msg => "Write reset instruction for CPU" & ID'Img
                                  & " to '" & Output_File & "'");

               Mutools.Templates.Replace
                 (Template => Template,
                  Pattern  => "__cpu__",
                  Content  => ID'Img (ID'Img'First + 1 .. ID'Img'Last));

               Append
                 (Source   => Rst_Entries,
                  New_Item => Mutools.Templates.To_String (Template => Template));
            end if;
         end;
      end loop;

      for F of Files loop
         declare
            Filename : constant Unbounded_String
                 := (if F.Filename_Padded /= Null_Unbounded_String then
                       F.Filename_Padded else F.Filename);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.xsct_entry_file_tcl);
         begin
            Mulog.Log (Msg => "Write load instruction for '"
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
              (Source   => File_Entries,
               New_Item => Mutools.Templates.To_String
                 (Template => Template));
         end;
      end loop;

      for CPU in CPUs.Iterate loop
         declare
            ID : constant Natural := CPU_Kernel_Map_Package.Key (CPU);

            Template : Mutools.Templates.Template_Type
              := Mutools.Templates.Create
                (Content => String_Templates.xsct_entry_run_tcl);
         begin
            Mutools.Templates.Replace
              (Template => Template,
               Pattern  => "__cpu__",
               Content  => ID'Img (ID'Img'First + 1 .. ID'Img'Last));

            Append
              (Source   => Run_Entries,
               New_Item => Mutools.Templates.To_String (Template => Template));
         end;
      end loop;

      declare
         Rst   : constant String := To_String (Rst_Entries);
         Files : constant String := To_String (File_Entries);
         Run   : constant String := To_String (Run_Entries);
      begin
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => "__rst_entries__",
            Content  => Rst (Rst'First .. Rst'Last - 1));
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => "__file_entries__",
            Content  => Files (Files'First .. Files'Last - 1));
         Mutools.Templates.Replace
           (Template => Template,
            Pattern  => "__run_entries__",
            Content  => Run (Run'First .. Run'Last - 1));
      end;

      Mutools.Templates.Write
        (Template => Template,
         Filename => Output_File);
   end Write;

end DS.XSCT;
