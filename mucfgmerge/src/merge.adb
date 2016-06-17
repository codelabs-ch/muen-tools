--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with GNAT.Directory_Operations;

with Muxml.Utils;
with Mulog;
with Mutools.System_Config;

with Mergers;
with Merge.Checks;

package body Merge
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Run
     (Config_File : String;
      Output_File : String)
   is
      Config : Muxml.XML_Data_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing system config '" & Config_File & "'");
      Muxml.Parse (Data => Config,
                   Kind => Muxml.System_Config,
                   File => Config_File);
      Checks.Required_Config_Values (Policy => Config);

      declare
         Policy_File : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "system");
      begin
         Mulog.Log (Msg => "Using policy file '" & Policy_File & "'");
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => Policy_File);
         Muxml.Utils.Merge (Left      => Policy.Doc,
                            Right     => Config.Doc,
                            List_Tags => (1 => U ("boolean"),
                                          2 => U ("integer"),
                                          3 => U ("string")));
         Mergers.Merge_XIncludes
           (Policy  => Policy,
            Basedir => GNAT.Directory_Operations.Dir_Name
              (Path => Policy_File));
      end;

      declare
         Hardware_File : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "hardware");
      begin
         Mulog.Log (Msg => "Using hardware file '" & Hardware_File & "'");
         Mergers.Merge_Hardware
           (Policy        => Policy,
            Hardware_File => Hardware_File);
      end;

      if Mutools.System_Config.Has_String
        (Data => Config,
         Name => "additional_hardware")
      then
         declare
            Additional_Hw_File : constant String
              := Mutools.System_Config.Get_Value
                (Data => Config,
                 Name => "additional_hardware");
         begin
            Mulog.Log (Msg => "Using additional hardware file '"
                       & Additional_Hw_File & "'");
            Mergers.Merge_Hardware
              (Policy        => Policy,
               Hardware_File => Additional_Hw_File);
         end;
      end if;

      declare
         Platform_File : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "platform");
      begin
         Mulog.Log (Msg => "Using platform file '" & Platform_File & "'");
         Mergers.Merge_Platform
           (Policy        => Policy,
            Platform_File => Platform_File);
      end;

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/expressions");

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_Src,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '" & Output_File & "'");
   end Run;

end Merge;
