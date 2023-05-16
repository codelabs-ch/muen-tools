--
--  Copyright (C) 2023, 2023  David Loosli <david@codelabs.ch>
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

with String_Templates;

package body DTS.Generator
is

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      pragma Unreferenced (Policy);

      DTS_Filename : constant String
        := Output_Dir & "/" & "devicetree.dts";

      Template : constant Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.devicetree_dsl);
   begin
      Mulog.Log (Level => Mulog.Info,
                 Msg   => DTS.Hello);
      Mutools.Templates.Write (Template => Template,
                               Filename => DTS_Filename);
      Mulog.Log (Level => Mulog.Info,
                 Msg   => "DTS should now be written!");
   end Write;

end DTS.Generator;
