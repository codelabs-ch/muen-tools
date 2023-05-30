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

with DOM.Core;

with Mutools.Templates;
with Muxml;

package DTS.Root
is

   procedure Write
     (Policy       : Muxml.XML_Data_Type;
      Subject      : DOM.Core.Node;
      Subject_Name : String;
      Filename     : String);

private

   procedure Add_Aliases_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node);

   procedure Add_APU_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node);

   procedure Add_Chosen_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node);

   procedure Add_Memory_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node);

end DTS.Root;
