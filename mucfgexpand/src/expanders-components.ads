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

with Muxml;

package Expanders.Components
is

   --  Add component binary to subjects as physical memory regions and
   --  mappings.
   procedure Add_Binaries (Data : in out Muxml.XML_Data_Type);

   --  Expand logical component channels to subject channels.
   procedure Add_Channels (Data : in out Muxml.XML_Data_Type);

   --  Remove components section from policy.
   procedure Remove_Components (Data : in out Muxml.XML_Data_Type);

   --  Remove component element from subject nodes.
   procedure Remove_Component_Reference (Data : in out Muxml.XML_Data_Type);

end Expanders.Components;
