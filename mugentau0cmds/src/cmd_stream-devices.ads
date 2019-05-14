--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Cmd_Stream.Devices
is

   --  Generate command stream to create physical PCI devices of given system
   --  policy.
   procedure Create_Physical_PCI_Devices
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type);

   --  Generate command stream to create physical legacy devices of given
   --  system policy.
   procedure Create_Physical_Legacy_Devices
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type);

end Cmd_Stream.Devices;
