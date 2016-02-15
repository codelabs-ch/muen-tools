--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with DOM.Core;

package Mutools.PCI
is

   --  PCI Bus Device Function (BDF).
   type Bus_Range      is range 0 .. 255;
   type Device_Range   is range 0 .. 31;
   type Function_Range is range 0 .. 7;

   type BDF_Type is private;

   Null_BDF : constant BDF_Type;

   --  Create BDF object from given bus, device and function values.
   function Create
     (Bus    : Bus_Range;
      Device : Device_Range;
      Func   : Function_Range)
      return BDF_Type;

   --  I/O APIC bus(8)/dev(5)/func(3) f0:1f.00
   IOAPIC_Bus_Dev_Func : constant := 16#f0f8#;

   --  Convert given PCI BDF to IR SID (Intel VT-d spec 3.4.1).
   function To_SID (BDF : BDF_Type) return Interfaces.Unsigned_16;

   --  Return PCI BDF for device given by XML node. If the device is not a PCI
   --  device, Null_BDF is returned.
   function Get_BDF (Dev : DOM.Core.Node) return BDF_Type;

private

   type Null_Bus_Range      is range -1 .. Bus_Range'Last;
   type Null_Device_Range   is range -1 .. Device_Range'Last;
   type Null_Function_Range is range -1 .. Function_Range'Last;

   type BDF_Type is record
      Bus    : Null_Bus_Range;
      Device : Null_Device_Range;
      Func   : Null_Function_Range;
   end record;

   Null_BDF : constant BDF_Type := (Bus => -1, Device => -1, Func => -1);

end Mutools.PCI;
