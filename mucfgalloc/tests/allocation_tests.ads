--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Ahven.Framework;

package Allocation_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Detect overlapping physical memory
   procedure Overlapping_Physical_Memory;

   --  Perform automatic allocation from Format A XML
   procedure Automatic_Allocation;

   --  Allocate regions which have an upper limit specified
   procedure Limited_Allocation;

   --  Check whether device memory is treated correctly
   --  e.g. not used for allocation
   procedure Allocation_With_Devices;

   --  Check if overlaps between RAM an devices are handled
   procedure Overlapping_Devices;

   --  Check if overlaps between memory of different devices are handled
   procedure Overlap_Between_Devices;

   --  Check if overlaps between memory regions of the same device are handled
   procedure Overlap_Between_Device_Memory;

   --  Check whether file-backed regions are allocated first
   procedure File_Backed_First;

   --  Check whether fill pattern regions are allocated second
   procedure Fill_Pattern_Second;

private

   --  Create a directory if not present
   procedure Make_Directory (Name : String);

end Allocation_Tests;
