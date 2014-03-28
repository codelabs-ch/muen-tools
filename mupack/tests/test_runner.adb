--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ahven.Text_Runner;
with Ahven.Framework;

with Image_Tests;
with Pack_Tests;
with Content_Provider_Tests;
with Util_Tests;
with Pre_Check_Tests;
with Post_Check_Tests;
with Manifest_Tests;

procedure Test_Runner
is
   use Ahven.Framework;

   S : constant Test_Suite_Access := Create_Suite
     (Suite_Name => "Mupack tests");
begin
   Add_Test (Suite => S.all,
             T     => new Image_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Pack_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Content_Provider_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Util_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Pre_Check_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Post_Check_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Manifest_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);
end Test_Runner;
