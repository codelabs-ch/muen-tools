--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Sinfo.Generator.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Write_Sinfo (Arch : String)
      is
         Policy        : Muxml.XML_Data_Type;
         Subject_Sinfo : constant String := "obj/lnx_sinfo";
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy_" & Arch & ".xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/lnx_sinfo_" & Arch,
                  Filename2 => Subject_Sinfo),
                 Message   => "Subject info file mismatch (" & Arch & ")");
         Ada.Directories.Delete_File (Name => Subject_Sinfo);
      end Write_Sinfo;

      ----------------------------------------------------------------------

      --  Verify that device resources of a device with an explicitly empty
      --  <bars/> element are encoded with the No_BAR_Config marker, while
      --  configured device BAR indices are exported unchanged.
      procedure Check_No_BAR_Config
      is
         use type Interfaces.Unsigned_16;
         use type Musinfo.Name_Type;
         use type Musinfo.Resource_Kind;

         subtype Sinfo_Stream is Ada.Streams.Stream_Element_Array
           (1 .. Musinfo.Subject_Info_Type_Size);
         function Convert is new Ada.Unchecked_Conversion
           (Source => Sinfo_Stream,
            Target => Musinfo.Subject_Info_Type);

         Policy : Muxml.XML_Data_Type;
         File   : Ada.Streams.Stream_IO.File_Type;
         Stream : Sinfo_Stream;
         Last   : Ada.Streams.Stream_Element_Offset;
         Info   : Musinfo.Subject_Info_Type;
         Found  : Boolean := False;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy_x86_64.xml");
         Write (Output_Dir => "obj",
                Policy     => Policy);
         Ada.Streams.Stream_IO.Open
           (File => File,
            Mode => Ada.Streams.Stream_IO.In_File,
            Name => "obj/lnx_sinfo");
         Ada.Streams.Stream_IO.Read (File => File, Item => Stream, Last => Last);
         Ada.Streams.Stream_IO.Close (File => File);
         Info := Convert (S => Stream);

         if Info.Resource_Count > 0 then
            for I in 1 .. Natural (Info.Resource_Count) loop
               declare
                  Res : constant Musinfo.Resource_Type
                    := Info.Resources (Musinfo.Resource_Index_Type (I));
               begin
                  if Res.Kind = Musinfo.Res_Device_IO_Port then
                     if Res.Name = Sinfo.Utils.Create_Name
                       (Str => "manual0_port1")
                     then
                        Assert (Condition => Res.Dev_IO_Port_Data.BAR_Idx
                                  = Musinfo.No_BAR_Config,
                                Message   => "Manual device port BAR index");
                        Found := True;
                     elsif Res.Name = Sinfo.Utils.Create_Name
                       (Str => "eth0_port1")
                     then
                        Assert (Condition => Res.Dev_IO_Port_Data.BAR_Idx = 3,
                                Message   => "Configured device port BAR index");
                     end if;
                  elsif Res.Kind = Musinfo.Res_Device_Memory then
                     if Res.Name = Sinfo.Utils.Create_Name (Str => "manual0_mmio")
                       or else Res.Name = Sinfo.Utils.Create_Name
                         (Str => "manual0_mmconf")
                     then
                        Assert (Condition => Res.Dev_Mem_Data.BAR_Config.BAR_Idx
                                  = Musinfo.No_BAR_Config,
                                Message   => "Manual device memory BAR index");
                        Found := True;
                     end if;
                  end if;
               end;
            end loop;
         end if;
         Assert (Condition => Found,
                 Message   => "No manual device resource found");
         Ada.Directories.Delete_File (Name => "obj/lnx_sinfo");
      end Check_No_BAR_Config;
   begin
      Write_Sinfo (Arch => "x86_64");
      Write_Sinfo (Arch => "arm64");
      Check_No_BAR_Config;
--  begin read only
   end Test_Write;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Sinfo.Generator.Test_Data.Tests;
