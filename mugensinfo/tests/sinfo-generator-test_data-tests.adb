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
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Musinfo;
with Muxml.Utils;
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

      --  Write the sinfo of the given policy and deserialize it from
      --  obj/lnx_sinfo.
      function Get_Subject_Info
        (Policy : Muxml.XML_Data_Type)
         return Musinfo.Subject_Info_Type
      is
         subtype Sinfo_Stream is Ada.Streams.Stream_Element_Array
           (1 .. Musinfo.Subject_Info_Type_Size);
         function Convert is new Ada.Unchecked_Conversion
           (Source => Sinfo_Stream,
            Target => Musinfo.Subject_Info_Type);

         File   : Ada.Streams.Stream_IO.File_Type;
         Stream : Sinfo_Stream;
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         Write (Output_Dir => "obj",
                Policy     => Policy);
         Ada.Streams.Stream_IO.Open
           (File => File,
            Mode => Ada.Streams.Stream_IO.In_File,
            Name => "obj/lnx_sinfo");
         Ada.Streams.Stream_IO.Read (File => File, Item => Stream, Last => Last);
         Ada.Streams.Stream_IO.Close (File => File);
         return Convert (S => Stream);
      end Get_Subject_Info;

      ----------------------------------------------------------------------

      --  Verify that the I/O port resource of a device with an explicitly
      --  empty <bars/> element is encoded with the No_BAR_Config marker.
      procedure Check_No_BAR_Config
      is
         use type Musinfo.Name_Type;
         use type Musinfo.Resource_Kind;

         Policy : Muxml.XML_Data_Type;
         Info   : Musinfo.Subject_Info_Type;
         Port_Log_Name : constant String := "manual0_port1";
         Port_Name : constant Musinfo.Name_Type
           := Sinfo.Utils.Create_Name (Str => Port_Log_Name);
         Found : Boolean := False;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy_x86_64.xml");
         Info := Get_Subject_Info (Policy => Policy);

         for I in 1 .. Natural (Info.Resource_Count) loop
            declare
               Res : constant Musinfo.Resource_Type
                 := Info.Resources (Musinfo.Resource_Index_Type (I));
            begin
               if Res.Kind = Musinfo.Res_Device_IO_Port
                 and then Res.Name = Port_Name
               then
                  Assert (Condition => Res.Dev_IO_Port_Data.BAR_Idx
                            = Musinfo.No_BAR_Config,
                          Message   => "Unexpected manual device port BAR "
                          & "index: should be No_BAR_Config");
                  Found := True;
               end if;
            end;
         end loop;
         Assert (Condition => Found,
                 Message   => "Manual device port resource not found");
         Ada.Directories.Delete_File (Name => "obj/lnx_sinfo");
      end Check_No_BAR_Config;

      ----------------------------------------------------------------------

      --  Verify that a device with FLR support but without mmconf mapping
      --  is not announced with a reset method.
      procedure Check_Reset_Method
      is
         use type Musinfo.Dev_Reset_Method_Type;
         use type Musinfo.Name_Type;
         use type Musinfo.Resource_Kind;

         Policy       : Muxml.XML_Data_Type;
         Info         : Musinfo.Subject_Info_Type;
         Dev_Log_Name : constant String := "eth0";
         Dev_Name     : constant Musinfo.Name_Type
           := Sinfo.Utils.Create_Name (Str => Dev_Log_Name);
         Found        : Boolean := False;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy_x86_64.xml");
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='lnx']/devices/"
            & "device[@logical='" & Dev_Log_Name
            & "']/memory[@logical='mmconf']");
         Info := Get_Subject_Info (Policy => Policy);

         for I in 1 .. Natural (Info.Resource_Count) loop
            declare
               Res : constant Musinfo.Resource_Type
                 := Info.Resources (Musinfo.Resource_Index_Type (I));
            begin
               if Res.Kind = Musinfo.Res_Device
                 and then Res.Name = Dev_Name
               then
                  Assert (Condition => Res.Dev_Data.Reset_Method
                            = Musinfo.Dev_Reset_Method_None,
                          Message   => "Unexpected reset method for device "
                          & "without mmconf: "
                          & Res.Dev_Data.Reset_Method'Img);
                  Found := True;
               end if;
            end;
         end loop;
         Assert (Condition => Found,
                 Message   => "FLR device without mmconf not found");
         Ada.Directories.Delete_File (Name => "obj/lnx_sinfo");
      end Check_Reset_Method;
   begin
      Write_Sinfo (Arch => "x86_64");
      Write_Sinfo (Arch => "arm64");
      Check_No_BAR_Config;
      Check_Reset_Method;
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
