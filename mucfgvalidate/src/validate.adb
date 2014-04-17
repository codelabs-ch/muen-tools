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

with Mulog;
with Muxml;
with Mucfgcheck.Memory;
with Mucfgcheck.MSR;
with Mucfgcheck.Device;
with Mucfgcheck.Scheduling;
with Mucfgcheck.Kernel;
with Mucfgcheck.Subject;
with Mucfgcheck.Platform;
with Mucfgcheck.Events;

with Validate.XML_Processors;
with Validate.Command_Line;

package body Validate
is

   --  Register policy validators.
   procedure Register_All;

   -------------------------------------------------------------------------

   procedure Register_All
   is
      use Mucfgcheck;
   begin
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Memory_References'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_In_Lowmem'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_In_Lowmem'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Virtual_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.Entity_Name_Encoding'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Memory.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Stack_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Store_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Consecutiveness'Access);
      XML_Processors.Register
        (Process => MSR.Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => MSR.Low_Or_High'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_References'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_IRQ_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_References'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => Device.Device_IO_Port_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_References'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Range_Equality'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_References'Access);
      XML_Processors.Register
        (Process => Device.Device_Sharing'Access);
      XML_Processors.Register
        (Process => Scheduling.CPU_Element_Count'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_References'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_CPU_Affinity'Access);
      XML_Processors.Register
        (Process => Scheduling.Major_Frame_Ticks'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Store_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Stack_Address_Equality'Access);
      XML_Processors.Register
        (Process => Subject.Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.CPU_ID'Access);
      XML_Processors.Register
        (Process => Events.Source_Targets'Access);
      XML_Processors.Register
        (Process => Events.Subject_Event_References'Access);
      XML_Processors.Register
        (Process => Events.Self_References'Access);
      XML_Processors.Register
        (Process => Events.Switch_Same_Core'Access);
      XML_Processors.Register
        (Process => Events.IPI_Different_Core'Access);
      XML_Processors.Register
        (Process => Platform.Memory_Space'Access);
      XML_Processors.Register
        (Process => Platform.Memory_Block_Overlap'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run
   is
      Data        : Muxml.XML_Data_Type;
      Policy_File : constant String := Command_Line.Get_Policy;
   begin
      Mulog.Log (Msg => "Validating policy '" & Policy_File & "'");

      Register_All;
      Mulog.Log
        (Msg => "Registered validators" & XML_Processors.Get_Count'Img);

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy_File);
      XML_Processors.Run (Data => Data);

      Mulog.Log (Msg => "Successfully validated policy '" & Policy_File & "'");
   end Run;

end Validate;
