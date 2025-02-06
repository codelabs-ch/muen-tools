--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Expanders.Config
is

   --  Common x86_64/arm64 settings.

   Kernel_Text_Section_Addr         : constant := 16#0010_0000#;
   Kernel_Data_Section_Size         : constant := 16#0000_2000#;
   Kernel_Interrupt_Stack_Addr      : constant := 16#0012_9000#;
   Kernel_Interrupt_Stack_Size      : constant := 16#0000_1000#;
   Tau0_Interface_Virtual_Addr      : constant := 16#003f_f000#;
   Crash_Audit_Virtual_Addr         : constant := 16#0040_0000#;
   Kernel_Devices_Virtual_Addr      : constant := 16#0050_0000#;
   Subject_States_Virtual_Addr      : constant := 16#0060_0000#;
   Subject_Timed_Event_Virtual_Addr : constant := 16#0070_0000#;
   Subject_Interrupts_Virtual_Addr  : constant := 16#0080_0000#;
   Subject_MSR_Store_Virtual_Addr   : constant := 16#0090_0000#;
   Subject_VMCS_Virtual_Addr        : constant := 16#00a0_0000#;
   Scheduling_Info_Virtual_Addr     : constant := 16#00b0_0000#;
   Subject_FPU_States_Virtual_Addr  : constant := 16#00c0_0000#;
   Ucode_Virtual_Addr               : constant := 16#00d0_0000#;

   Subject_Sinfo_Region_Size        : constant := 16#8000#;
   Scheduling_Info_Region_Size      : constant := 16#1000#;

   Subject_Loader_Source_Base_Addr  : constant := 16#7000_0000_0000#;

   --  Per-arch specific settings.

   type Settings_Type is record
      Kernel_Text_Section_Size        : Interfaces.Unsigned_64;
      Kernel_Data_Section_Addr        : Interfaces.Unsigned_64;
      Kernel_BSS_Section_Addr         : Interfaces.Unsigned_64;
      Kernel_BSS_Section_Size         : Interfaces.Unsigned_64;
      Kernel_Stack_Addr               : Interfaces.Unsigned_64;
      Kernel_Stack_Size               : Interfaces.Unsigned_64;
      Kernel_Global_Data_Section_Addr : Interfaces.Unsigned_64;
      Kernel_Global_Data_Section_Size : Interfaces.Unsigned_64;
      Kernel_RO_Section_Addr          : Interfaces.Unsigned_64;
      Kernel_RO_Section_Size          : Interfaces.Unsigned_64;
   end record;

   Arch_Specific : constant array (Expanders.Arch_Type) of Settings_Type :=
     (Arm64 =>
        (Kernel_Text_Section_Size        => 16#0007_0000#,
         Kernel_Data_Section_Addr        => 16#0018_2000#,
         Kernel_BSS_Section_Addr         => 16#0018_4000#,
         Kernel_BSS_Section_Size         => 16#0000_2000#,
         Kernel_Stack_Addr               => 16#001a_0000#,
         Kernel_Stack_Size               => 16#0002_0000#,
         Kernel_Global_Data_Section_Addr => 16#0018_6000#,
         Kernel_Global_Data_Section_Size => 16#0000_a000#,
         Kernel_RO_Section_Addr          => 16#0017_0000#,
         Kernel_RO_Section_Size          => 16#0001_2000#),
      X86_64 =>
        (Kernel_Text_Section_Size        => 16#0001_1000#,
         Kernel_Data_Section_Addr        => 16#0012_0000#,
         Kernel_BSS_Section_Addr         => 16#0012_2000#,
         Kernel_BSS_Section_Size         => 16#0000_4000#,
         Kernel_Stack_Addr               => 16#0012_7000#,
         Kernel_Stack_Size               => 16#0000_1000#,
         Kernel_Global_Data_Section_Addr => 16#0012_b000#,
         Kernel_Global_Data_Section_Size => 16#0000_1000#,
         Kernel_RO_Section_Addr          => 16#0012_f000#,
         Kernel_RO_Section_Size          => 16#002c_0000#));

end Expanders.Config;
