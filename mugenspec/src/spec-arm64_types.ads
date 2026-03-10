--
--  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Spec.ARM64_Types
is

   --  Hypervisor Control Register fields. Must be kept in sync with XML
   --  hcrType.
   type HCR_EL2_Field_Type is
     (vm,
      swio,
      ptw,
      fmo,
      imo,
      amo,
      vf,
      vi,
      vse,
      fb,
      bsu,
      dc,
      twi,
      twe,
      tid0,
      tid1,
      tid2,
      tid3,
      tsc,
      tidcp,
      tacr,
      tsw,
      tpc,
      tpu,
      ttlb,
      tvm,
      tge,
      tdz,
      hcd,
      trvm,
      rw,
      cd,
      id);

   subtype HCR_Field_Name_String is String (1 .. 31);

   type HCR_XML_Tag_To_Record_Field_Type is array (HCR_EL2_Field_Type)
     of HCR_Field_Name_String;

   --  Mapping of XML tag to Ada record field name, see
   --  ARMv8.Cortex_A53.Hypervisor.Hypervisor_Configuration_Register_EL2_Type.
   To_Field_Name : constant HCR_XML_Tag_To_Record_Field_Type
     := (vm    => "Virtualization                 ",
         swio  => "SW_Invalidation                ",
         ptw   => "Protected_Table_Walk           ",
         fmo   => "Physical_FIQ                   ",
         imo   => "Physical_IRQ                   ",
         amo   => "Physical_SError                ",
         vf    => "Virtual_FIQ                    ",
         vi    => "Virtual_IRQ                    ",
         vse   => "Virtual_SError                 ",
         fb    => "Force_Broadcast                ",
         bsu   => "Barrier_Shareability           ",
         dc    => "Default_Cacheability           ",
         twi   => "Trap_WFI_Instruction           ",
         twe   => "Trap_WFE_Instruction           ",
         tid0  => "Trap_ID_Group_0_Register       ",
         tid1  => "Trap_ID_Group_1_Register       ",
         tid2  => "Trap_ID_Group_2_Register       ",
         tid3  => "Trap_ID_Group_3_Register       ",
         tsc   => "Trap_SMC_Instruction           ",
         tidcp => "Trap_IMPL_DEF_Instruction      ",
         tacr  => "Trap_ACR_Register              ",
         tsw   => "Trap_DC_SW_Instruction         ",
         tpc   => "Trap_DC_AC_Instruction         ",
         tpu   => "Trap_DC_AU_Instruction         ",
         ttlb  => "Trap_TLB_Instruction           ",
         tvm   => "Trap_Virtual_Memory_Register   ",
         tge   => "Trap_All_EL0_Exceptions        ",
         tdz   => "Trap_DC_ZVA_Instruction        ",
         hcd   => "HVC_Instruction                ",
         trvm  => "Trap_Virtual_Memory_Instruction",
         rw    => "Lower_Level_Execution_State    ",
         cd    => "SLAT_Data_Cacheability         ",
         id    => "SLAT_Instruction_Cacheability  ");

end Spec.ARM64_Types;
