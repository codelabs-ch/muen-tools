with System;

with GIC;

with SK.Interrupts;

package SKP.Interrupts
  with
    SPARK_Mode => On
is

   ----------------
   -- Base Types --
   ----------------
   subtype Physical_Interrupt_ID_Config is
     GIC.Interrupt_ID_Type range 0 ..__pirq_id_max__;

   subtype Virtual_Interrupt_ID_Config is
     GIC.Interrupt_ID_Type range 0 ..__virq_id_max__;

   type IRQ_Routing_Array is array
     (CPU_ID_Config, Physical_Interrupt_ID_Config)
     of Boolean;

   -----------------------------
   -- Interrupt Specification --
   -----------------------------
   subtype Interprocessor_Interrupts is
     Physical_Interrupt_ID_Config range 0 .. 15;

   Virtual_Maintenance_Interrupt : constant Physical_Interrupt_ID_Config
     :=  25;
   Hypervisor_Timer_Interrupt    : constant Physical_Interrupt_ID_Config
     :=  26;
   SMMU_Controller_Interrupt     : constant Physical_Interrupt_ID_Config
     :=__pirq_id_smmu__;

   ------------------------------
   -- Controller Specification --
   ------------------------------
   GIC_Instance_Base_Address : constant System.Address
     := System'To_Address (__gic_base_address__);

   -----------------------------
   -- Interrupt Configuration --
   -----------------------------
   IRQ_Routing_Config : constant IRQ_Routing_Array
     := (__irq_routing_table__)
     with
       Linker_Section => ".globalrodata";

   --------------------------
   -- Interrupt Assignment --
   --------------------------
   Vector_Routing_Config : constant SK.Interrupts.Vector_Routing_Array
     (CPU_ID_Config, Physical_Interrupt_ID_Config)
     := (__vector_routing_table__)
     with
       Linker_Section => ".globalrodata";

end SKP.Interrupts;
