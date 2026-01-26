with GIC;

with SK.Interrupts;

package Skp.Interrupts
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
     (CPU_Range, Physical_Interrupt_ID_Config)
     of Boolean;

   -----------------------------
   -- Interrupt Specification --
   -----------------------------
   subtype Interprocessor_Interrupts is
     Physical_Interrupt_ID_Config range 0 .. 15;

   Virtual_Maintenance_Interrupt : constant Physical_Interrupt_ID_Config
     :=__virtual_maintenance_irq__;
   Hypervisor_Timer_Interrupt    : constant Physical_Interrupt_ID_Config
     :=  26;
   SMMU_Controller_Interrupt     : constant Physical_Interrupt_ID_Config
     :=__pirq_id_smmu__;

   -----------------------------
   -- Interrupt Configuration --
   -----------------------------
   IRQ_Routing_Config : constant IRQ_Routing_Array
     := (__irq_routing_table__);

   --------------------------
   -- Interrupt Assignment --
   --------------------------
   Vector_Routing_Config : constant SK.Interrupts.Vector_Routing_Array
     (CPU_Range, Physical_Interrupt_ID_Config)
     := (__vector_routing_table__);

end Skp.Interrupts;
