with SK;

--# inherit SK, Skp;
package Skp.Interrupts is

   subtype Remapped_Vector_Type is SK.Byte range 32 .. 255;

   type IRQ_Route_Type is record
      CPU    : Skp.CPU_Range;
      IRQ    : SK.Byte;
      Vector : Remapped_Vector_Type;
   end record;

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (CPU    => 0,
      IRQ    => 0,
      Vector => 255);

   type Routing_Range is range __routing_range__;

   type IRQ_Routing_Array is array (Routing_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

   type Vector_Routing_Array is array (Remapped_Vector_Type)
     of Skp.Dst_Subject_Type;

   Vector_Routing : constant Vector_Routing_Array := Vector_Routing_Array'(
__vector_routing_table__);

end Skp.Interrupts;
