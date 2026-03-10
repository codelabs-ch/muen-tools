with SK;

with ARMv8.Register;
with ARMv8.Cortex_A53.Hypervisor;

--D @Interface
--D This package contains subject specifications as defined by the system
--D policy. The given values define the configurable part of the subject
--D state for each subject and establish their initial state according to
--D the policy.
package Skp.Subjects
  with
    SPARK_Mode => On
is

   function Get_CPU_ID
     (Subject_ID : Global_Subject_ID_Type)
      return CPU_Range;

   function Get_General_Purpose_Registers
     (Subject_ID : Global_Subject_ID_Type)
      return ARMv8.Register.General_Purpose_Register_X_Type;

   function Get_Exception_Link_Register_EL2
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_64_Type;

   function Get_Hypervisor_Configuration_Register_EL2
     (Subject_ID : Global_Subject_ID_Type)
      return
      ARMv8.Cortex_A53.Hypervisor.Hypervisor_Configuration_Register_EL2_Type;

   function Get_VTTBR_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_48_Type;

   function Accepts_Interrupts
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean;

end Skp.Subjects;
