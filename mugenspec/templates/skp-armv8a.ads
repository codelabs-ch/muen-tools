--D @Interface
--D The \texttt{Skp} package hierarchy is a codified static representation of
--D the system policy. All the values are derived from the system policy and
--D parameterize the Muen SK on the source level.
--D \paragraph*{}
--D This package contains numeric constants and range type definitions
--D derived from the system policy.

with ARMv8;

package SKP
  with
    SPARK_Mode => On
is

   use type ARMv8.CPU_ID_Type;

   CPU_Count : constant ARMv8.CPU_ID_Type := __cpu_count__;

   subtype CPU_Range is ARMv8.CPU_ID_Type range 0 .. CPU_Count - 1;

   subtype Global_Subject_ID_Type is Natural range __subj_range__;

   Invalid_Subject : constant := Global_Subject_ID_Type'Last + 1;

   subtype Dst_Subject_Type is Natural range 0 .. Invalid_Subject;

   type Vector_Range is range 0 .. 255;

   Invalid_Vector : constant := 256;

   type Dst_Vector_Range is range 0 .. Invalid_Vector;

end SKP;
