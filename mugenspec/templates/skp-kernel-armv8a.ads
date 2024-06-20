--D @Interface
--D This package contains virtual addresses of various kernel data structure
--D mappings as specified by the system policy.
package SKP.Kernel
  with
    SPARK_Mode => On
is

   Tau0_Iface_Address        : constant := __tau0_iface_addr__;
   Subj_States_Address       : constant := __subj_states_addr__;
   Subj_Timed_Events_Address : constant := __subj_timed_events_addr__;
   Scheduling_Info_Address   : constant := __sched_info_addr__;
   Crash_Audit_Address       : constant := __crash_audit_addr__;
   Crash_Audit_Size          : constant := __crash_audit_size__;

end SKP.Kernel;
