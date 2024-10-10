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

with Muxml;
with Mulog;
with Mucfgcheck.Config;
with Mucfgcheck.Memory;
with Mucfgcheck.MSR;
with Mucfgcheck.Device;
with Mucfgcheck.Scheduling;
with Mucfgcheck.Kernel;
with Mucfgcheck.Subject;
with Mucfgcheck.Hardware;
with Mucfgcheck.Platform;
with Mucfgcheck.Events;
with Mucfgcheck.Device_Domains;
with Mucfgcheck.Validation_Errors;

with Mutools.System_Config;

with Validate.XML_Processors;

package body Validate
is

   -------------------------------------------------------------------------

   procedure Register_ARMv8a
   is
      use Mucfgcheck;
   begin
      --  NOTE: Comments can be removed in future, for documentation
      --  purposes and as foundation for discussion only.

      --  Arch Independent: Validate that physical memory referenced by
      --  logical memory exists.
      XML_Processors.Register
        (Process => Memory.Physical_Memory_References'Access);

      --  Arch independent: Validate that physical devices referenced by
      --  logical devices exist.
      XML_Processors.Register
        (Process => Device.Physical_Device_References'Access);

      --  Partially Arch Dependent: ARMv8-A does not officially distinguish
      --  between PCI and legacy devices, therefore the second registered
      --  check currently should never find any such devices
      XML_Processors.Register
        (Process => Device.Legacy_Device_References'Access);
      XML_Processors.Register
        (Process => Device.PCI_Device_References'Access);

      --  Arch Independent: Validate that physical device IRQs referenced
      --  by logical IRQs exist.
      XML_Processors.Register
        (Process => Device.Physical_IRQ_References'Access);

      --  Arch Dependent: The concept of I/O ports does not exist on ARMv8-A
      --  platforms - the corresponding x86/64 check can be removed.

      --  Arch Independent: Validate that device memory referenced by
      --  logical device memory exists.
      XML_Processors.Register
        (Process => Device.Device_Memory_References'Access);

      --  Arch Independent: Validate that subjects referenced by scheduling
      --  groups exist.
      XML_Processors.Register
        (Process => Scheduling.Subject_References'Access);

      --  Arch Independent: Validate that events referenced by subjects
      --  exist.
      XML_Processors.Register
        (Process => Events.Subject_Event_References'Access);

      --  Arch Independent: Validate config variable name uniqueness.
      XML_Processors.Register
        (Process => Config.Name_Uniqueness'Access);

      --  Arch Independent: Validate that physical memory region names
      --  are unique.
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Name_Uniqueness'Access);

      --  Arch Dependent: VMXON and VMCS do not exist for ARMv8-A cores,
      --  but the similarly used hypervisor registers (e.g. HCR_EL2) as
      --  stored in the respective subject's state has to be validated in
      --  future. Checks: 4K alignment translation tables, currently AArch64
      --  only, ev. start addresses in virtual memory, etc.

      --  Arch Independent: Validate that all physical / virtual memory
      --  addresses are page aligned and that all memory region sizes are
      --  multiples of page size.
      XML_Processors.Register
        (Process => Memory.Physical_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Virtual_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Region_Size'Access);

      --  Partially Arch Dependent: Validate kernel or subject entities
      --  encoded in physical memory names (e.g. kernel_0|vmxon, linux|vmcs
      --  etc. not for ARMv8-A and linux|dtb not for x86/64).
      XML_Processors.Register
        (Process => Memory.Entity_Name_Encoding'Access);

      --  Arch Independent: Validate that no physical memory regions overlap.
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Overlap'Access);

      --  Arch Independent: Validate that an uncached crash audit region
      --  is present.
      XML_Processors.Register
        (Process => Memory.Uncached_Crash_Audit_Presence'Access);

      --  Arch Independent: Validate that no virtual memory regions of
      --  the kernel / subjects overlap.
      XML_Processors.Register
        (Process => Kernel.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Subject.Virtual_Memory_Overlap'Access);

      --  Partially Arch Dependent: Validate that the kernel binary regions
      --  exist for every CPU (ARMv8-A does not require an interrupt stack).
      XML_Processors.Register
        (Process => Memory.Kernel_Data_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_BSS_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Stack_Region_Presence'Access);

      --  Arch Independent: Validate that a kernel PT region exists for
      --  every CPU.
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Region_Presence'Access);

      --  Arch Dependent?: Validate that kernel PT regions are in the
      --  first 4G (May be an x86/64 hardware requirement? On ARMv8-A
      --  not required from a hardware perspective for the currently used
      --  platform Xilinx MPSoC ZCU102/4).

      --  Arch Independent: Validate that a subject state memory region
      --  with the expected size exists for every subject (not yet
      --  supported on ARMv8-A, currently manually written Skp files).
      XML_Processors.Register
        (Process => Memory.Subject_State_Region_Presence'Access);

      --  Arch Independent: Validate that a subject timed event memory
      --  region with the expected size exists for every subject.
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Region_Presence'Access);

      --  Arch Dependent: Interrupt memory regions are not required for
      --  ARMv8-A due to the usage of the virtualisation extension of
      --  the Generic Interrupt Controller GIC. In future, these regions
      --  could be used to provide a paravirtualised Distributor to the
      --  subject managed by a Subject Monitor.

      --  Arch Dependent: I/O bitmap, MSR bitmap and store regions not
      --  required for ARMv8-A (probably similar register memory regions
      --  for PMU etc. functionality in future).

      --  Arch Independent: Validate that a subject FPU state memory region
      --  with the expected size exists for every subject. (not yet
      --  supported on ARMv8-A, currently manually written Skp files).
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Region_Presence'Access);

      --  Arch Independent: Validate that a subject pagetable memory region
      --  exists for each subject.
      XML_Processors.Register
        (Process => Memory.Subject_PT_Region_Presence'Access);

      --  Arch Independent: Validate that a scheduling info memory region
      --  exists for each scheduling partition.
      XML_Processors.Register
        (Process => Memory.Scheduling_Info_Region_Presence'Access);

      --  Arch Independent: Validate that memory of type kernel is only
      --  mapped by kernel or Tau0. ATTENTION - Exception during execution
      --  due to compatibility nodes for mugenspec, enable after mugenspec
      --  completely supported for ARMv8-A!
      --  XML_Processors.Register
      --    (Process => Memory.Kernel_Memory_Mappings'Access);

      --  Arch Independent: Validate that memory of type system is not
      --  mapped by any entity.
      XML_Processors.Register
        (Process => Memory.System_Memory_Mappings'Access);

      --  Arch Independent: Validate that memory of type 'device' (e.g.
      --  device\_rmrr) is only mapped by device domains. The device
      --  domain concept seems to be mappable to the GICv2 architecture
      --  (c.f. SMMU / IOMMU, below).
      XML_Processors.Register
        (Process => Memory.Device_Memory_Mappings'Access);

      --  Arch Independent: Validate that subject state memory regions are
      --  mapped by the kernel running that subject. Also verify that the
      --  kernel mapping is at the expected virtual location. (not yet
      --  supported on ARMv8-A, currently manually written Skp files).
      XML_Processors.Register
        (Process => Memory.Subject_State_Mappings'Access);

      --  Arch Dependent: Interrupt memory regions are not required for
      --  ARMv8-A due to the usage of the virtualisation extension of
      --  the Generic Interrupt Controller GIC.

      --  Arch Independent: Validate that subject timed event memory regions
      --  are mapped by the kernel running that subject. Also verify that the
      --  kernel mapping is at the expected virtual location.
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Mappings'Access);

      --  Arch Dependent: VMCS and MSR not required on ARMv8-A (c.f. above).

      --  Arch Independent: Validate that subject FPU state regions are
      --  mapped by the kernel running that subject. Also verify that the
      --  kernel mapping is at the expected virtual location. (not yet
      --  supported on ARMv8-A, currently manually written Skp files).
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Mappings'Access);

      --  Arch Independent: Validate that subjects map the scheduling info
      --  region of their associated scheduling partition and validate that
      --  scheduling info regions are mapped by the kernel running subjects
      --  of that scheduling partition. Also verify that the kernel mapping
      --  is at the expected virtual location.
      XML_Processors.Register
        (Process => Memory.Subject_Sched_Info_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Sched_Info_Mappings'Access);

      --  Arch Dependent: Validate that the Linux kernel image's base address is
      --  properly aligned for ARMv8-A (2 MB).
      XML_Processors.Register
        (Process => Memory.Subject_Linux_Image_Alignment'Access);

      --  Partially Arch Dependent: Validate that subject state, timed event
      --  and pending interrupts memory regions are only mapped writable by
      --  subjects in the same scheduling group or by siblings. For ARMv8-A,
      --  the pending interrupts memory regions are not required (c.f. above).
      XML_Processors.Register
        (Process => Memory.Monitor_Subject_Region_Mappings'Access);

      --  Arch Dependent: MSR not required on ARMv8-A (c.f. above).

      --  Arch Independent: Validate that device names (including device
      --  aliases / classes) are unique.
      XML_Processors.Register
        (Process => Device.Physical_Device_Name_Uniqueness'Access);

      --  Partially Arch Dependent: Validate that all physical IRQs are
      --  unique. This is true for device related IRQs (i.e. SPI on ARMv8-A)
      --  as validated by this procedure - but on ARMv8-A the SGI and PPI
      --  interrupts have to be only unique per CPU! Therefore, this check
      --  has to be adapted for ARMv8-A / GICv2.

      --  Arch Independent: Validate that physical IRQ names are unique
      --  per device.
      XML_Processors.Register
        (Process => Device.Device_IRQ_Name_Uniqueness'Access);

      --  Arch Dependent: All IRQ types and classes depend on the architecture
      --  and in the case of ARMv8-A even on the Generic Interrupt Controller
      --  implementation / architecture (c.f. GICv2 vs. GICv3 etc.)

      --  Arch Dependent: The concept of I/O ports does not exist on ARMv8-A
      --  platforms - the corresponding x86/64 check can be removed.

      --  Arch Independent: Validate that device memory names are unique
      --  per device.
      XML_Processors.Register
        (Process => Device.Device_Memory_Name_Uniqueness'Access);

      --  Arch Independent: Validate that logical device references of
      --  each subject do not refer to the same physical device.
      XML_Processors.Register
        (Process => Device.Device_Reference_Uniqueness'Access);

      --  Arch Independent?: Although ARMv8-A does not officially distinguish
      --  between PCI and legacy devices, the policy abstractions for PCI
      --  devices should be similar on ARMv8-A (Except for IRQs? Maybe also
      --  for ARMv8-A AMBA/AXI integration vs. PCI Southbridge?).
      XML_Processors.Register
        (Process => Device.PCI_Device_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.PCI_Multifunction_Device_Refs'Access);
      XML_Processors.Register
        (Process => Device.Device_Reference_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_References_PCI_Bus_Number'Access);

      --  Arch Independent: Validate that each major frame specifies the
      --  same number of CPUs.
      XML_Processors.Register
        (Process => Scheduling.CPU_Element_Count'Access);

      --  Arch Independent:  Validate that scheduling partition IDs
      --  are unique.
      XML_Processors.Register
        (Process => Scheduling.Partition_ID'Access);

      --  Arch Independent: Validate that scheduling group IDs are unique.
      XML_Processors.Register
        (Process => Scheduling.Group_ID'Access);

      --  Arch Independent: Validate that scheduling partitions are
      --  scheduled in at least one minor frame and that all minor frame
      --  references are on the same logical CPU.
      XML_Processors.Register
        (Process => Scheduling.Partition_CPU_Affinity'Access);

      --  Arch Independent: Validate that subjects are scheduled on the
      --  correct logical CPU.
      XML_Processors.Register
        (Process => Scheduling.Subject_CPU_Affinity'Access);

      --  Arch Independent: Validate that subjects are part of exactly
      --  one scheduling group.
      XML_Processors.Register
        (Process => Scheduling.Subject_Scheduling_Group_Assignment'Access);

      --  Arch Independent: Validate that all subjects of a scheduling
      --  group are runnable.
      XML_Processors.Register
        (Process => Scheduling.Subject_Scheduling_Group_Runnability'Access);

      --  Arch Independent: Validate tick counts in major frame.
      XML_Processors.Register
        (Process => Scheduling.Major_Frame_Ticks'Access);

      --  Arch Independent: Validate that barrier IDs do not exceed barrier
      --  count and are unique.
      XML_Processors.Register
        (Process => Scheduling.Barrier_ID'Access);

      --  Arch Independent: Validate that barrier sizes do not exceed
      --  the number of logical CPUs.
      XML_Processors.Register
        (Process => Scheduling.Barrier_Size'Access);

      --  Arch Independent: Validate that the barrier sizes and count of a
      --  major frame corresponds to the minor frame synchronization points.
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Sync_Points'Access);

      --  Arch Independent: Validate that minor frame barrier references
      --  are valid.
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Barrier_Refs'Access);

      --  Arch Independent: Validate partition references in minor frames.
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Partition_References'Access);

      --  Arch Independent: Validate that all CPU-local data section
      --  virtual addresses are equal. On ARMv8-A, separate data section
      --  mapping per CPU is not yet supported.
      XML_Processors.Register
        (Process => Kernel.CPU_Local_Data_Address_Equality'Access);

      --  Arch Independent: Validate that all CPU-local BSS section virtual
      --  addresses are equal. On ARMv8-A, separate BSS mapping per CPU is
      --  not yet supported.
      XML_Processors.Register
        (Process => Kernel.CPU_Local_BSS_Address_Equality'Access);

      --  Arch Independent: Validate that all global data section virtual
      --  addresses are equal and that the expected number of mappings
      --  exists.
      XML_Processors.Register
        (Process => Kernel.Global_Data_Address_Equality'Access);

      --  Arch Independent: Validate that all crash audit mappings exist
      --  and that their virtual addresses are equal.
      XML_Processors.Register
        (Process => Kernel.Crash_Audit_Address_Equality'Access);

      --  Arch Independent: Validate that all stack virtual addresses
      --  are equal.
      XML_Processors.Register
        (Process => Kernel.Stack_Address_Equality'Access);

      --  Arch Dedependent: ARMv8-A does not require an interrupt stack
      --  and therefore the stack layout is different. Checks have to
      --  be defined after layout discussion for ARM.

      --  Arch Independent: Validate that each active CPU has a memory
      --  section.
      XML_Processors.Register
        (Process => Kernel.CPU_Memory_Section_Count'Access);

      --  Arch Dedependent: The ARMv8-A PMU is not yet supported and
      --  is completey different from an architectural point of view.
      --  These checks have to be added after PMU integration.

      --  Arch Independent: Validate that the debug console device and
      --  its resources matches the kernel diagnostics device specified
      --  in the platform section.
      XML_Processors.Register
        (Process => Kernel.Diagnostics_Device_Reference'Access);

      --  Arch Independent: Validate subject name uniqueness.
      XML_Processors.Register
        (Process => Subject.Name_Uniqueness'Access);

      --  Arch Independent: Validate subject CPU ID.
      XML_Processors.Register
        (Process => Subject.CPU_ID'Access);

      --  Arch Independent: Validate uniqueness of global subject IDs.
      XML_Processors.Register
        (Process => Subject.Global_ID_Uniqueness'Access);

      --  Arch Independent:  Validate per-CPU uniqueness of local subject IDs.
      XML_Processors.Register
        (Process => Subject.Local_ID_Uniqueness'Access);

      --  Arch Independent: Validate memory types of memory mappings
      --  (ie. allow access by subjects).
      XML_Processors.Register
        (Process => Subject.Memory_Types'Access);

      --  Arch Independent: Validate that all subjects are runnable, i.e.
      --  referenced in a scheduling group.
      XML_Processors.Register
        (Process => Subject.Runnability'Access);

      --  Arch Independent: Validate that subject scheduling group IDs
      --  match values as determined by the scheduling plan and handover
      --  events.
      XML_Processors.Register
        (Process => Subject.Scheduling_Group_IDs'Access);

      --  Arch Independent: Validate that logical names of subject memory
      --  regions are unique.
      XML_Processors.Register
        (Process => Subject.Logical_Memory_Name_Uniqueness'Access);

      --  Arch Independent: Validate that logical names of subject
      --  devices are unique.
      XML_Processors.Register
        (Process => Subject.Logical_Device_Name_Uniqueness'Access);

      --  Arch Independent?: Although ARMv8-A does not officially distinguish
      --  between PCI and legacy devices, the policy abstractions for PCI
      --  devices should be similar on ARMv8-A (Except for IRQs? Maybe also
      --  for ARMv8-A AMBA/AXI integration vs. PCI Southbridge?).
      XML_Processors.Register
        (Process => Subject.Logical_IRQ_MSI_Consecutiveness'Access);

      --  Arch Dependent: GICv2 has hardware virtualisation support for
      --  IRQs and a different acknowledge process compared to x86/64. Hence,
      --  the 'unmask' event is actually not required (but supported) for
      --  GIC-400 platforms.
      XML_Processors.Register
        (Process => Subject.Logical_Unmask_Event'Access);

      --  Arch Independent: Validate that multiple initramfs regions are
      --  consecutive. Should work on ARMv8-A the same way as tested with
      --  sinfo etc. integration.
      XML_Processors.Register
        (Process => Subject.Initramfs_Consecutiveness'Access);

      --  Arch Independent: Validate that no subject has write access to
      --  the crash audit region.
      XML_Processors.Register
        (Process => Subject.Crash_Audit_Write_Access'Access);

      --  Arch Dependent?: In an ARMv8-A SoC, PCI devices are always
      --  memory mapped. Therefore 'Device_Mmconf_Mappings' seems to
      --  be unnecessary for ARM.

      --  Arch Dependent: All VMX controls etc. are x86/64 specific.
      --  For ARMv8-A similar checks have to be added (c.f. above,
      --  HCR_EL2 etc.).

      --  Arch Independent: Check that all physical event names are unique.
      XML_Processors.Register
        (Process => Events.Physical_Event_Name_Uniqueness'Access);

      --  Arch Independent:Check that each global event has associated
      --  sources and one target.
      XML_Processors.Register
        (Process => Events.Source_Targets'Access);

      --  Arch Independent: Validate that there are no self-references in
      --  subject's event notification entries.
      XML_Processors.Register
        (Process => Events.Self_References'Access);

      --  Arch Independent: Validate that notification entries switch to a
      --  subject running on the same core and in the same scheduling group.
      XML_Processors.Register
        (Process => Events.Switch_Same_Core'Access);

      --  Arch Independent: Validate that target subjects of IPI
      --  notification entries run on different logical CPUs.
      XML_Processors.Register
        (Process => Events.IPI_Different_Core'Access);

      --  Arch Independent: Validate that target event IDs as well as
      --  logical names are unique.
      XML_Processors.Register
        (Process => Events.Target_Event_ID_Name_Uniqueness'Access);

      --  Arch Independent: Check source event ID validity.
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Validity'Access);

      --  Arch Independent: Validate that source event IDs as well as
      --  logical names are unique per group.
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Name_Uniqueness'Access);

      --  Arch Independent: Check that source event IDs of the VMX Exit
      --  group are all given or a default is specified.
      XML_Processors.Register
        (Process => Events.Source_VMX_Exit_Event_Completeness'Access);

      --  Arch Independent: Check that self events provide a target action.
      XML_Processors.Register
        (Process => Events.Self_Event_Action'Access);

      --  Arch Independent: Check that kernel-mode events have an action
      --  specified. Currently, this check will always trigger for ARMv8-A
      --  due to the 'magic_number' event patched into the kernel config
      --  during the build process of a demo system. The 'magic_number'
      --  mechanism is going to be removed in the Demo Update task.
      XML_Processors.Register
        (Process => Events.Kernel_Mode_Event_Actions'Access);

      --  Arch Independent: Check that system-related actions are only
      --  used with kernel-mode events.
      XML_Processors.Register
        (Process => Events.Kernel_Mode_System_Actions'Access);

      --  Arch Dependent: Check that level-triggered IRQs have a
      --  corresponding unmask IRQ event. This is not required for
      --  GICv2 based PIC due to the acknowledgment process.

      --  Arch Independent: Validate that memory regions fit into
      --  available hardware memory.
      XML_Processors.Register
        (Process => Hardware.Memory_Space'Access);

      --  Arch Independent: Validate that no memory blocks overlap.
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Overlap'Access);

      --  Arch Independent: Validate that the size of memory blocks is
      --  a multiple of page size.
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Size'Access);

      --  Arch Dependent?: Validate that PCI config space address and
      --  size are specified if PCI devices are present. PCI devices
      --  are always memory mapped (c.f. above).

      --  Arch Independent: Validate that the hardware provides enough
      --  physical CPU cores.
      XML_Processors.Register
        (Process => Hardware.CPU_Count'Access);

      --  Arch Dependent: The ARMv8-A architecture does neither specify CPU
      --  sub-component concepts like APIC ID or x86/64 BSP (not to be
      --  confused with ARM BSP, which means Board Support Package, i.e. a
      --  collection of library functions to interact with a specific
      --  evaluation board / SoC) nor a system board device with IO Ports.

      --  Arch Dependent: The concept of I/O APIC does not exist on
      --  ARMv8-A based SoC.

      --  Arch Independent: Validate that the physical device and resources
      --  referenced by the kernel diagnostics device exists.
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Device_Reference'Access);

      --  Arch Independent: Validate that the kernel diagnostics device
      --  resources match the requirements of the specified diagnostics
      --  type.
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Type_Resources'Access);

      --  Arch Dependent: VT-d is x86/64 specific, but ARMv8-A has similar
      --  hardware virtualisation extension(s) to be tested (c.f. above).

      --  Arch Dependent: The IOMMU (i.c. SMMU for ARMv8-A SoC) depends
      --  on the SMMUv2 specification and therefore requires different
      --  checks like context bank restrictions, matching mechanism,
      --  stream to context register constraints etc.

      --  Partially Arch Dependent: Validate most aspects of device domains.
      --  Others could possibly be added (e.g. for SMMU hardware limitations
      --  like 16 Context Banks on Xilinx ZCU104).
      XML_Processors.Register
        (Process => Device_Domains.Device_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Type'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_PT_Region_Presence'Access);
   end Register_ARMv8a;

   -------------------------------------------------------------------------

   procedure Register_X86_64
   is
      use Mucfgcheck;
   begin

      --  Check references first, some of these are fatal if not correct.

      XML_Processors.Register
        (Process => Memory.Physical_Memory_References'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_References'Access);
      XML_Processors.Register
        (Process => Device.Legacy_Device_References'Access);
      XML_Processors.Register
        (Process => Device.PCI_Device_References'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_References'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_References'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_References'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_References'Access);
      XML_Processors.Register
        (Process => Events.Subject_Event_References'Access);

      XML_Processors.Register
        (Process => Config.Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Name_Uniqueness'Access);
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
        (Process => Memory.Uncached_Crash_Audit_Presence'Access);
      XML_Processors.Register
        (Process => Kernel.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Subject.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Data_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_BSS_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Stack_Region_Presence'Access);
      -- TODO: MOA: No interrupt stack.
      -- XML_Processors.Register
      --   (Process => Memory.Kernel_Intr_Stack_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Below_4G'Access);
      XML_Processors.Register
        (Process => Memory.Subject_State_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Interrupts_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_IOBM_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSRBM_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSR_Store_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Scheduling_Info_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.System_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Device_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_State_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Interrupts_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_VMCS_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSR_Store_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Sched_Info_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Sched_Info_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Monitor_Subject_Region_Mappings'Access);
      XML_Processors.Register
        (Process => MSR.Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => MSR.Check_Whitelist'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_IRQ_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_ISA'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_PCI_LSI'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_PCI_MSI'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_MSI_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => Device.Device_IO_Port_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.PCI_Device_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.PCI_Multifunction_Device_Refs'Access);
      XML_Processors.Register
        (Process => Device.Device_Reference_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_References_PCI_Bus_Number'Access);
      XML_Processors.Register
        (Process => Scheduling.CPU_Element_Count'Access);
      XML_Processors.Register
        (Process => Scheduling.Partition_ID'Access);
      XML_Processors.Register
        (Process => Scheduling.Group_ID'Access);
      XML_Processors.Register
        (Process => Scheduling.Partition_CPU_Affinity'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_CPU_Affinity'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_Scheduling_Group_Assignment'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_Scheduling_Group_Runnability'Access);
      XML_Processors.Register
        (Process => Scheduling.Major_Frame_Ticks'Access);
      XML_Processors.Register
        (Process => Scheduling.Barrier_ID'Access);
      XML_Processors.Register
        (Process => Scheduling.Barrier_Size'Access);
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Sync_Points'Access);
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Barrier_Refs'Access);
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Partition_References'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Local_Data_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Local_BSS_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Global_Data_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Crash_Audit_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Stack_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Stack_Layout'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Memory_Section_Count'Access);
      XML_Processors.Register
        (Process => Kernel.System_Board_Reference'Access);
      XML_Processors.Register
        (Process => Kernel.Diagnostics_Device_Reference'Access);
      XML_Processors.Register
        (Process => Subject.Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.CPU_ID'Access);
      XML_Processors.Register
        (Process => Subject.Global_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Local_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Memory_Types'Access);
      XML_Processors.Register
        (Process => Subject.Runnability'Access);
      XML_Processors.Register
        (Process => Subject.Scheduling_Group_IDs'Access);
      XML_Processors.Register
        (Process => Subject.Logical_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Logical_Device_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Logical_IRQ_MSI_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Subject.Logical_Unmask_Event'Access);
      XML_Processors.Register
        (Process => Subject.Initramfs_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Subject.Crash_Audit_Write_Access'Access);
      XML_Processors.Register
        (Process => Subject.Device_Mmconf_Mappings'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Entry_Checks'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Pin_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Proc_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Proc2_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VM_Exit_Controls_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VM_Entry_Controls_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_CR0_Mask_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_CR4_Mask_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Exception_Bitmap_Requirements'Access);
      XML_Processors.Register
        (Process => Events.Physical_Event_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_Targets'Access);
      XML_Processors.Register
        (Process => Events.Self_References'Access);
      XML_Processors.Register
        (Process => Events.Switch_Same_Core'Access);
      XML_Processors.Register
        (Process => Events.IPI_Different_Core'Access);
      XML_Processors.Register
        (Process => Events.Target_Event_ID_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Validity'Access);
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_VMX_Exit_Event_Completeness'Access);
      XML_Processors.Register
        (Process => Events.Self_Event_Action'Access);
      XML_Processors.Register
        (Process => Events.Kernel_Mode_Event_Actions'Access);
      XML_Processors.Register
        (Process => Events.Kernel_Mode_System_Actions'Access);
      XML_Processors.Register
        (Process => Events.Level_Triggered_Unmask_IRQ_Action'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Space'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Overlap'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Size'Access);
      XML_Processors.Register
        (Process => Hardware.PCI_Config_Space'Access);
      XML_Processors.Register
        (Process => Hardware.CPU_Count'Access);
      XML_Processors.Register
        (Process => Hardware.CPU_Sub_Elements'Access);
      XML_Processors.Register
        (Process => Hardware.System_Board_Presence'Access);
      -- TODO: MOA: No I/O APIC.
      -- XML_Processors.Register
      --   (Process => Hardware.IOAPIC_Presence'Access);
      -- XML_Processors.Register
      --   (Process => Hardware.IOAPIC_Cap_SID'Access);
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Device_Reference'Access);
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Type_Resources'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Root_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Root_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Context_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VTd_IRT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Cap_Agaw'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Cap_Register_Offsets'Access);
      XML_Processors.Register
        (Process => Device.IOMMU_Region_Size'Access);
      XML_Processors.Register
        (Process => Kernel.IOMMU_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Device_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Type'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Device_Domains.PCI_Bus_Context_Region_Presence'Access);
   end Register_X86_64;

   -------------------------------------------------------------------------

   procedure Run (Policy : String)
   is
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Validating policy '" & Policy & "'");

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy);

      declare
         Is_ARM_System : constant Boolean
           := Mutools.System_Config.Has_Boolean
             (Data => Data,
              Name => "armv8") and then
           Mutools.System_Config.Get_Value
             (Data => Data,
              Name => "armv8");
      begin
         if Is_ARM_System then
            Register_ARMv8a;
         else
            Register_X86_64;
         end if;
      end;

      Mulog.Log
        (Msg => "Registered validators" & XML_Processors.Get_Count'Img);

      XML_Processors.Run (Data => Data);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Successfully validated policy '" & Policy & "'");
   end Run;

end Validate;
