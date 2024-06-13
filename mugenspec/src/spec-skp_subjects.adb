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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;
with Mutools.System_Config;

with Spec.VMX_Types;

with String_Templates;

package body Spec.Skp_Subjects
is

   use Ada.Strings.Unbounded;

   --  Add subject GPR values to buffer.
   procedure Add_GPRs
     (Buffer : in out Unbounded_String;
      GPRs   :        DOM.Core.Node);

   --  Add subject segment register values to buffer.
   procedure Add_Segment_Regs
     (Buffer   : in out Unbounded_String;
      Seg_Regs :        DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Add_GPRs
     (Buffer : in out Unbounded_String;
      GPRs   :        DOM.Core.Node)
   is
      Regs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query (N     => GPRs,
                                            XPath => "*");
   begin
      Buffer := Buffer
        & Indent & "    GPRs               => SK.CPU_Registers_Type'("
        & ASCII.LF
        & Indent (N => 3) & " CR2 => 16#0000#";
      for I in 0 .. DOM.Core.Nodes.Length (List => Regs) - 1 loop
         declare
            Reg       : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Regs,
               Index => I);
            Reg_Name  : constant String := Ada.Characters.Handling.To_Upper
              (Item => DOM.Core.Nodes.Node_Name (N => Reg));
            Reg_Value : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Nodes.Node_Value
                   (N => DOM.Core.Nodes.First_Child (N => Reg)));
         begin

            if Reg_Name /= "RIP" and then Reg_Name /= "RSP" then
               Buffer := Buffer & "," & ASCII.LF
                 & Indent (N => 3) & " " & Reg_Name & " => "
                 & Mutools.Utils.To_Hex (Number => Reg_Value);
            end if;
         end;
      end loop;
      Buffer := Buffer & ")," & ASCII.LF;
   end Add_GPRs;

   -------------------------------------------------------------------------

   procedure Add_Segment_Regs
     (Buffer   : in out Unbounded_String;
      Seg_Regs :        DOM.Core.Node)
   is
      Segs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query (N     => Seg_Regs,
                                            XPath => "*");
   begin
      Buffer := Buffer
        & Indent & "    Segment_Regs       => SK.Segment_Registers_Type'("
        & ASCII.LF;
      for I in 0 .. DOM.Core.Nodes.Length (List => Segs) - 1 loop
         declare
            Seg : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Segs,
               Index => I);
            Seg_Name : constant String := Ada.Characters.Handling.To_Upper
              (Item => DOM.Core.Nodes.Node_Name (N => Seg));
            Seg_Selector : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Seg,
               Name => "selector");
            Seg_Base : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Seg,
               Name => "base");
            Seg_Limit : constant String := DOM.Core.Elements.Get_Attribute
                (Elem => Seg,
                 Name => "limit");
            Seg_AR : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Seg,
               Name => "access");
         begin
            if I > 0 then
               Buffer := Buffer & ")," & ASCII.LF;
            end if;

            Buffer := Buffer & Indent (N => 3) & " " & Seg_Name & " =>"
              & ASCII.LF
              & Indent (N => 4) & "(Selector      => " & Seg_Selector & ","
              & ASCII.LF
              & Indent (N => 4) & " Base          => " & Seg_Base & ","
              & ASCII.LF
              & Indent (N => 4) & " Limit         => " & Seg_Limit & ","
              & ASCII.LF
              & Indent (N => 4) & " Access_Rights => " & Seg_AR & "";
         end;
      end loop;
      Buffer := Buffer & "))," & ASCII.LF;
   end Add_Segment_Regs;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Is_ARM_System : constant Boolean
        := Mutools.System_Config.Has_Boolean
          (Data => Policy,
           Name => "armv8") and then
        Mutools.System_Config.Get_Value
          (Data => Policy,
           Name => "armv8");
   begin
      if Is_ARM_System then
         Write_ARMv8a (Output_Dir => Output_Dir,
                       Policy     => Policy);
      else
         Write_X86_64 (Output_Dir => Output_Dir,
                       Policy     => Policy);
      end if;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_ARMv8a
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Interfaces;

      --  (1) extract all subjects and physical memory nodes
      Subjects        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Physical_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");

      --  (2) extract all physical devices with GIC capability
      Physical_GIC_Dev : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device" &
             "[capabilities/capability/@name='gic']");

      --  (3) extract all physical memory regions that contain a linux
      --  device tree (i.e. type of 'subject_devicetree')
      Physical_DTS : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_devicetree']");

      --  (4) calculate number of subjects
      Subj_Count  : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      Buffer : Unbounded_String;
      Tmpl   : Mutools.Templates.Template_Type;

      --  Check if given subject is Linux VM.
      function Is_Linux_VM (Subject : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Is_Linux_VM (Subject : DOM.Core.Node) return Boolean
      is
      begin
         --  (a) for every physical device tree memory node check if the
         --  given subject exclusively makes use of this dts region to un-
         --  ambiguously identify a Linux VM subject
         for I in 0 .. DOM.Core.Nodes.Length (Physical_DTS) - 1 loop
            declare
               DTS_Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Physical_DTS,
                                         Index => I);
               DTS_Name : constant String
                 := DOM.Core.Elements.Get_Attribute (Elem => DTS_Node,
                                                     Name => "name");

               -- (b) extract all dts memory regions with given physical name
               Linux_Subjects : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subject,
                    XPath => "memory/memory[@physical='" & DTS_Name & "']");
            begin
               --  (c) list has to be either length 1 or 0, with 1 indicating
               --  a subject of type Linux VM
               if DOM.Core.Nodes.Length (Linux_Subjects) = 1 then
                  return True;
               end if;
            end;
         end loop;

         return False;
      end Is_Linux_VM;

      ----------------------------------------------------------------------

      --  Append SPARK specification of given subject to template buffer.
      procedure Write_Subject_Spec (Subject : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : DOM.Core.Node)
      is

         function U
           (Source : String)
            return Unbounded_String
            renames To_Unbounded_String;

         --  (a) extract basic configuration values
         Name    : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                               Name => "name");
         Subj_ID : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                               Name => "globalId");
         CPU_ID  : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                               Name => "cpu");

         --  (b) get category for given subject (either "Linux" or "Native")
         --  and set default cacheability and trapped instructions accordingly
         Linux_VM             : constant Boolean
           := Is_Linux_VM (Subject => Subject);
         Subject_Category     : constant String
           := (if Linux_VM then "Linux" else "Native");
         Default_Cacheability : constant String
           := (if Linux_VM then "False" else "True");
         Trap_WFI_Instruction : constant String
           := (if Linux_VM then "False" else "True");
         Trap_WFE_Instruction : constant String
           := (if Linux_VM then "False" else "True");

         --  (c) extract virtual translation table base address
         VTTBR_Address : constant Unsigned_64
           := Unsigned_64'Value (Muxml.Utils.Get_Attribute
                                 (Nodes     => Physical_Memory,
                                  Refs      => ((Name  => U ("type"),
                                                 Value => U ("system_pt")),
                                                (Name  => U ("name"),
                                                 Value => U (Name & "|pt"))),
                                  Attr_Name => "physicalAddress"));

         --  (d) check if GIC device is used by subject (NOTE - currently
         --  only one GIC device supported)
         Virtual_GIC_Dev   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "devices/device[@physical='" & DOM.Core.Elements.
                Get_Attribute (Elem => DOM.Core.Nodes.
                                     Item (List  => Physical_GIC_Dev,
                                           Index => 0),
                               Name => "name") & "']");
         Interrupt_Support : constant String
           := (if DOM.Core.Nodes.Length (Virtual_GIC_Dev) = 1
               then "True" else "False");
      begin
         Buffer := Buffer & Subj_ID & " =>" &
           ASCII.LF & Indent (N => 3) &
           "  (CPU_ID               => " & CPU_ID & "," &
           ASCII.LF & Indent (N => 3) &
           "   Subject_Category     => " & Subject_Category & "," &
           ASCII.LF & Indent (N => 3) &
           "   VTTBR_Address        => " &
           Mutools.Utils.To_Hex (Number => VTTBR_Address) & "," &
           ASCII.LF & Indent (N => 3) &
           "   Default_Cacheability => " & Default_Cacheability & "," &
           ASCII.LF & Indent (N => 3) &
           "   Trap_WFI_Instruction => " & Trap_WFI_Instruction & "," &
           ASCII.LF & Indent (N => 3) &
           "   Trap_WFE_Instruction => " & Trap_WFE_Instruction & "," &
           ASCII.LF & Indent (N => 3) &
           "   Interrupt_Support    => " & Interrupt_Support & ")";
      end Write_Subject_Spec;
   begin
      Mulog.Log (Msg => "Writing subject spec to '"
                 & Output_Dir & "/skp-subjects.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_armv8a_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.ads");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_armv8a_adb);

      for I in 0 .. Subj_Count - 1 loop
         if I /= 0 then
            Buffer := Buffer & Indent (N => 3);
         end if;

         Write_Subject_Spec
           (Subject => DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I));

         if I < Subj_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subjects__",
         Content  => To_String (Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.adb");
   end Write_ARMv8a;

   -------------------------------------------------------------------------

   procedure Write_X86_64
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Interfaces;

      Subjects    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subj_Count  : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      Buffer   : Unbounded_String;
      Subj_Buf : Unbounded_String;
      Tmpl     : Mutools.Templates.Template_Type;

      --  Append SPARK specification of given subject to template buffer.
      procedure Write_Subject_Spec (Subject : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         package VMX renames VMX_Types;

         function U
           (Source : String)
            return Unbounded_String
            renames To_Unbounded_String;

         --  EPT memory type WB, page-walk length 4
         EPT_Flags : constant := 16#1e#;

         Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "name");
         Subj_ID : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "globalId");
         CPU_ID  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "cpu");

         Sibling : constant String
           := Muxml.Utils.Get_Attribute (Doc   => Subject,
                                         XPath => "sibling",
                                         Name  => "ref");
         PT_Name : constant String
           := (if Sibling'Length > 0 then Sibling else Name);
         PML4_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_pt")),
                             (Name  => U ("name"),
                              Value => U (PT_Name & "|pt"))),
               Attr_Name => "physicalAddress"));

         GPR_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Subject,
              XPath => "vcpu/registers/gpr");
         Segments_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Subject,
              XPath => "vcpu/registers/segments");
         Entry_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => GPR_Node,
               XPath => "rip"));
         Stack_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => GPR_Node,
               XPath => "rsp"));
         VMCS_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("kernel_vmcs")),
                             (Name  => U ("name"),
                              Value => U (Name & "|vmcs"))),
               Attr_Name => "physicalAddress"));
         IOBM_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_iobm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|iobm"))),
               Attr_Name => "physicalAddress"));
         MSRBM_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_msrbm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|msrbm"))),
               Attr_Name => "physicalAddress"));

         MSR_Store_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes => Phys_Memory,
              Refs  => ((Name  => U ("type"),
                         Value => U ("kernel_msrstore")),
                        (Name  => U ("name"),
                         Value => U (Name & "|msrstore"))));
         MSR_Store_Addr : Unsigned_64 := 0;
         MSR_Count      : Natural     := 0;

         Pin_Ctrls   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/pin/*");
         Proc_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc/*");
         Proc2_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc2/*");
         Entry_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/entry/*");
         Exit_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/exit/*");
         CR0_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr0/*");
         CR0_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr0/*");
         CR0_Shadow   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr0Shadow/*");
         CR4_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr4/*");
         CR4_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr4/*");
         CR4_Shadow   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr4Shadow/*");
         Exceptions  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/exception/*");
      begin
         if MSR_Store_Node /= null then
            MSR_Store_Addr := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => MSR_Store_Node,
                  Name => "physicalAddress"));
            declare
               Ctrls_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Subject,
                    XPath => "vcpu/vmx/controls");
               Debug_Ctrl : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_DEBUGCTL
                   (Controls => Ctrls_Node);
               PERF_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PERFGLOBALCTRL
                   (Controls => Ctrls_Node);
               PAT_Ctrl   : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PAT (Controls => Ctrls_Node);
               EFER_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_EFER
                   (Controls => Ctrls_Node);
            begin
               MSR_Count := Mutools.XML_Utils.Calculate_MSR_Count
                 (MSRs                   => McKae.XML.XPath.XIA.XPath_Query
                    (N     => Subject,
                     XPath => "vcpu/msrs/msr[@mode='w' or @mode='rw']"),
                  DEBUGCTL_Control       => Debug_Ctrl,
                  PAT_Control            => PAT_Ctrl,
                  PERFGLOBALCTRL_Control => PERF_Ctrl,
                  EFER_Control           => EFER_Ctrl);
            end;
         end if;

         Subj_Buf := To_Unbounded_String (Indent (N => 2)) & Subj_ID
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    CPU_ID             => " & CPU_ID & ","
           & ASCII.LF;

         if Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
         then
            Subj_Buf := Subj_Buf
              & Indent & "    PML4_Address       => 0,"
              & ASCII.LF
              & Indent & "    EPT_Pointer        => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr + EPT_Flags)
              & ",";
         else
            Subj_Buf := Subj_Buf
              & Indent & "    PML4_Address       => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr) & ","
              & ASCII.LF
              & Indent & "    EPT_Pointer        => 0,";
         end if;

         Subj_Buf := Subj_Buf & ASCII.LF
           & Indent & "    VMCS_Address       => "
           & Mutools.Utils.To_Hex (Number => VMCS_Addr) & ","
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address  => "
           & Mutools.Utils.To_Hex (Number => IOBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Bitmap_Address => "
           & Mutools.Utils.To_Hex (Number => MSRBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Store_Address  => "
           & Mutools.Utils.To_Hex (Number => MSR_Store_Addr) & ","
           & ASCII.LF
           & Indent & "    Stack_Address      => "
           & Mutools.Utils.To_Hex (Number => Stack_Addr) & ","
           & ASCII.LF
           & Indent & "    Entry_Point        => "
           & Mutools.Utils.To_Hex (Number => Entry_Addr) & ","
           & ASCII.LF;

         Add_GPRs (Buffer => Subj_Buf,
                   GPRs   => GPR_Node);

         Add_Segment_Regs (Buffer   => Subj_Buf,
                           Seg_Regs => Segments_Node);

         Subj_Buf := Subj_Buf
           & Indent & "    CR0_Value          => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR0 (Fields => CR0_Value))
           & "," & ASCII.LF
           & Indent & "    CR0_Shadow         => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR0 (Fields => CR0_Shadow))
           & "," & ASCII.LF
           & Indent & "    CR0_Mask           => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR0
                                   (Fields  => CR0_Mask,
                                    Default => Interfaces.Unsigned_64'Last))
           & "," & ASCII.LF
           & Indent & "    CR4_Value          => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR4 (Fields => CR4_Value))
           & "," & ASCII.LF
           & Indent & "    CR4_Shadow         => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR4 (Fields => CR4_Shadow))
           & "," & ASCII.LF
           & Indent & "    CR4_Mask           => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR4
                                   (Fields => CR4_Mask,
                                    Default => Interfaces.Unsigned_64'Last))
           & "," & ASCII.LF
           & Indent & "    Exception_Bitmap   => "
           & Mutools.Utils.To_Hex
           (Number => VMX.Get_Exceptions (Fields  => Exceptions,
                                          Default => 16#ffff_ffff#)) & ","
           & ASCII.LF
           & Indent & "    MSR_Count          =>" & MSR_Count'Img & ","
           & ASCII.LF
           & Indent & "    VMX_Controls       => VMX_Controls_Type'("
           & ASCII.LF
           & Indent (N => 3) & " Exec_Pin    =>"
           & VMX.Get_Pin_Controls (Fields => Pin_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc   =>"
           & VMX.Get_Proc_Controls (Fields => Proc_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc2  =>"
           & VMX.Get_Proc2_Controls (Fields => Proc2_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exit_Ctrls  =>"
           & VMX.Get_Exit_Controls (Fields => Exit_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Entry_Ctrls =>"
           & VMX.Get_Entry_Controls (Fields => Entry_Ctrls)'Img  & "))";
      end Write_Subject_Spec;
   begin
      Mulog.Log (Msg => "Writing subject spec to '"
                 & Output_Dir & "/skp-subjects.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.ads");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_adb);

      for I in 0 .. Subj_Count - 1 loop
         Write_Subject_Spec
           (Subject => DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I));

         if I < Subj_Count - 1 then
            Subj_Buf := Subj_Buf & "," & ASCII.LF;
         end if;
         Buffer := Buffer & Subj_Buf;
      end loop;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subjects__",
         Content  => To_String (Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.adb");
   end Write_X86_64;

end Spec.Skp_Subjects;
