--
--  Copyright (C) 2016-2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016-2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams.Stream_IO;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.Files;
with Mutools.Utils;

package body Memhashes.Utils
is

   --  Get first child node of `Node` that has name "file" or "fill", or null
   --  if none present.
   function File_Or_Fill_Child (Node : DOM.Core.Node) return DOM.Core.Node;

   -----------------------------------------------------------------------------

   function File_Or_Fill_Child (Node : DOM.Core.Node) return DOM.Core.Node
   is
      use type DOM.Core.Node;
      N : DOM.Core.Node := DOM.Core.Nodes.First_Child (Node);
   begin
      while N /= null loop
         if DOM.Core.Nodes.Local_Name (N) = "file" or else
           DOM.Core.Nodes.Local_Name (N) = "fill"
         then
            return N;
         end if;
         N := DOM.Core.Nodes.Next_Sibling (N);
      end loop;
      return null;
   end File_Or_Fill_Child;

   -------------------------------------------------------------------------

   function SHA256_Digest
     (Node       : DOM.Core.Node;
      Input_Dirs : Mutools.Strings.String_Array)
      return Result_Type
   is
      type Content_Type is (File, Fill);

      use type Ada.Streams.Stream_Element_Offset;

      --  Hash specified pattern with given size into specified context.
      procedure Hash_Pattern
        (Context : in out GNAT.SHA256.Context;
         Pattern :        Ada.Streams.Stream_Element;
         Size    :        Ada.Streams.Stream_Element_Offset);

      ----------------------------------------------------------------------

      Mem_Size : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "size"));
      Content_Node : constant DOM.Core.Node := File_Or_Fill_Child (Node);
      Content_Kind : constant Content_Type
        := Content_Type'Value
          (DOM.Core.Elements.Get_Tag_Name (Elem => Content_Node));
      Buf_Size     : constant Ada.Streams.Stream_Element_Offset := 4096;
      Buf          : Ada.Streams.Stream_Element_Array (0 .. Buf_Size - 1);
      Added        : Ada.Streams.Stream_Element_Offset := 0;
      Content_Size : Ada.Streams.Stream_Element_Count  := 0;

      Hash_Context : GNAT.SHA256.Context := GNAT.SHA256.Initial_Context;

      ----------------------------------------------------------------------

      procedure Hash_Pattern
        (Context : in out GNAT.SHA256.Context;
         Pattern :        Ada.Streams.Stream_Element;
         Size    :        Ada.Streams.Stream_Element_Offset)
      is
         Count     : constant Ada.Streams.Stream_Element_Offset
           := Size / Buf_Size;
         Remainder : constant Ada.Streams.Stream_Element_Offset
           := Size mod Buf_Size;
      begin
         Buf := (others => Pattern);

         for I in 1 .. Count loop
            GNAT.SHA256.Update
              (C     => Context,
               Input => Buf);
         end loop;

         if Remainder > 0 then
            GNAT.SHA256.Update
              (C     => Context,
               Input => Buf (Buf'First .. Remainder - 1));
         end if;
      end Hash_Pattern;
   begin
      case Content_Kind
      is
         when File =>
            declare
               Offset_Str : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Content_Node,
                    Name => "offset");
               Filename : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Content_Node,
                    Name => "filename");
               Path : constant String
                 := Mutools.Utils.Lookup_File
                   (Filename    => Filename,
                    Directories => Input_Dirs);

               Offset : Ada.Streams.Stream_Element_Offset := 0;
               Last   : Ada.Streams.Stream_Element_Offset := Buf_Size - 1;
               File   : Ada.Streams.Stream_IO.File_Type;
            begin
               --  "shared=no": Allow opening the same file in distinct threads
               Mutools.Files.Open
                 (Filename => Path,
                  File     => File,
                  Writable => False,
                  Form     => "shared=no");

               if Offset_Str /= "none" then
                  Offset := Ada.Streams.Stream_Element_Offset'Value
                    (Offset_Str);
                  Ada.Streams.Stream_IO.Set_Index
                    (File => File,
                     To   => Ada.Streams.Stream_IO.Count (Offset + 1));
               end if;

               while Mem_Size - Added > 0 and Last + 1 = Buf_Size loop
                  Ada.Streams.Stream_IO.Read
                    (File => File,
                     Item => Buf,
                     Last => Last);
                  GNAT.SHA256.Update
                    (C     => Hash_Context,
                     Input => Buf
                       (Buf'First .. Ada.Streams.Stream_Element_Offset'Min
                            (Last, Mem_Size - Added)));
                  Added := Added + Last + 1;
               end loop;

               Content_Size := Offset + Added;

               if Mem_Size - Added > 0 then

                  --  Pad with 0 up to memory region size.

                  Hash_Pattern
                    (Context => Hash_Context,
                     Pattern => 0,
                     Size    => Mem_Size - Added);
               end if;

               Ada.Streams.Stream_IO.Close (File => File);

            exception
               when others =>
                  if Ada.Streams.Stream_IO.Is_Open (File => File) then
                     Ada.Streams.Stream_IO.Close (File => File);
                  end if;
                  raise;
            end;
         when Fill =>
            Hash_Pattern
              (Context => Hash_Context,
               Pattern => Ada.Streams.Stream_Element'Value
                 (DOM.Core.Elements.Get_Attribute
                      (Elem => Content_Node,
                       Name => "pattern")),
               Size    => Mem_Size);
      end case;

      return Result_Type'
        (Hash         => "16#" & GNAT.SHA256.Digest (C => Hash_Context) & "#",
         Use_Size     => Content_Kind = File,
         Content_Size => Content_Size);
   end SHA256_Digest;

end Memhashes.Utils;
