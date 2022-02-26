with Ada.Iterator_Interfaces;
with System;

generic
   type Element_Type is private;
package Buffers.Bounded is

   package Appendable_Buffers is new Buffers.Appendable_Buffers (Element_Type);
   use Appendable_Buffers;

   type Data_Container is array (Positive range <>) of aliased Element_Type;

   type Data_Container_Access is access all Data_Container;

   type Bounded_Buffer (Size : Positive) is new Appendable with private with
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type,
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference;

   procedure Clear (Self : out Bounded_Buffer)
     with Inline;

   procedure Set_Length (Self : in out Bounded_Buffer; Count : Natural)
     with Inline;

   function Length (Self : Bounded_Buffer) return Natural
     with Inline;

   function Is_Empty (Self : Bounded_Buffer) return Boolean
     with Inline;

   overriding
   procedure Append (Self : in out Bounded_Buffer; E : Element_Type)
     with Inline;

   function Element (Self : Bounded_Buffer; Index : Positive)
                     return Element_Type
     with Inline;

   function Data_Access (Self : in out Bounded_Buffer)
                         return Data_Container_Access;

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Iterators is new Ada.Iterator_Interfaces
     (Cursor      => Cursor,
      Has_Element => Has_Element);

   type Iterator is new Iterators.Forward_Iterator with private;

   overriding
   function First (Object : Iterator) return Cursor;

   overriding
   function Next (Object : Iterator; Position : Cursor) return Cursor;

   function Iterate (Container : Bounded_Buffer)
                     return Iterators.Forward_Iterator'Class;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is private
     with Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Element_Type) is private
     with Implicit_Dereference => Element;

   function Constant_Reference (Container : aliased in Bounded_Buffer;
                                Index     : Positive)
                                return Constant_Reference_Type;

   function Constant_Reference (Container : aliased in Bounded_Buffer;
                                Position  : Cursor)
                                return Constant_Reference_Type;

   function Reference (Container : aliased in out Bounded_Buffer;
                       Index     : in Positive)
                       return Reference_Type;

   function Reference (Container : aliased in out Bounded_Buffer;
                       Position  : in Cursor)
                       return Reference_Type;

private

   Address_Size_Bits  : constant := System.Word_Size;
   Address_Size_Bytes : constant := Address_Size_Bits / System.Storage_Unit;

   type Bound is (Lower, Upper);

   type Bounds is array (Bound) of Natural;

   type Bounded_Buffer (Size : Positive) is new Appendable with record
      Data  : aliased Data_Container (1 .. Size);
      Slice : aliased Bounds := (1, 0);
   end record;

   type Cursor is record
      Last     : Positive;
      Position : Positive; --  TODO: Rename to Index
   end record;

   type Iterator is new Iterators.Forward_Iterator with record
      First : Positive;
      Last  : Positive;
   end record;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is null record;

   type Reference_Type
     (Element : not null access Element_Type) is null record;

end Buffers.Bounded;
