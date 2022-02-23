with Ada.Iterator_Interfaces;
with System;

generic
   type Element_Type is private;
package Bounded_Buffers is

   type Data_Container is array (Positive range <>) of aliased Element_Type;

   type Data_Container_Access is access all Data_Container;

   type Bounded_Buffer (Size : Positive) is tagged private with
     Default_Iterator => Iterate,
     Iterator_Element => Element_Type,
     Constant_Indexing => Constant_Reference;

   --  TODO: Find out how to use this to access an element by index using parentheses
   function Element (Self : Bounded_Buffer; Index : Positive)
                     return Element_Type;

   procedure Clear (Self : out Bounded_Buffer)
     with Inline;

   procedure Set_Length (Self : in out Bounded_Buffer; Count : Natural)
     with Inline;

   function Length (Self : Bounded_Buffer) return Natural
     with Inline;

   function Is_Empty (Self : Bounded_Buffer) return Boolean
     with Inline;

   procedure Append (Self : in out Bounded_Buffer; E : Element_Type)
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

   function Constant_Reference (Container : aliased in Bounded_Buffer;
                                Position  : Cursor)
                                return Constant_Reference_Type;

private

   Address_Size_Bits  : constant := System.Word_Size;
   Address_Size_Bytes : constant := Address_Size_Bits / System.Storage_Unit;

   type Bound is (Lower, Upper);

   type Bounds is array (Bound) of Natural;

   type Bounded_Buffer (Size : Positive) is tagged record
      Data  : aliased Data_Container (1 .. Size);
      Slice : aliased Bounds := (1, 0);
   end record;

   type Cursor is record
      Last     : Positive;
      Position : Positive;
   end record;

   type Iterator is new Iterators.Forward_Iterator with record
      First : Positive;
      Last  : Positive;
   end record;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is null record;

end Bounded_Buffers;
