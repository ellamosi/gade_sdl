private package Audio.Queues is
   --  This package could be made generic, but there's a substantial reduction
   --  of verbosity in not doing so, such as the ability of relying on modular
   --  types for indexing or  relying on null access return values that not all
   --  types could offer.

   Frame_Buffer_Count : constant := 3;

   type Container_Range is mod Frame_Buffer_Count;

   type Buffer_Queue_Container is array (Container_Range) of Bounded_Buffer_Access;

   protected type Buffer_Queue is
      procedure Queue (V : not null Bounded_Buffer_Access);
      entry Dequeue (V : out not null Bounded_Buffer_Access);
      procedure Dequeue_No_Block (V : out Bounded_Buffer_Access);
      function Peek return Bounded_Buffer_Access;
   private
      Container : Buffer_Queue_Container;
      First     : Container_Range := 0;
      Count     : Natural := 0;
   end Buffer_Queue;

   type Buffer_Queue_Access is access all Buffer_Queue;

   --  TODO: should use new types for busy/free queues to prevent mixing

end Audio.Queues;
