package body Audio.Queues is

   protected body Buffer_Queue is
      procedure Queue (V : Bounded_Buffer_Access) is
         Pos : Container_Range;
      begin
         if Count < Container_Range'Range_Length then
            Pos := First + Container_Range (Count);
            Container (Pos) := V;
            Count := Count + 1;
         end if;
      end Queue;

      entry Dequeue (V : out Bounded_Buffer_Access) when Count > 0 is
      begin
         V := Container (First);
         First := First + 1;
         Count := Count - 1;
      end Dequeue;

      procedure Dequeue_No_Block (V : out Bounded_Buffer_Access) is
      begin
         if Count > 0 then
            V := Container (First);
            First := First + 1;
            Count := Count - 1;
         else
            V := null;
         end if;
      end Dequeue_No_Block;
   end Buffer_Queue;

end Audio.Queues;
