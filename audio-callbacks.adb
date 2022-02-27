with SDL.Log; use SDL.Log;

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Audio.Callbacks is

   function Create
     (Free_Queue : Buffer_Queue_Access;
      Busy_Queue : Buffer_Queue_Access)
      return Callback_Context_Access
   is
      Result : constant Callback_Context_Access := new Callback_Context;
   begin
      Result.Free_Queue := Free_Queue;
      Result.Busy_Queue := Busy_Queue;
      return Result;
   end Create;

   procedure Set_Spec (Context : in out Callback_Context;
                       Spec    : Obtained_Spec)
   is
   begin
      Reset
        (Context.Resampler,
         Float (Gade.Audio_Buffer.Samples_Second),
         Float (Spec.Frequency));

      Context.Margin_Frames :=
        Positive (Margin_Size * Float (Spec.Frequency));
      Context.Margin_Low := Positive (Spec.Samples);
      Context.Margin_High :=
        Positive (Spec.Samples) + Context.Margin_Frames;
   end Set_Spec;

   function User_Data (Context : aliased in out Callback_Context)
                       return User_Data_Access is
   begin
      return Context'Access;
   end User_Data;

   function Callback (Context : aliased in out Callback_Context)
                      return Audio_Callback is
      pragma Unreferenced (Context);
   begin
      return SDL_Callback'Access;
   end Callback;

   procedure Resample
     (Context : in out Callback_Context;
      Input   : Sample_Buffers.Bounded_Buffer;
      Output  : in out Circular_Buffer)
   is
      Input_Frequency : constant Float := Float (Input.Length * 60);  -- TODO: use constant for fps
      Fill_Level      : constant Float := Context.Fill_Level;

      Dynamic_Frequency : Float;
   begin
      if Fill_Level >= 1.0 then
         Put_Debug ("Fill Level" & Fill_Level'Img & Context.Ring.Length'Img & " OVERFLOW");
      elsif Fill_Level <= 0.0 then
         Put_Debug ("Fill Level" & Fill_Level'Img & Context.Ring.Length'Img & " UNDERFLOW");
      else
         Put_Verbose ("Fill Level" & Fill_Level'Img & Context.Ring.Length'Img);
      end if;

      Dynamic_Frequency :=
         (((1.0 - Max_Delta) + 2.0 * Fill_Level * Max_Delta) * Input_Frequency);

      Context.Resampler.Set_Input_Frequency (Dynamic_Frequency);
      Context.Resampler.Resample (Input, Output);
   end Resample;

   procedure Write_Silence (Buffer : out Float_Buffers.Data_Container) is
   begin
      if Buffer'Size > 0 then
         Put_Error ("Silence Padding:" & Integer'Image (Buffer'Length));
      end if;
      Buffer := (others => (0.0, 0.0));
   end Write_Silence;

   function Estimated_Frames (Input : Video_Frame_Sample_Buffer) return Natural is
      Input_Frames : constant Natural := Input.Length;
      Nominal_Input_Frequency : constant Float := Float (Samples_Second / 4);
      Min_Input_Frequency : constant Float := Nominal_Input_Frequency * (1.0 - Max_Delta);
      --  TODO: put in record:
      --  Worst case frequency ratio
      Frequency_Ratio : constant Float := Min_Input_Frequency / Float (48_000);
   begin
      return Natural (Float (Input_Frames) / Frequency_Ratio);
   end Estimated_Frames;

   procedure Flush
     (Input        : in out Circular_Buffer;
      Output       : in out Float_Buffers.Data_Container;
      Output_Index : in out Positive)
   is
   begin
      while Output_Index <= Output'Last and not Input.Is_Empty loop
         Input.Pop (Output (Output_Index));
         Output_Index := Output_Index + 1;
      end loop;
   end Flush;

   procedure SDL_Callback
     (User_Data : User_Data_Access;
      Buffer    : out Float_Buffers.Data_Container)
   is
      Context : constant Callback_Context_Access := Callback_Context_Access (User_Data);
      --  Audio : Audio_Access renames UD.Audio;
      Input_Buffer : Bounded_Buffer_Access;

      Buffer_Index : Positive;

      --  Dequeued : Natural := 0;

      --  Input_Buffers : Bound
      Diff : Integer;

      Estimated_Input_Frames : Natural := 0;
      New_Estimated_Input_Frames : Natural;
      Input_Buffers : array (1 .. Frame_Buffer_Count) of Bounded_Buffer_Access;
      Input_Buffer_Count : Natural := 0;
   begin
      Buffer_Index := 1;

--        Put_Debug ("Pre Flush I: Buffer'Length:" & Integer'Image (Buffer'Length) &
--                     " Ring.Length:" & Integer'Image (UD.Circular.Length) &
--                     " Ring.Available:" & Integer'Image (UD.Circular.Available) &
--                     " Margin_Low:" & Integer'Image (UD.Margin_Low) &
--                     " Margin_High:" & Integer'Image (UD.Margin_High) &
--                     " FL:" & Fill_Level (UD.all, UD.Circular)'Img);

      Flush (Context.Ring, Buffer, Buffer_Index);

--        Put_Debug ("Post Flush I: Buffer'Length:" & Integer'Image (Buffer'Length) &
--                     " Ring.Length:" & Integer'Image (UD.Circular.Length) &
--                     " Ring.Available:" & Integer'Image (UD.Circular.Available) &
--                     " Margin_Low:" & Integer'Image (UD.Margin_Low) &
--                     " Margin_High:" & Integer'Image (UD.Margin_High) &
--                     " FL:" & Fill_Level (UD.all, UD.Circular)'Img);

      Input_Buffer := Context.Busy_Queue.Peek;
      while Input_Buffer /= null loop --  and then Can_Fit (Input_Buffer.all, UD.Circular) loop
         --  Put_Error ("Estimated_Frames:" & Estimated_Frames (Input_Buffer.all)'Img);
         New_Estimated_Input_Frames := Estimated_Frames (Input_Buffer.all) + Estimated_Input_Frames;
         if Context.Ring.Available >= New_Estimated_Input_Frames then
            Context.Busy_Queue.Dequeue_No_Block (Input_Buffer);
            Input_Buffer_Count := Input_Buffer_Count + 1;
            Input_Buffers (Input_Buffer_Count) := Input_Buffer;
            Estimated_Input_Frames := New_Estimated_Input_Frames;
            Context.Free_Queue.Queue (Input_Buffer);
            Input_Buffer := Context.Busy_Queue.Peek;
         else
            Input_Buffer := null;
         end if;
      end loop;

      for Input_Buffer of Input_Buffers (1 .. Input_Buffer_Count) loop
         Diff := Context.Ring.Length;
         Context.Resample (Input_Buffer.all, Context.Ring);
         Diff := Context.Ring.Length - Diff;
         --  Put_Error ("Estimated VS Real" & Estimated_Frames (Input_Buffer.all)'Img & Diff'Img);
      end loop;

      --  Put_Error ("Estimated_Input_Frames" & Estimated_Input_Frames'Img & UD.Circular.Length'Img);

--        Audio.Busy_Queue.Dequeue_No_Block (Input_Buffer);
--        while Input_Buffer /= null loop
--           Dequeued := Dequeued + 1;
--           --  This might attempt to fill the circular buffer past capacity:
--           Audio.Resample (Input_Buffer.all, UD.Circular);
--           Audio.Free_Queue.Queue (Input_Buffer);
--           Audio.Busy_Queue.Dequeue_No_Block (Input_Buffer);
--        end loop;

      --  Put_Critical ("Dequeued" & Input_Buffer_Count'Img);

--        Put_Debug ("Pre Flush II: Buffer'Length:" & Integer'Image (Buffer'Length) &
--                     " Ring.Length:" & Integer'Image (UD.Circular.Length) &
--                     " Ring.Available:" & Integer'Image (UD.Circular.Available) &
--                     " Margin_Low:" & Integer'Image (UD.Margin_Low) &
--                     " Margin_High:" & Integer'Image (UD.Margin_High) &
--                     " FL:" & Fill_Level (UD.all, UD.Circular)'Img);
--
      Flush (Context.Ring, Buffer, Buffer_Index);

      Write_Silence (Buffer (Buffer_Index .. Buffer'Last));
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Callback Exception");
         Ada.Text_IO.Put_Line (Exception_Message (E));
--           if Input_Buffer /= null then
--              --  TODO: Prevent this recovery from being needed alltogether
--              Audio.Free_Queue.Queue (Input_Buffer);
--           end if;
   end SDL_Callback;

   function Fill_Level (Context : Callback_Context) return Float is
      Length : constant Natural := Context.Ring.Length;
   begin
      return
        (if Length <= Context.Margin_Low then 0.0
         elsif Length >= Context.Margin_High then 1.0
         else Float (Length - Context.Margin_Low) /
             Float (Context.Margin_Frames));
   end Fill_Level;

end Audio.Callbacks;
