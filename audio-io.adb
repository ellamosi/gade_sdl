with SDL.Log; use SDL.Log;
with System.Address_Image;

package body Audio.IO is

   procedure Create (Audio : aliased out Instance) is
      Device   : Audio_Devices.Device renames Audio.Device;
      Desired  : Desired_Spec;
      Obtained : Obtained_Spec renames Audio.Spec;
   begin
      Desired.Frequency := 48_000;
      Desired.Format    := Sample_Format;
      Desired.Channels  := Channel_Count;
      Desired.Samples   := 1_024; --  About 20ms latency

      Put_Debug ("Desired - Frequency :" & Desired.Frequency'Img);
      Put_Debug ("Desired - Format/Bit_Size :" & Desired.Format.Bit_Size'Img);
      Put_Debug ("Desired - Format/Float :" & Desired.Format.Float'Img);
      Put_Debug ("Desired - Format/Big_Endian :" & Desired.Format.Endianness'Img);
      Put_Debug ("Desired - Format/Signed :" & Desired.Format.Signed'Img);
      Put_Debug ("Desired - Channels :" & Desired.Channels'Img);
      Put_Debug ("Desired - Samples :" & Desired.Samples'Img);

      Put_Debug ("Opening Default Device");

      Audio_Devices.Open
        (Device,
         Callback  => Callback'Access,
         User_Data => Audio.User_Data'Unchecked_Access,
         Desired   => Desired,
         Obtained  => Obtained);

      Put_Debug ("Opened Device:" & Device.Get_ID'Img);
      Put_Debug ("Device Status: " & Device.Get_Status'Img);

      Put_Debug ("Obtained - Frequency :" & Obtained.Frequency'Img);
      Put_Debug ("Obtained - Format/Bit_Size :" & Obtained.Format.Bit_Size'Img);
      Put_Debug ("Obtained - Format/Float : " & Obtained.Format.Float'Img);
      Put_Debug ("Obtained - Format/Endianness : " & Obtained.Format.Endianness'Img);
      Put_Debug ("Obtained - Format/Signed : " & Obtained.Format.Signed'Img);
      Put_Debug ("Obtained - Channels :" & Obtained.Channels'Img);
      Put_Debug ("Obtained - Samples :" & Obtained.Samples'Img);
      Put_Debug ("Obtained - Silence :" & Obtained.Silence'Img);
      Put_Debug ("Obtained - Size :" & Obtained.Size'Img);

      Audio.User_Data.Audio := Audio'Unchecked_Access;
      Audio.User_Data.Held_Index := 1;
      Audio.User_Data.Held_Buffer := Audio.User_Data.Silence'Unchecked_Access;
      Audio.User_Data.Silence.Set_Length (1_024);
      for Frame of Audio.User_Data.Silence loop -- TODO: Could implement aggregate syntax
         Frame := (0.0, 0.0);
      end loop;
      Audio.User_Data.Silence.Set_Length (0);

      for Frame_Buffer of Audio.Frame_Buffers loop
         Frame_Buffer.Clear;
         Audio.Free_Queue.Queue (Frame_Buffer'Unchecked_Access);
      end loop;

      Reset
        (Audio.Resampler,
         Float (Gade.Audio_Buffer.Samples_Second),
         Float (Obtained.Frequency));

      Audio.Base_Frame_Capacity := Positive (Obtained.Samples);

      Audio.Device.Pause (False);

      --  Audio.Resampling_Task.Start (Audio'Unchecked_Access);
   end Create;

   procedure Queue_Asynchronously (Audio : in out Instance) is
      Buffer      : Bounded_Buffer_Access;
      Frame_Count : Natural;

      Buffer_Was_Available : Boolean;
   begin
      Audio.Free_Queue.Dequeue_No_Block (Buffer);
      Buffer_Was_Available := Buffer /= null;

      if not Buffer_Was_Available then
         Put_Debug ("Unavailable free buffer");
         Buffer := Audio.Dummy_Buffer'Unrestricted_Access;
      end if;

      Generate (Data_Access (Buffer), Frame_Count);
      Buffer.Set_Length (Frame_Count);

      if Buffer_Was_Available then
         Audio.Busy_Queue.Queue (Buffer);
      end if;
   end Queue_Asynchronously;

   procedure Queue_Synchronously
     (Audio : in out Instance;
      Input : Sample_Buffers.Bounded_Buffer)
   is
      Output : Float_Buffers.Bounded_Buffer (Audio.Base_Frame_Capacity);
   begin
      Audio.Resample (Input, Output);
      Audio.Device.Queue (Output.Data_Access);
   end Queue_Synchronously;

   procedure Resample
     (Audio  : in out Instance;
      Input  : Sample_Buffers.Bounded_Buffer;
      Output : out Float_Buffers.Bounded_Buffer)
   is
      Device : Audio_Devices.Device renames Audio.Device;

      Queue_Size   : constant Natural := Natural (Device.Get_Queued_Size);
      Queue_Frames : constant Natural := Queue_Size / Frame_Size_Bytes;

      Input_Frequency : constant Float := Float (Input.Length * 60);  -- TODO: use constant for fps

      Fill_Level : Float :=
        Float (Queue_Frames) / Float (Audio.Base_Frame_Capacity);

      Dynamic_Frequency : Float;
   begin
      Fill_Level := 0.5; -- Temp

      if Fill_Level > High_Fill_Threshold then
         --  There are some uncommon circumstances which can cause the queue to
         --  be overfed past the nominal capacity:
         --  - LCD disabled?
         --  - SDL reporting more samples in the queue than are actually present
         --  For now this prevents from accumulation an excessive delay, while
         --  having some tolerance with the counts that SDL reports.
         Put_Debug ("Fill Level" & Fill_Level'Img & " OVERFLOW");
         Fill_Level := 0.0;
         Device.Clear_Queued;
      elsif Fill_Level <= 0.0 then
         Put_Debug ("Fill Level" & Fill_Level'Img & " UNDERFLOW");
      else
         Put_Verbose ("Fill Level" & Fill_Level'Img);
      end if;

      Dynamic_Frequency :=
         (((1.0 - Max_Delta) + 2.0 * Fill_Level * Max_Delta) * Input_Frequency);

      Audio.Resampler.Set_Input_Frequency (Dynamic_Frequency);
      Audio.Resampler.Resample (Input, Output);
   end Resample;

   task body Resampler_Task is
      Audio  : Audio_Access;
      Buffer : Bounded_Buffer_Access;
   begin
      select
         accept Start (Audio : Audio_Access) do
            Resampler_Task.Audio := Audio;
         end Start;
      or
         terminate;
      end select;
      --  TODO: Figure out how to nicely exit the task past this point
      loop
         Audio.Busy_Queue.Dequeue (Buffer);
         --  Audio.Queue_Synchronously (Buffer.all);
         Audio.Free_Queue.Queue (Buffer);
      end loop;
   end Resampler_Task;

   procedure Callback
     (User   : in Audio_Devices.User_Data_Access;
      Buffer : out Float_Buffers.Data_Container)
   is
      UD : constant Support_User_Data_Access := Support_User_Data_Access (User);
      Audio : Audio_Access renames UD.Audio;
      Input_Buffer : Bounded_Buffer_Access;

      Buffer_Index : Positive;

      --  FF : Float_Frame;
      --  F : Float;
      Silence_Frame_Count : Natural;
   begin
      Put_Debug ("Start Callback");
      Buffer_Index := Buffer'First;
      while Buffer_Index <= Buffer'Last loop
         --  UD.Held_Buffer.Set_Length (800);
         --  Flush existing buffer
         Put_Debug ("Flushing existing buffer " & UD.Held_Index'Img & " .." & UD.Held_Buffer.Length'Img);
         while UD.Held_Index <= UD.Held_Buffer.Length and Buffer_Index <= Buffer'Last loop
            Buffer (Buffer_Index) := UD.Held_Buffer.all (UD.Held_Index);

--              UD.Counter := UD.Counter + 1;
--              if UD.Counter > 100 then
--                 UD.Counter := 0;
--                 UD.State := not UD.State;
--              end if;
--              F := (if UD.State then 1.0 else -1.0);
--              Buffer (Buffer_Index) := (F, F);

            UD.Held_Index := UD.Held_Index + 1;
            Buffer_Index := Buffer_Index + 1;
         end loop;
         --  Get new buffer

         if UD.Held_Index > UD.Held_Buffer.Length and Buffer_Index <= Buffer'Last then
            Audio.Busy_Queue.Dequeue_No_Block (Input_Buffer);

            --  Put_Debug ("Samples: " & UD.Held_Buffer.Length'Img & UD.Held_Buffer (1).Left'Img);
            if Input_Buffer /= null then
               Put_Debug ("Dequeued " & System.Address_Image (Input_Buffer.all'Address));
               UD.Held_Buffer := UD.Resampled'Access;
               Audio.Resample (Input_Buffer.all, UD.Held_Buffer.all);
               Audio.Free_Queue.Queue (Input_Buffer);

            else
               --  Fill Buffer with silence until end of this Callback call
               UD.Held_Buffer := UD.Silence'Access;
               Silence_Frame_Count := Buffer'Length + 1 - Buffer_Index;
               UD.Held_Buffer.Set_Length (Silence_Frame_Count);
               Put_Warn ("Could not dequeue: adding silence" & Silence_Frame_Count'Img);
            end if;
            UD.Held_Index := 1;

                     --  Partially dump new buffer
            Put_Debug ("Dumping new buffer " & UD.Held_Index'Img & " .." & UD.Held_Buffer.Length'Img);
            while UD.Held_Index <= UD.Held_Buffer.Length and Buffer_Index <= Buffer'Last loop
               Buffer (Buffer_Index) := UD.Held_Buffer.all (UD.Held_Index);


               --              UD.Counter := UD.Counter + 1;
               --              if UD.Counter > 100 then
               --                 UD.Counter := 0;
               --                 UD.State := not UD.State;
               --              end if;
               --              F := (if UD.State then 1.0 else -1.0);
               --              Buffer (Buffer_Index) := (F, F);

               UD.Held_Index := UD.Held_Index + 1;
               Buffer_Index := Buffer_Index + 1;
            end loop;
         end if;
      end loop;
      Put_Debug ("End Callback");
   exception
         when others => Put_Critical ("Callback exception");
   end Callback;

   overriding
   procedure Finalize (Self : in out Instance) is
   begin
      Put_Debug ("Audio Finalize");
      Self.Device.Pause (True);
      abort Self.Resampling_Task;
      Self.Device.Close;
      --  TODO: ETC
   end Finalize;

end Audio.IO;
