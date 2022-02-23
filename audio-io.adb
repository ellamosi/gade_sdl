with SDL.Log; use SDL.Log;

package body Audio.IO is

   procedure Create (Audio : aliased out Instance) is
      Device   : Audio_Devices.Device renames Audio.Device;
      Desired  : Desired_Spec;
      Obtained : Obtained_Spec renames Audio.Spec;
   begin
      Desired.Frequency := 48_000;
      Desired.Format    := Sample_Format;
      Desired.Channels  := Channel_Count;
      Desired.Samples   := 4_096; --  About 80ms latency (that's more than desirable)

      Put_Debug ("Desired - Frequency :" & Desired.Frequency'Img);
      Put_Debug ("Desired - Format/Bit_Size :" & Desired.Format.Bit_Size'Img);
      Put_Debug ("Desired - Format/Float :" & Desired.Format.Float'Img);
      Put_Debug ("Desired - Format/Big_Endian :" & Desired.Format.Endianness'Img);
      Put_Debug ("Desired - Format/Signed :" & Desired.Format.Signed'Img);
      Put_Debug ("Desired - Channels :" & Desired.Channels'Img);
      Put_Debug ("Desired - Samples :" & Desired.Samples'Img);

      Put_Debug ("Opening Default Device");

      Audio_Devices.Open (Device, Desired => Desired, Obtained => Obtained);

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

      for Frame_Buffer of Audio.Frame_Buffers loop
         Frame_Buffer.Clear;
         Audio.Free_Queue.Queue (Frame_Buffer'Unchecked_Access);
      end loop;

      Reset
        (Audio.Resampler,
         Float (Gade.Audio_Buffer.Samples_Second),
         Float (Obtained.Frequency));

      Audio.Base_Frame_Capacity := Positive (Obtained.Samples);

      Audio.Resampling_Task.Start (Audio'Unchecked_Access);
   end Create;

   procedure Queue_Asynchronously (Audio : in out Instance) is
      Buffer      : Bounded_Buffer_Access;
      Frame_Count : Natural;
   begin
      Audio.Free_Queue.Dequeue_No_Block (Buffer);

      if Buffer = null then
         Put_Debug ("Unavailable free buffer");
         Buffer := Audio.Dummy_Buffer'Unrestricted_Access;
      end if;

      Generate (Data_Access (Buffer), Frame_Count);
      Buffer.Set_Length (Frame_Count);

      if Buffer /= null then
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
      Audio.Device.Pause (False);
      --  TODO: Figure out how to nicely exit the task past this point
      loop
         Audio.Busy_Queue.Dequeue (Buffer);
         Audio.Queue_Synchronously (Buffer.all);
         Audio.Free_Queue.Queue (Buffer);
      end loop;
   end Resampler_Task;

   overriding
   procedure Finalize (Self : in out Instance) is
   begin
      Put_Debug ("Audio Finalize");
      abort Self.Resampling_Task;
      Self.Device.Close;
      --  TODO: ETC
   end Finalize;

end Audio.IO;
