private with Audio.Queues, Audio.Resamplers;

with SDL.Audio.Devices;
with SDL.Audio.Sample_Formats;

with Ada.Finalization;
with System;

package Audio.IO is

   type Instance is new Ada.Finalization.Limited_Controlled with private;

   --  TODO: Rename to Self
   procedure Create (Audio : aliased out Instance);

   generic
      with procedure Generate (Buffer : Audio_Buffer_Access;
                               Count  : out Natural);
   procedure Queue_Asynchronously (Audio : in out Instance);

   overriding
   procedure Finalize (Self : in out Instance);

private
   use Audio.Queues, Audio.Resamplers;

   Channel_Count : constant := 2;

   Max_Delta           : constant Float := 0.005;
   High_Fill_Threshold : constant Float := 4.0; --  Clear SDL queue if surpassed

   type Audio_Access is access all Instance;

   package Audio_Devices is new SDL.Audio.Devices
     (Frame_Type   => Float_Frame,
      Buffer_Index => Positive,
      Buffer_Type  => Float_Buffers.Data_Container);
   use Audio_Devices;

   task type Resampler_Task is
      entry Start (Audio : Audio_Access);
   end Resampler_Task;

   type Frame_Buffer_Array is array (1 .. Frame_Buffer_Count)
     of aliased Video_Frame_Sample_Buffer;

   type Float_Buffer_Access is access all Float_Buffers.Bounded_Buffer (1_024);

   type Support_User_Data is new Audio_Devices.User_Data with record
      Audio       : Audio_Access;
      --  Held_Buffer : Bounded_Buffer_Access;
      Held_Index  : Positive;

      Held_Buffer : Float_Buffer_Access;
      Resampled   : aliased Float_Buffers.Bounded_Buffer (1_024);
      Silence     : aliased Float_Buffers.Bounded_Buffer (1_024);

      Counter : Integer := 0;
      State   : Boolean := True;
   end record;

   procedure Callback
     (User   : in Audio_Devices.User_Data_Access;
      Buffer : out Float_Buffers.Data_Container);

   type Support_User_Data_Access is access all Support_User_Data;

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Device              : Audio_Devices.Device;
      Spec                : Obtained_Spec;
      Resampler           : Resamplers.Resampler;
      Base_Frame_Capacity : Positive; --  Try not to fill SDL queue more than this

      Resampling_Task  : Resampler_Task;

      Dummy_Buffer  : aliased Video_Frame_Sample_Buffer;
      Frame_Buffers : Frame_Buffer_Array;

      Free_Queue : Buffer_Queue;
      Busy_Queue : Buffer_Queue;

      User_Data : aliased Support_User_Data;
   end record;

   Sample_Format : constant SDL.Audio.Sample_Formats.Sample_Format :=
     SDL.Audio.Sample_Formats.Sample_Format_F32LSB;

   Frame_Size_Bytes : constant Natural :=
     Natural (Sample_Format.Bit_Size) * Channel_Count / System.Storage_Unit;

   procedure Resample
     (Audio  : in out Instance;
      Input  : Sample_Buffers.Bounded_Buffer;
      Output : out Float_Buffers.Bounded_Buffer);

   procedure Queue_Synchronously
     (Audio : in out Instance;
      Input : Sample_Buffers.Bounded_Buffer);

end Audio.IO;
