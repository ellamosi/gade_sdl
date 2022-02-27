private with Audio.Queues, Audio.Callbacks;

with SDL.Audio.Sample_Formats;

with Ada.Finalization;
with System;

package Audio.IO is

   type Instance is new Ada.Finalization.Limited_Controlled with private;

   procedure Create (Self : aliased out Instance);

   generic
      with procedure Generate (Buffer : Audio_Buffer_Access;
                               Count  : out Natural);
   procedure Queue_Asynchronously (Self : in out Instance);

   overriding
   procedure Finalize (Self : in out Instance);

private
   use Audio.Queues, Audio.Callbacks, Devices;

   --  TODO: Rename these to desired? Use aggregate constant spec?
   Output_Frequency : constant Positive := 48_000;
   --  Hz
   --  Just a suggestion. Actual frequency supported by system might vary.

   Channel_Count : constant := 2;
   --  Stereo!

   Callback_Frames : constant Positive := 1_024;
   --  ~46ms of samples
   --  How big should the SDL audio callback buffer be. Has to be a power of
   --  two. Bigger values reduce the chances of buffer underruns (which would
   --  cause short audio interruptions) but increase latency.

   type Frame_Buffer_Array is array (1 .. Frame_Buffer_Count)
     of aliased Video_Frame_Sample_Buffer;

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Device : Devices.Device;
      Spec   : Obtained_Spec;

      Dummy_Buffer  : aliased Video_Frame_Sample_Buffer;
      Frame_Buffers : Frame_Buffer_Array;

      Free_Queue : aliased Buffer_Queue;
      Busy_Queue : aliased Buffer_Queue;

      Callback_Context : Callback_Context_Access;
   end record;

   Sample_Format : constant SDL.Audio.Sample_Formats.Sample_Format :=
     SDL.Audio.Sample_Formats.Sample_Format_F32LSB;

   Frame_Size_Bytes : constant Natural :=
     Natural (Sample_Format.Bit_Size) * Channel_Count / System.Storage_Unit;

end Audio.IO;
