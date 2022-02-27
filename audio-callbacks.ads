with Audio.Queues;     use Audio.Queues;
with Audio.Resamplers; use Audio.Resamplers;

private package Audio.Callbacks is
   use Devices;

   type Callback_Context is tagged private;

   type Callback_Context_Access is access all Callback_Context;

   function Create
     (Free_Queue : Buffer_Queue_Access;
      Busy_Queue : Buffer_Queue_Access)
      return Callback_Context_Access;

   procedure Set_Spec
     (Context : in out Callback_Context;
      Spec    : Obtained_Spec);

   function User_Data (Context : aliased in out Callback_Context)
                       return User_Data_Access;

   function Callback (Context : aliased in out Callback_Context)
                      return Audio_Callback;

private
   use Circular_Float_Buffers;

   --  TODO: Should not be here, ends up being dynamic
   Callback_Frames : constant Positive := 1_024;
   --  ~46ms of samples
   --  How big should the SDL audio callback buffer be. Has to be a power of
   --  two. Bigger values reduce the chances of buffer underruns (which would
   --  cause short audio interruptions) but increase latency.

   Max_Delta : constant Float := 0.005;
   --  Max relative frequency variation for the resampler to apply Dynamic
   --  Rate Control with.

   Margin_Size : constant Float := 0.02;
   --  20 Milliseconds
   --  Aim to keep the ring buffer filled between Callback_Frames and
   --  Callback_Frames + Margin_Size (in audio frames).

   type Callback_Context is new Devices.User_Data with record
      Resampler : Resamplers.Resampler;
      --  Audio       : Audio_Access;
      --  TODO: Figure out actually needed size, should be MAX (frame, pull) * 2?
      --  For things to work well, should be able to put all free buffers in ring
      --  otherwise fill level cannot be correctly calculated (cannot measure
      --  queued buffers other than top).
      Ring : Circular_Buffer (Callback_Frames + 900 * Frame_Buffer_Count);

      Busy_Queue : Buffer_Queue_Access;
      Free_Queue : Buffer_Queue_Access;

      Margin_Frames : Positive;
      Margin_Low    : Positive;
      Margin_High   : Positive;
   end record;

   procedure SDL_Callback
     (User_Data : User_Data_Access;
      Buffer    : out Float_Buffers.Data_Container);

   procedure Resample
     (Context : in out Callback_Context;
      Input   : Sample_Buffers.Bounded_Buffer;
      Output  : in out Circular_Buffer);

   function Estimated_Frames (Input : Video_Frame_Sample_Buffer) return Natural;

   procedure Flush
     (Input        : in out Circular_Buffer;
      Output       : in out Float_Buffers.Data_Container;
      Output_Index : in out Positive);

   procedure Write_Silence (Buffer : out Float_Buffers.Data_Container);

   function Fill_Level (Context : Callback_Context) return Float;

end Audio.Callbacks;
