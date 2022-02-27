with SDL.Log; use SDL.Log;

with Interfaces;
with Interfaces.C;

package body Audio.IO is

   procedure Create (Self : aliased out Instance) is
      Device   : Devices.Device renames Self.Device;
      Desired  : Desired_Spec;
      Obtained : Obtained_Spec renames Self.Spec;
   begin
      --  TODO: Consider allocating in heap
      Self.Callback_Context := Create (Self.Free_Queue'Unchecked_Access,
                                       Self.Busy_Queue'Unchecked_Access);

      Desired.Frequency := Interfaces.C.int (Output_Frequency);
      Desired.Format    := Sample_Format;
      Desired.Channels  := Channel_Count;
      Desired.Samples   := Interfaces.Unsigned_16 (Callback_Frames);

      Put_Debug ("Desired - Frequency :" & Desired.Frequency'Img);
      Put_Debug ("Desired - Format/Bit_Size :" & Desired.Format.Bit_Size'Img);
      Put_Debug ("Desired - Format/Float :" & Desired.Format.Float'Img);
      Put_Debug ("Desired - Format/Big_Endian :" & Desired.Format.Endianness'Img);
      Put_Debug ("Desired - Format/Signed :" & Desired.Format.Signed'Img);
      Put_Debug ("Desired - Channels :" & Desired.Channels'Img);
      Put_Debug ("Desired - Samples :" & Desired.Samples'Img);

      Put_Debug ("Opening Default Device");

      Open (Device,
            Callback  => Self.Callback_Context.Callback,
            User_Data => Self.Callback_Context.User_Data,
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

      for Frame_Buffer of Self.Frame_Buffers loop
         Self.Free_Queue.Queue (Frame_Buffer'Unchecked_Access);
      end loop;

      Self.Callback_Context.Set_Spec (Obtained);

      Self.Device.Pause (False);
   end Create;

   procedure Queue_Asynchronously (Self : in out Instance) is
      Buffer      : Bounded_Buffer_Access;
      Frame_Count : Natural;

      Buffer_Was_Available : Boolean;
   begin
      Self.Free_Queue.Dequeue_No_Block (Buffer);
      Buffer_Was_Available := Buffer /= null;

      if not Buffer_Was_Available then
         Put_Debug ("Unavailable free buffer");
         Buffer := Self.Dummy_Buffer'Unrestricted_Access;
      end if;

      Buffer.Set_Length (Buffer.Size);
      Generate (Data_Access (Buffer), Frame_Count);
      Buffer.Set_Length (Frame_Count);

      if Buffer_Was_Available then
         Self.Busy_Queue.Queue (Buffer);
      end if;
   end Queue_Asynchronously;

   overriding
   procedure Finalize (Self : in out Instance) is
   begin
      Put_Debug ("Audio Finalize");
      Self.Device.Pause (True);
      Self.Device.Close;
      --  TODO: ETC, deallocate
   end Finalize;

end Audio.IO;
