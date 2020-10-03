with Ada.Text_IO;          use Ada.Text_IO;
--  with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Calendar;         use Ada.Calendar;
with GNAT.Command_Line;    use GNAT.Command_Line;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with Ada.Streams.Stream_IO;

with Gade.Interfaces;      use Gade.Interfaces;
with Gade.Input_Reader;    use Gade.Input_Reader;

with Gade_Window;          use Gade_Window;
with Gade_Audio;           use Gade_Audio;

with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
--
--  with GNAT.Traceback;
--  with GNAT.Traceback.Symbolic;

with Soundio; use Soundio;

procedure Gade_Main is
   use Ada;
   --  use GNAT.Traceback;
   --  use GNAT.Traceback.Symbolic;

   Config : Command_Line_Configuration;

   Finished       : Boolean;
   Event          : SDL.Events.Events.Events;
   Unlimited_FPS  : aliased Boolean := False;
   Stream_Context : aliased Stream_Context_Type;

   --  Sound IO vars
   Sound_IO             : constant access Soundio.SoundIo := Create;
   Default_Device_Index : int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Err                  : SoundIo_Error;
   --  End Sound IO vars

   Output_File : Ada.Streams.Stream_IO.File_Type;

   Last_Frame_At  : Time;
   pragma Warnings (Off, "static fixed-point value is not a multiple of Small");
   Frame_Duration : constant Duration := Duration (1.0 / 60.0);
   pragma Warnings (On, "static fixed-point value is not a multiple of Small");

--     procedure Next_Frame;
--
--     procedure Next_Frame is
--     begin
--        Next_Frame (Stream_Context.Window, Stream_Context.G);
--     exception
--        --  This seems to crash SDL if an exception is raised and captured at
--        --  lower level?!
--        when E :
--           others =>
--              Put_Line (Exception_Information (E));
--              Finished := True;
--     end Next_Frame;

   type Input_Reader_Type is new Gade.Input_Reader.Input_Reader_Type with null record;

   Buttons : Input_State;

   overriding function Read_Input (Reader : Input_Reader_Type) return Input_State;

   overriding
   function Read_Input (Reader : Input_Reader_Type) return Input_State is
      pragma Unreferenced (Reader);
   begin
      return Buttons;
   end Read_Input;

   Reader : aliased Input_Reader_Type;

   procedure Set_Button_Pressed  (Event : SDL.Events.Events.Events; Pressed : Boolean);
   procedure Set_Button_Pressed
     (Event : SDL.Events.Events.Events; Pressed : Boolean) is
   begin
      case Event.Keyboard.Key_Sym.Scan_Code is
         when SDL.Events.Keyboards.Scan_Code_Z =>
            Buttons.A := Pressed;
         when SDL.Events.Keyboards.Scan_Code_X =>
            Buttons.B := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Left =>
            Buttons.LEFT := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Right =>
            Buttons.RIGHT := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Up =>
            Buttons.UP := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Down =>
            Buttons.DOWN := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Return =>
            Buttons.START := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Backspace =>
            Buttons.SEL := Pressed;
         when others => null;
      end case;
   end Set_Button_Pressed;

   function ROM_Filename return String;
   function ROM_Filename return String is
      Path : constant String := Get_Argument;
   begin
      if Path = "" then
         Put_Line ("No ROM file path was provided!");
         raise Program_Error;
      end if;
      return Path;
   end ROM_Filename;

begin
   Define_Switch (Config, Unlimited_FPS'Access, "-u", "Unlimited framerate");
   Getopt (Config);

   if SDL.Initialise then
      Buttons := (others => False);
      Create (Stream_Context.Window);

      Put_Line ("Initializing libgade");
      Create (Stream_Context.G);
      Put_Line ("Loading ROM");
      Load_ROM (Stream_Context.G, ROM_Filename);
      Put_Line ("Setting up input handling");
      Set_Input_Reader (Stream_Context.G, Reader'Access);

      Ada.Streams.Stream_IO.Create
        (Output_File, Ada.Streams.Stream_IO.Out_File, "gade_audio.raw");
      Stream_Context.File_Stream := Ada.Streams.Stream_IO.Stream (Output_File);

      --  Sound IO setup
      Put_Line ("Setting up SoundIO connection");
      Err := Connect (Sound_IO);
      Put_Line (Err'Img);

      Put_Line ("Flushing events...");
      Flush_Events (Sound_IO);

      Put_Line ("Getting default output device index");
      Default_Device_Index := Default_Output_Device_Index (Sound_IO);
      Put_Line ("Getting output device");
      Device := Get_Output_Device (Sound_IO, Default_Device_Index);
      Put_Line (To_Ada (Interfaces.C.Strings.Value (Device.name)));
      Put_Line ("Creating output stream");
      Out_Stream := Outstream_Create (Device);
      Put_Line ("Setting up format");
      Out_Stream.Format := Format_Float32NE; -- Format_S32LE; --  Format_S16LE;
      --  Put_Line ("Setting up write callback");
      Out_Stream.Write_Callback := Write_Callback'Access;
      Put_Line ("Setting up stream context");
      Out_Stream.User_Data := Stream_Context'Address;

      Put_Line ("Operning Stream");
      Err := Outstream_Open (Out_Stream);
      Put_Line (Err'Img);

      Put_Line ("Starting Stream");
      Err := Outstream_Start (Out_Stream);
      Put_Line (Err'Img);
      --  End Sound IO setup

      Flush_Events (Sound_IO);

      Finished := False;
      while not Finished loop
         Last_Frame_At := Clock;
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Keyboards.Key_Down =>
                  Set_Button_Pressed (Event, True);
               when SDL.Events.Keyboards.Key_Up =>
                  Set_Button_Pressed (Event, False);
               when SDL.Events.Quit =>
                  Finished := True;
               when others =>
                  null;
            end case;
         end loop;

         --  Run_For ();
         --  Next_Frame;

         --  Wait_Events (IO);
         if not Unlimited_FPS then
            delay until Last_Frame_At + Frame_Duration;
         end if;
      end loop;

      --  TODO: Need to finalize stream first!

      Finalize (Stream_Context.Window);
      Finalize (Stream_Context.G);
      SDL.Finalise;
   end if;
--  --  This seems to actually hide the exceptions, got to test it further
--  exception
--     when E : others =>
--        Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gade_Main;
