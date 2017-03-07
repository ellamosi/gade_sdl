with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Command_line;     use Ada.Command_Line;
with Ada.Calendar;         use Ada.Calendar;
with Ada.Finalization;

with Gade.Interfaces;      use Gade.Interfaces;
with Gade.Video_Buffer;    use Gade.Video_Buffer;
with Gade.Input_Reader;    use Gade.Input_Reader;

with Gade_Window;          use Gade_Window;

with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;

procedure Gade_Main is

   Finished : Boolean;
   Window   : Gade_Window_Type;
   G        : Gade_Type;
   Event    : SDL.Events.Events.Events;


   Last_Frame_At  : Time;
   Frame_Duration : constant Duration := 1.0/60.0;

   procedure Next_Frame is
   begin
      Next_Frame (Window, G);
   exception
      -- This seems to crash SDL if an exception is raised and captured at
      -- lower level?!
      when E:
         others =>
            Put_Line (Exception_Information(E));
            Finished := True;
   end Next_Frame;

   type Input_Reader_Type is new Gade.Input_Reader.Input_Reader_Type with null record;

   Buttons : Input_State;

   overriding function Read_Input (Reader : Input_Reader_Type) return Input_State is
   begin
      return Buttons;
   end Read_Input;

   Reader : aliased Input_Reader_Type;

   procedure Set_Button_Pressed
     (Event : SDL.Events.Events.Events; Pressed: Boolean) is
   begin
      case Event.Keyboard.Key_Sym.Scan_Code is
         when SDL.Events.Keyboards.Scan_Code_Z =>
            Buttons.A := Pressed;
         when SDL.Events.Keyboards.Scan_Code_X =>
            Buttons.B := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Left =>
            Buttons.Left := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Right =>
            Buttons.Right := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Up =>
            Buttons.Up := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Down =>
            Buttons.Down := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Return =>
            Buttons.Start := Pressed;
         when SDL.Events.Keyboards.Scan_Code_Backspace =>
            Buttons.Sel := Pressed;
         when others => null;
      end case;
   end Set_Button_Pressed;

   function ROM_Filename return String is
   begin
      if Argument_Count < 1 then
         Put_Line("No ROM file path was provided.");
         raise Program_Error;
      end if;
      return Argument(1);
   end ROM_Filename;

   package Controlled_Type_Test is
      type Access_Type is limited private;

      procedure Test(Access_Value : out Access_type);
   private
      type Actual_Type;
      type Access_Type is access Actual_Type;
   end Controlled_Type_Test;

   package body Controlled_Type_Test is
      type Actual_Type is new Ada.Finalization.Controlled with record
        n : Integer;
      end record;

      overriding procedure Initialize (This: in out Actual_Type);
      overriding procedure Adjust     (This: in out Actual_Type);
      overriding procedure Finalize   (This: in out Actual_Type);

      overriding procedure Initialize (This: in out Actual_Type) is
      begin
         Put_Line("Initialize");
         This.n := 100;
      end Initialize;

      overriding procedure Adjust     (This: in out Actual_Type) is
      begin
         Put_Line("Adjust");
      end Adjust;

      overriding procedure Finalize (This: in out Actual_Type) is
      begin
         Put_Line("Finalize");
      end Finalize;

      procedure Test(Access_Value : out Access_type) is
      begin
         Access_Value := new Actual_Type;
         Access_Value.all.n := 5;
      end Test;
   end Controlled_Type_Test;

   Access_Value : Controlled_Type_Test.Access_Type;

begin
   if SDL.Initialise = True then
      Buttons := (others => False);
      Create(Window);
      Create(G);
      Load_ROM(G, ROM_Filename);
      Set_Input_Reader(G, Reader'Access);

      Finished := False;
      while not Finished loop
         Last_Frame_At := Clock;
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Keyboards.Key_Down =>
                  Set_Button_Pressed(Event, True);
               when SDL.Events.Keyboards.Key_Up =>
                  Set_Button_Pressed(Event, False);
               when SDL.Events.Quit =>
                  Finished := True;
               when others =>
                  null;
            end case;
         end loop;
         Next_Frame;
         --delay until Last_Frame_At + Frame_Duration;
      end loop;

      Finalize(Window);
      SDL.Finalise;
   end if;
end Gade_Main;
