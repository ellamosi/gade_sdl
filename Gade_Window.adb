with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;

with Ada.Unchecked_Conversion;
with Ada.Calendar; use Ada.Calendar;
with Ada.Float_Text_IO;

with Gade.Video_Buffer; use Gade.Video_Buffer;

package body Gade_Window is

   procedure Create (Window : out Gade_Window_Type) is
   begin
      SDL.Video.Windows.Makers.Create
        (Win    => Window.Window,
         Title  => "Gade",
         X      => 100,
         Y      => 100,
         Width  => Display_Width * 2,
         Height => Display_Height * 2);

      SDL.Video.Renderers.Makers.Create
        (Rend   => Window.Renderer,
         Window => Window.Window,
         Driver => 1, -- Cocoa
         Flags  => SDL.Video.Renderers.Accelerated);

      SDL.Video.Textures.Makers.Create
        (Tex      => Window.Texture,
         Renderer => Window.Renderer,
         Format   => SDL.Video.Pixel_Formats.Pixel_Format_RGB_888,
         Kind     => SDL.Video.Textures.Streaming,
         Size     => (Display_Width, Display_Height));

      Window.Frame_Count := 0;
      Window.Timer := Ada.Calendar.Clock;
   end Create;

   procedure Next_Frame
     (Window : in out Gade_Window_Type;
      G      : in out Gade_Type)
   is
      pragma Unreferenced (G);

      Pitch_Pointer : SDL.Video.Pixels.Pitch_Access.Pointer;
      Pixel_Pointer : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

--        function ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access is
--           new Ada.Unchecked_Conversion
--             (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
--              Target => RGB32_Display_Buffer_Access);

      Frames_Per_Second : Float;
      Now : Ada.Calendar.Time;
      Title : String := "Gade (XXX.X fps)";
   begin
      SDL.Video.Textures.Lock
        (Self    => Window.Texture,
         Pixels  => Pixel_Pointer,
         Pitches => Pitch_Pointer);

--        Gade.Interfaces.Next_Frame
--          (G,
--           ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access (Pixel_Pointer));

      SDL.Video.Textures.Unlock (Window.Texture);

      SDL.Video.Renderers.Clear (Window.Renderer);
      SDL.Video.Renderers.Copy (Window.Renderer, Window.Texture);
      SDL.Video.Renderers.Present (Window.Renderer);

      Window.Frame_Count := Window.Frame_Count + 1;
      if Window.Frame_Count = 120 then
         Now := Ada.Calendar.Clock;
         Frames_Per_Second := 120.0 / Float (Now - Window.Timer);
         Ada.Float_Text_IO.Put (To   => Title (7 .. 11),
                                Item => Frames_Per_Second,
                                Aft  => 1,
                                Exp  => 0);
         Window.Window.Set_Title (Title);
         Window.Timer := Ada.Calendar.Clock;
         Window.Frame_Count := 0;
      end if;
   end Next_Frame;

   procedure Finalize (Window : in out Gade_Window_Type) is
   begin
      SDL.Video.Windows.Finalize (Window.Window);
   end Finalize;

end Gade_Window;
