with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
With SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;

with Ada.Unchecked_Conversion;

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
   end Create;

   procedure Next_Frame
     (Window : in out Gade_Window_Type;
      G      : in out Gade_Type) is
      Pitch_Pointer : SDL.Video.Pixels.Pitch_Access.Pointer;
      Pixel_Pointer : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

      function ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access is
         new Ada.Unchecked_Conversion
           (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
            Target => RGB32_Display_Buffer_Access);
   begin
      SDL.Video.Textures.Lock
        (Self    => Window.Texture,
         Pixels  => Pixel_Pointer,
         Pitches => Pitch_Pointer);

      Gade.Interfaces.Next_Frame
        (G,
         ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access(Pixel_Pointer));

      SDL.Video.Textures.Unlock(Window.Texture);

      SDL.Video.Renderers.Clear(Window.Renderer);
      SDL.Video.Renderers.Copy(Window.Renderer, Window.Texture);
      SDL.Video.Renderers.Present(Window.Renderer);
   end Next_Frame;

   procedure Finalize (Window : in out Gade_Window_Type) is
   begin
      SDL.Video.Windows.Finalize(Window.Window);
   end Finalize;

end Gade_Window;
