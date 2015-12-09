with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Interfaces;
with XCB;
procedure XCB_Test is
   C : constant XCB.Connection.T := XCB.Connection.Connect;
   Setup : constant XCB.Setup.T := XCB.Setup.Get (C);
   Screen_Iterator : constant XCB.Screen.Iterator_T :=
     XCB.Screen.Root_Iterator (Setup);
   Screen : constant XCB.Screen.T := Screen_Iterator.Data.all;
   Window : constant XCB.Window.T := XCB.Window.Generate_Id (C);
   GC : constant XCB.GC.T := XCB.GC.Generate_Id (C);
   use type Interfaces.Unsigned_32;

   procedure Draw (Point : in XCB.Point.T);
   procedure Draw_Line (From, To : in XCB.Point.T);

   procedure Draw (Point : in XCB.Point.T) is
   begin
      XCB.Point.Poly (C,
                      Drawable => XCB.Drawable_T (Window),
                      Context  => GC,
                      Length   => 1,
                      Points   => (1 => (Point.X, Point.Y)));
      XCB.Connection.Flush (C);
   end Draw;

   procedure Draw_Line (From, To : in XCB.Point.T) is
   begin
      XCB.Line.Poly (C,
                      Drawable => XCB.Drawable_T (Window),
                      Context  => GC,
                      Length   => 2,
                      Points   => (1 => From, 2 => To));
      XCB.Connection.Flush (C);
   end Draw_Line;

   Painting : Boolean := False;
   Button_Pressed : XCB.Event.Button_T;
   Last_Point : XCB.Point.T;

   subtype Meaningful_Color_T is XCB.Pixel.T
     range 0 .. XCB.Pixel.T (2**24 - 1);
   package Random_Color is
     new Ada.Numerics.Discrete_Random (Meaningful_Color_T);
   G : Random_Color.Generator;
begin
   XCB.GC.Create (C,
                  Id => GC,
                  Drawable => XCB.Drawable_T (Screen.Root),
                  Values   =>
                    ((Key => XCB.GC.Foreground,
                      Value => Interfaces.Unsigned_32 (Screen.White_Pixel)),
                     (Key => XCB.GC.Graphics_Exposures,
                      Value => 0)));

   XCB.Window.Create (C,
                      Depth  => 0, -- copy_from_parent
                      Wid    => Window,
                      Parent => Screen.Root,
                      X     => 0,   Y => 0,
                      Width => 150, Height => 150,
                      Border_Width => 10,
                      Class  => XCB.Window.Input_Output,
                      Visual => Screen.Root_Visual,
                      Values => (1 => (Key => XCB.Window.Back_Pixel,
                                       Value => 16#002200#),
                                 2 => XCB.Event.Mask.To_Value
                                   ((XCB.Event.Mask.Key_Press
                                       | XCB.Event.Mask.Button_Press
                                       | XCB.Event.Mask.Button_Release
                                       | XCB.Event.Mask.Button_Motion
                                       | XCB.Event.Mask.Exposure => True,
                                     others => False))));
   XCB.Window.Map (C, Window);
   XCB.Connection.Flush (C);

   loop
      declare
         Next_Event : XCB.Event.T := XCB.Event.Wait_For_Event (C);
         use type XCB.Event.T;
         Exit_Requested : Boolean := False;
         use type XCB.Event.Button_T;
      begin
         exit when Next_Event = null;
         case Next_Event.Response_Type is
            when XCB.Event.Expose =>
               XCB.Rectangle.Poly (C,
                                   Drawable   => XCB.Drawable_T (Window),
                                   Context    => GC,
                                   Length     => 1,
                                   Rectangles => (1 => (X => 0,
                                                        Y => 0,
                                                        Width  => 150,
                                                        Height => 150)));
               XCB.Connection.Flush (C);
            when XCB.Event.Key_Press =>
               Exit_Requested := True;
            when XCB.Event.Button_Press =>
               if not Painting then
                  Painting := True;
                  Button_Pressed := Next_Event.Detail.Detail;
                  Last_Point := (Next_Event.Event_X, Next_Event.Event_Y);
                  Draw (Last_Point);
               end if;
            when XCB.Event.Button_Release =>
               if Button_Pressed = Next_Event.Detail.Detail then
                  Painting := False;
               end if;
            when XCB.Event.Motion_Notify =>
               if Painting then
                  declare
                     Color : constant Meaningful_Color_T :=
                       Random_Color.Random (G);
                  begin
                     XCB.GC.Change
                       (C, GC,
                        Values =>
                          (1 => (Key => XCB.GC.Foreground,
                                 Value =>
                                   Interfaces.Unsigned_32 (Color))));
                     Draw_Line (From => Last_Point,
                                To => (Next_Event.Event_X,
                                       Next_Event.Event_Y));
                  end;
               end if;
            when XCB.Event.Error =>
               Ada.Text_IO.Put_Line ("X protocol error");
            when others =>
               null;
         end case;
         XCB.Event.Free (Next_Event);
         exit when Exit_Requested;
      end;
   end loop;

   XCB.Connection.Disconnect (C);
end XCB_Test;
