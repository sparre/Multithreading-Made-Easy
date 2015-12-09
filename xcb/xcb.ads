with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;

with XCB_Value_List_G;

package XCB is

   pragma Linker_Options ("-lxcb");

   package Connection is
      type T is private;

      function Connect
        (Display_Name  : in     Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         Screen_Number : access Integer := null)
        return Connection.T;
      pragma Import (C, Connect, "xcb_connect");

      procedure Disconnect (C : in Connection.T);
      pragma Import (C, Disconnect, "xcb_disconnect");

      procedure Flush (C : in Connection.T)
        with Inline;
   private
      type T is access Integer; -- opaque type
   end Connection;


   package Setup is
      type T is private;
      function Get (C : in Connection.T) return T;
      pragma Import (C, Get, "xcb_get_setup");
   private
      type T is access Integer; -- opaque type
   end Setup;


   package Visualid is
      type T is private;
   private
      type T is access Integer; -- opaque type
   end Visualid;

   type Id_T is new Interfaces.Unsigned_32;
   type Drawable_T is new Id_T;

   package Window is
      type T is new Drawable_T;
      function Generate_Id (C : in Connection.T) return T;
      pragma Import (C, Generate_Id, "xcb_generate_id");

      type Class_T is (Copy_From_Parent, Input_Output, Input_Only);
      pragma Convention (C, Class_T);

      type Key_T is (Back_Pixmap,
                     Back_Pixel,
                     Border_Pixmap,
                     Border_Pixel,
                     Bit_Gravity,
                     Win_Gravity,
                     Backing_Store,
                     Backing_Planes,
                     Backing_Pixel,
                     Override_Redirect,
                     Save_Under,
                     Event_Mask,
                     Dont_Propagate,
                     Colormap,
                     Cursor);
      package Value_List is new XCB_Value_List_G (Key_T);

      procedure Create (C             : in Connection.T;
                        Depth         : in Interfaces.Unsigned_8;
                        Wid           : in T;
                        Parent        : in T;
                        X, Y          : in Interfaces.Integer_16;
                        Width, Height : in Interfaces.Unsigned_16;
                        Border_Width  : in Interfaces.Unsigned_16;
                        Class         : in Class_T;
                        Visual        : in Visualid.T;
                        Values        : in Value_List.T := (1 .. 0 => <>));

      procedure Map (C : in Connection.T; W : in T);
   end Window;


   package Colormap is
      type T is new Id_T;
   end Colormap;


   package Pixel is
      type T is new Interfaces.Unsigned_32;
   end Pixel;


   package Screen is

      type T is record
        Root                  : Window.T;
        Default_Colormap      : Colormap.T;
        White_Pixel           : Pixel.T;
        Black_Pixel           : Pixel.T;
        Current_Input_Masks   : Interfaces.Unsigned_32;
        Width_In_Pixels       : Interfaces.Unsigned_16;
        Height_In_Pixels      : Interfaces.Unsigned_16;
        Width_In_Millimeters  : Interfaces.Unsigned_16;
        Height_In_Millimeters : Interfaces.Unsigned_16;
        Min_Installed_Maps    : Interfaces.Unsigned_16;
        Max_Installed_Maps    : Interfaces.Unsigned_16;
        Root_Visual           : Visualid.T;
        Backing_Stores        : Interfaces.Unsigned_8;
        Save_Unders           : Interfaces.Unsigned_8;
        Root_Depth            : Interfaces.Unsigned_8;
        Allowed_Depths_Len    : Interfaces.Unsigned_8;
      end record;
      pragma Convention (C, T);

      type Iterator_T is record
         Data      : access T;
         Remainder : Integer;
         Index     : Integer;
      end record;
      pragma Convention (C, Iterator_T);

      function Root_Iterator (S : in Setup.T) return Iterator_T;
      pragma Import (C, Root_Iterator, "xcb_setup_roots_iterator");

      procedure Next (Iter : in out Iterator_T);
      pragma Import (C, Next, "xcb_screen_next");
   private
      type Setup_T is access Integer;
   end Screen;


   package GC is
      type T is new Id_T;
      function Generate_Id (C : in Connection.T) return T;
      pragma Import (C, Generate_Id, "xcb_generate_id");

      type Key_T is (Function_Mask,
                     Plane_Mask,
                     Foreground,
                     Background,
                     Line_Width,
                     Line_Style,
                     Cap_Style,
                     Join_Style,
                     Fill_Style,
                     Fill_Rule,
                     Tile,
                     Stipple,
                     Tile_Stipple_Origin_X,
                     Tile_Stipple_Origin_Y,
                     Font,
                     Subwindow_Mode,
                     Graphics_Exposures,
                     Clip_Origin_X,
                     Clip_Origin_Y,
                     Clip_Mask,
                     Dash_Offset,
                     Dash_List,
                     Arc_Mode);

      package Value_List is new XCB_Value_List_G (Key_T);

      procedure Create (C          : in Connection.T;
                        Id         : in T;
                        Drawable   : in Drawable_T;
                        Values     : in Value_List.T := (1 .. 0 => <>));

      procedure Change (C          : in Connection.T;
                        Id         : in T;
                        Values     : in Value_List.T := (1 .. 0 => <>));
   end GC;

   package Coordinate_Mode is
      type T is (Origin, Previous);
      for T use (Origin => 0, Previous => 1);
      pragma Convention (C, T);
   end Coordinate_Mode;


   package Point is
      type T is record
         X, Y : Interfaces.Integer_16;
      end record;
      type Array_T is array (Interfaces.Unsigned_32 range <>) of T;

      procedure Poly (C        : in Connection.T;
                      Mode     : in Coordinate_Mode.T
                        := Coordinate_Mode.Origin;
                      Drawable : in Drawable_T;
                      Context  : in GC.T;
                      Length   : in Interfaces.Unsigned_32;
                      Points   : in Array_T);
   end Point;


   package Line is
      type Line_Style_T is (Solid, On_Off_Dash, Double_Dash);
      type Cap_Style_T is (Not_Last, Butt, Round, Projecting);
      type Join_Style_T is (Miter, Round, Bevel);
      procedure Poly (C        : in Connection.T;
                      Mode     : in Coordinate_Mode.T
                        := Coordinate_Mode.Origin;
                      Drawable : in Drawable_T;
                      Context  : in GC.T;
                      Length   : in Interfaces.Unsigned_32;
                      Points   : in Point.Array_T);
   end Line;


   package Rectangle is
      type T is record
         X, Y : Interfaces.Integer_16;
         Width, Height : Interfaces.Unsigned_16;
      end record;
      type Array_T is array (Interfaces.Unsigned_32 range <>) of T;

      procedure Poly (C          : in Connection.T;
                      Drawable   : in Drawable_T;
                      Context    : in GC.T;
                      Length     : in Interfaces.Unsigned_32;
                      Rectangles : in Array_T);
      procedure Poly_Fill (C          : in Connection.T;
                           Drawable   : in Drawable_T;
                           Context    : in GC.T;
                           Length     : in Interfaces.Unsigned_32;
                           Rectangles : in Array_T);
   end Rectangle;

   package Event is

      type Response_Type_T is (Error,
                               Reply,
                               Key_Press,
                               Key_Release,
                               Button_Press,
                               Button_Release,
                               Motion_Notify,
                               Enter_Notify,
                               Leave_Notify,
                               Focus_In,
                               Focus_Out,
                               Keymap_Notify,
                               Expose,
                               Graphics_Exposure,
                               No_Exposure,
                               Visibility_Notify,
                               Create_Notify,
                               Destroy_Notify,
                               Unmap_Notify,
                               Map_Notify,
                               Map_Request,
                               Reparent_Notify,
                               Configure_Notify,
                               Configure_Request,
                               Gravity_Notify,
                               Resize_Request,
                               Circulate_Notify,
                               Circulate_Request,
                               Property_Notify,
                               Selection_Clear,
                               Selection_Request,
                               Selection_Notify,
                               Colormap_Notify,
                               Client_Message,
                               Mapping_Notify);

      type Button_T is new Interfaces.Unsigned_8;

      type Detail_T (Response_Type : Response_Type_T := Reply) is record
         case Response_Type is
            when Key_Press | Key_Release =>
               Keycode : Interfaces.Unsigned_8;
            when Button_Press | Button_Release | Motion_Notify =>
               Detail  : Button_T;
            when others =>
               Padding : Interfaces.Unsigned_8;
         end case;
      end record
         with Unchecked_Union;

      type Response_T (Sent : Boolean; Response_Type : Response_Type_T) is
         record
            Detail   : Detail_T (Response_Type);
            Sequence : Interfaces.Unsigned_16;
            case Response_Type is
               when Key_Press
                 | Key_Release
                 | Button_Press
                 | Button_Release
                 | Motion_Notify =>
                  Time          : Interfaces.Unsigned_32;
                  Root          : XCB.Window.T;
                  Event         : XCB.Window.T;
                  Child         : XCB.Window.T;
                  Root_X        : Interfaces.Integer_16;
                  Root_Y        : Interfaces.Integer_16;
                  Event_X       : Interfaces.Integer_16;
                  Event_Y       : Interfaces.Integer_16;
                  State         : Interfaces.Unsigned_16;
                  Same_Screen   : Interfaces.Unsigned_8;
                  Pad_Button    : Interfaces.Unsigned_8;
               when Expose =>
                  Expose_Event_Window         : XCB.Window.T;
                  Expose_X, Expose_Y          : Interfaces.Unsigned_16;
                  Expose_Width, Expose_Height : Interfaces.Unsigned_16;
                  Count                       : Interfaces.Unsigned_16;
                  Expose_Pad                  : Interfaces.Unsigned_16;
               when Configure_Notify =>
                  Configure_Event_Window      : XCB.Window.T;
                  Configured_Window           : XCB.Window.T;
                  Above_Sibling               : XCB.Window.T;
                  X, Y                        : Interfaces.Integer_16;
                  Width, Height, Border_Width : Interfaces.Unsigned_16;
                  Override_Redirect           : Interfaces.Unsigned_8;
                  Pad_Configure               : Interfaces.Unsigned_8;
               when others =>
                  Pad_Others_2  : String (1 .. 4*6);
                  Full_Sequence : Interfaces.Unsigned_32;
            end case;
         end record;
      for Response_T use record
         Sent          at 0 range 7 .. 7;
         Response_Type at 0 range 0 .. 6;
      end record;

      type T is access Response_T;
      procedure Free is new Ada.Unchecked_Deallocation (Response_T, T);

      function Wait_For_Event (C : in Connection.T) return T;
      pragma Import (C, Wait_For_Event, "xcb_wait_for_event");

      package Mask is

         type Bit_T is (Key_Press,
                        Key_Release,
                        Button_Press,
                        Button_Release,
                        Enter_Window,
                        Leave_Window,
                        Pointer_Motion,
                        Pointer_Motion_Hint,
                        Button_1_Motion,
                        Button_2_Motion,
                        Button_3_Motion,
                        Button_4_Motion,
                        Button_5_Motion,
                        Button_Motion,
                        Keymap_State,
                        Exposure,
                        Visibility_Change,
                        Structure_Notify,
                        Resize_Redirect,
                        Substructure_Notify,
                        Substructure_Redirect,
                        Focus_Change,
                        Property_Change,
                        Color_Map_Change,
                        Owner_Grab_Button);
         type T is array (Bit_T) of Boolean;
         pragma Pack (T);
         for T'Size use 32;

         function To_Value (Mask : in T)
                           return XCB.Window.Value_List.Key_Value_Pair_T;
         --  Pass the event mask as one of the Key_ValuePairs to
         --  XCB.Window.Create like so:
         --
         --  XCB.Window.Create
         --    (...,
         --     Values => (1 => XCB.Event.Mask.To_Value
         --                       ((XCB.Event.Mask.Button_Release => True,
         --                        others => False))));
      end Mask;
   end Event;
end XCB;
