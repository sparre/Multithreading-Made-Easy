with Ada.Unchecked_Conversion;

package body XCB is

   type Void_Cookie_T is record
      --  the type returned by "void" XCB functions, translated to procedures
      --  in Ada.
      Sequence_Number : Interfaces.C.unsigned;
   end record;

   package body Connection is
      procedure Flush (C : in Connection.T) is
         function Internal (C : in Connection.T) return Interfaces.C.int;
         pragma Import (C, Internal, "xcb_flush");
         Ignored : constant Interfaces.C.int := Internal (C);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Flush;
   end Connection;

   package body Window is

      procedure Create (C             : in Connection.T;
                        Depth         : in Interfaces.Unsigned_8;
                        Wid           : in T;
                        Parent        : in T;
                        X, Y          : in Interfaces.Integer_16;
                        Width, Height : in Interfaces.Unsigned_16;
                        Border_Width  : in Interfaces.Unsigned_16;
                        Class         : in Class_T;
                        Visual        : in Visualid.T;
                        Values        : in Value_List.T := (1 .. 0 => <>)) is
         function Internal (C             : in Connection.T;
                            Depth         : in Interfaces.Unsigned_8;
                            Wid           : in T;
                            Parent        : in T;
                            X, Y          : in Interfaces.Integer_16;
                            Width, Height : in Interfaces.Unsigned_16;
                            Border_Width  : in Interfaces.Unsigned_16;
                            Class         : in Class_T;
                            Visual        : in Visualid.T;
                            Mask          : in Interfaces.Unsigned_32;
                            Values        : in Value_List.Low_Level_T
                              := (1 .. 0 => <>))
                           return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_create_window");
         Ignored : constant Void_Cookie_T :=
           Internal (C,
                     Depth,
                     Wid,
                     Parent,
                     X, Y, Width, Height,
                     Border_Width,
                     Class,
                     Visual,
                     Mask   => Value_List.Mask_Of (Values),
                     Values => Value_List.To_C (Values));
         pragma Unreferenced (Ignored);
      begin
         null;
      end Create;


      procedure Map (C : in Connection.T; W : in T) is
         function Internal (C : in Connection.T; W : in T)
                           return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_map_window");
         Ignored : constant Void_Cookie_T := Internal (C, W);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Map;

   end Window;

   package body GC is

      procedure Create (C        : in Connection.T;
                        Id       : in T;
                        Drawable : in Drawable_T;
                        Values   : in Value_List.T := (1 .. 0 => <>))
      is
         function Internal (C          : in Connection.T;
                            Id         : in T;
                            Drawable   : in Drawable_T;
                            Value_Mask : in Interfaces.Unsigned_32;
                            Values     : in Value_List.Low_Level_T)
                           return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_create_gc");
         Ignored : constant Void_Cookie_T :=
           Internal (C, Id, Drawable,
                     Value_List.Mask_Of (Values),
                     Value_List.To_C (Values));
         pragma Unreferenced (Ignored);
      begin
         null;
      end Create;

      procedure Change (C          : in Connection.T;
                        Id         : in T;
                        Values     : in Value_List.T := (1 .. 0 => <>)) is
         function Internal (C          : in Connection.T;
                            Id         : in T;
                            Value_Mask : in Interfaces.Unsigned_32;
                            Values     : in Value_List.Low_Level_T)
                           return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_change_gc");
         Ignored : constant Void_Cookie_T :=
           Internal (C, Id,
                     Value_List.Mask_Of (Values),
                     Value_List.To_C (Values));
         pragma Unreferenced (Ignored);
      begin
         null;
      end Change;
   end GC;


   package body Point is
      procedure Poly (C        : in Connection.T;
                      Mode     : in Coordinate_Mode.T
                        := Coordinate_Mode.Origin;
                      Drawable : in Drawable_T;
                      Context  : in GC.T;
                      Length   : in Interfaces.Unsigned_32;
                      Points   : in Array_T) is
         function Internal (C        : in Connection.T;
                            Mode     : in Coordinate_Mode.T;
                            Drawable : in Drawable_T;
                            Context  : in GC.T;
                            Length   : in Interfaces.Unsigned_32;
                            Points   : in Array_T) return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_poly_point");
         Ignored : constant Void_Cookie_T := Internal (C,
                                                       Mode,
                                                       Drawable,
                                                       Context,
                                                       Length,
                                                       Points);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Poly;
   end Point;


   package body Line is
      procedure Poly (C        : in Connection.T;
                      Mode     : in Coordinate_Mode.T
                        := Coordinate_Mode.Origin;
                      Drawable : in Drawable_T;
                      Context  : in GC.T;
                      Length   : in Interfaces.Unsigned_32;
                      Points   : in Point.Array_T) is
         function Internal (C        : in Connection.T;
                            Mode     : in Coordinate_Mode.T;
                            Drawable : in Drawable_T;
                            Context  : in GC.T;
                            Length   : in Interfaces.Unsigned_32;
                            Points   : in Point.Array_T) return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_poly_line");
         Ignored : constant Void_Cookie_T := Internal (C,
                                                       Mode,
                                                       Drawable,
                                                       Context,
                                                       Length,
                                                       Points);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Poly;
   end Line;


   package body Rectangle is

      procedure Poly (C          : in Connection.T;
                      Drawable   : in Drawable_T;
                      Context    : in GC.T;
                      Length     : in Interfaces.Unsigned_32;
                      Rectangles : in Array_T) is
         function Internal (C          : in Connection.T;
                            Drawable   : in Drawable_T;
                            Context    : in GC.T;
                            Length     : in Interfaces.Unsigned_32;
                            Rectangles : in Array_T) return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_poly_rectangle");
         Ignored : constant Void_Cookie_T := Internal (C,
                                                       Drawable,
                                                       Context,
                                                       Length,
                                                       Rectangles);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Poly;


      procedure Poly_Fill (C             : in Connection.T;
                      Drawable   : in Drawable_T;
                      Context    : in GC.T;
                      Length     : in Interfaces.Unsigned_32;
                      Rectangles : in Array_T) is
         function Internal (C          : in Connection.T;
                            Drawable   : in Drawable_T;
                            Context    : in GC.T;
                            Length     : in Interfaces.Unsigned_32;
                            Rectangles : in Array_T) return Void_Cookie_T;
         pragma Import (C, Internal, "xcb_poly_fill_rectangle");
         Ignored : constant Void_Cookie_T := Internal (C,
                                                       Drawable,
                                                       Context,
                                                       Length,
                                                       Rectangles);
         pragma Unreferenced (Ignored);
      begin
         null;
      end Poly_Fill;

   end Rectangle;


   package body Event is
      package body Mask is
         function To_Value (Mask : in T)
                           return XCB.Window.Value_List.Key_Value_Pair_T is
            function To_Unsigned_32 is
               new Ada.Unchecked_Conversion (T, Interfaces.Unsigned_32);
         begin
            return (Key => XCB.Window.Event_Mask,
                    Value => To_Unsigned_32 (Mask));
         end To_Value;
      end Mask;
   end Event;

end XCB;
