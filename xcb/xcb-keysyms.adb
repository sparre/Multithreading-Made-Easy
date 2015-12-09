package body XCB.Keysyms is

   function Get (C : in XCB.Connection.T) return T is
      function Allocate (C : in XCB.Connection.T) return System.Address;
      pragma Import (C, Allocate, "xcb_key_symbols_alloc");
   begin
      return (Ada.Finalization.Limited_Controlled with
                Symbols => Allocate (C));
   end Get;

   function From_Key_Press (In_Symbols : in     T;
                            Event      : access Key_Press_Event_T;
                            Col        : in     Integer)
                           return Keysym_T is
      function Lookup (In_Symbols : in     System.Address;
                       Event      : access Key_Press_Event_T;
                       Col        : in     Integer)
                      return Keysym_T;
      pragma Import (C, Lookup, "xcb_key_press_lookup_keysym");
   begin
      return Lookup (In_Symbols.Symbols, Event, Col);
   end From_Key_Press;

   function From_Key_Release (In_Symbols : in     T;
                              Event      : access Key_Release_Event_T;
                              Col        : in     Integer)
                             return Keysym_T is
      function Lookup (In_Symbols : in     System.Address;
                       Event      : access Key_Release_Event_T;
                       Col        : in     Integer)
                   return Keysym_T;
      pragma Import (C, Lookup, "xcb_key_release_lookup_keysym");
   begin
      return Lookup (In_Symbols.Symbols, Event, Col);
   end From_Key_Release;

   overriding procedure Finalize (K : in out T) is
      procedure Free (Symbols : in System.Address);
      pragma Import (C, Free, "xcb_key_symbols_free");
   begin
      Free (K.Symbols);
   end Finalize;
end XCB.Keysyms;
