with Ada.Finalization;
private with System;

package XCB.Keysyms is
   pragma Linker_Options ("-lxcb-keysyms");

   type Keysym_T is new Interfaces.Unsigned_32;

   type T (<>) is limited private;
   --  The entire array of all keysyms defined in the server; used to translate
   --  keycodes in Key_Press/Key_Release events to keysyms.

   function Get (C : in XCB.Connection.T) return T;

   subtype Key_Press_Event_T is
     XCB.Event.Response_T (Sent => False,
                           Response_Type => XCB.Event.Key_Press);
   function From_Key_Press (In_Symbols : in     T;
                            Event      : access Key_Press_Event_T;
                            Col        : in     Integer)
                           return Keysym_T;

   subtype Key_Release_Event_T is
     XCB.Event.Response_T (Sent => False,
                           Response_Type => XCB.Event.Key_Release);
   function From_Key_Release (In_Symbols : in     T;
                              Event      : access Key_Release_Event_T;
                              Col        : in     Integer)
                             return Keysym_T;

private
   type T is new Ada.Finalization.Limited_Controlled with record
      Symbols : System.Address;
   end record;
   overriding procedure Finalize (K : in out T);
end XCB.Keysyms;
