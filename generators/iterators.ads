--  Interface for iterations (because I'm too lazy to implement an Ada 2012
--  iterator...).

generic
   type T (<>) is private;
   type Cursor_Type is private;

package Iterators is

   type Iterator is limited interface with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element);

   function First (I : in out Iterator) return Cursor_Type is abstract;
   function Next (I : in out Iterator; C : Cursor_Type) return Cursor_Type
                  is abstract;
   function Has_Element (I : in out Iterator; C : Cursor_Type) return Boolean
                         is abstract;
   function Element (I : in out Iterator; C : Cursor_Type) return T
                     is abstract;

end Iterators;
