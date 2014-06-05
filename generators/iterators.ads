generic
   type T (<>) is private;
package Iterators is

   type Iterator is limited interface;

   function Has_Next (I : in out Iterator) return Boolean is abstract;
   function Next (I : in out Iterator) return T is abstract;

end Iterators;
