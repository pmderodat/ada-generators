--  Copyright (C) 2014-2022, Pierre-Marie de Rodat
--  SPDX-License-Identifier: Apache-2.0

--  Thin binding for PCL (Portable Coroutine Library)

with Ada.Unchecked_Conversion;
with System;

package PCL is

   pragma Linker_Options ("-lpcl");

   type Coroutine is private;
   type Callback is access procedure (Data : System.Address)
     with Convention => C;

   Null_Coroutine : constant Coroutine;

   function Thread_Init return Integer
     with Import   => True,
     Convention    => C,
     External_Name => "co_thread_init";

   procedure Thread_Cleanup
     with Import   => True,
     Convention    => C,
     External_Name => "co_thread_cleanup";

   function Create
     (Func  : Callback;
      Data  : System.Address;
      Stack : System.Address;
      Size  : Integer) return Coroutine
     with Import   => True,
     Convention    => C,
     External_Name => "co_create";

   procedure Delete (Coro : Coroutine)
     with Import   => True,
     Convention    => C,
     External_Name => "co_delete";

   procedure Call (Coro : Coroutine)
     with Import   => True,
     Convention    => C,
     External_Name => "co_call";

   procedure Resume
     with Import   => True,
     Convention    => C,
     External_Name => "co_resume";

   procedure Exit_To (Coro : Coroutine)
     with Import   => True,
     Convention    => C,
     External_Name => "co_exit_to";

   function Current return Coroutine
     with Import   => True,
     Convention    => C,
     External_Name => "co_current";

   function Get_Data (Coro : Coroutine) return System.Address
     with Import   => True,
     Convention    => C,
     External_Name => "co_get_data";

   function Set_Data
     (Coro : Coroutine;
      Data : System.Address) return System.Address
     with Import   => True,
     Convention    => C,
     External_Name => "co_set_data";

private
   type Coroutine is new System.Address;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Coroutine);

   Null_Coroutine : constant Coroutine := Convert (System.Null_Address);
end PCL;
