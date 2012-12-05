(* $Id: bigint.mli,v 1.2 2011-05-03 15:48:23-07 dmfrank - $ *)
(* Derek Frank, dmfrank@ucsc.edu *)

module Bigint : sig
   type bigint
   val bigint_of_string : string -> bigint
   val string_of_bigint : bigint -> string
   val add : bigint -> bigint -> bigint
   val sub : bigint -> bigint -> bigint
   val mul : bigint -> bigint -> bigint
   val div : bigint -> bigint -> bigint
   val rem : bigint -> bigint -> bigint
   val pow : bigint -> bigint -> bigint
   val zero : bigint
end

