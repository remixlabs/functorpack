module Types : sig

  type bigstring =
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

  module type MESSAGE_EXTRACTOR = sig
    type message
    type fragment

    val create : unit -> fragment
    val extract : fragment -> message

    val read_nil : fragment -> fragment
    val read_bool : bool -> fragment -> fragment
    val read_fixnum : int -> fragment -> fragment
    val read_uint8 : int -> fragment -> fragment
    val read_uint16 : int -> fragment -> fragment
    val read_uint32 : int32 -> fragment -> fragment
    val read_uint64 : int64 -> fragment -> fragment
    val read_int8 : int -> fragment -> fragment
    val read_int16 : int -> fragment -> fragment
    val read_int32 : int32 -> fragment -> fragment
    val read_int64 : int64 -> fragment -> fragment
    val read_float32 : float -> fragment -> fragment
    val read_float64 : float -> fragment -> fragment
    val read_fixstr : bytes -> int -> int -> fragment -> fragment
    val read_str8 : bytes -> int -> int -> fragment -> fragment
    val read_str16 : bytes -> int -> int -> fragment -> fragment
    val read_str32 : bytes -> int -> int -> fragment -> fragment
    val read_bin8 : bytes -> int -> int -> fragment -> fragment
    val read_bin16 : bytes -> int -> int -> fragment -> fragment
    val read_bin32 : bytes -> int -> int -> fragment -> fragment
    val read_fixarray_start : int -> fragment -> fragment
    val read_fixarray_end : int -> fragment -> fragment
    val read_array16_start : int -> fragment -> fragment
    val read_array16_end : int -> fragment -> fragment
    val read_array32_start : int -> fragment -> fragment
    val read_array32_end : int -> fragment -> fragment
    val read_fixmap_start : int -> fragment -> fragment
    val read_fixmap_next : fragment -> fragment
    val read_fixmap_end : int -> fragment -> fragment
    val read_map16_start : int -> fragment -> fragment
    val read_map16_next : fragment -> fragment
    val read_map16_end : int -> fragment -> fragment
    val read_map32_start : int -> fragment -> fragment
    val read_map32_next : fragment -> fragment
    val read_map32_end : int -> fragment -> fragment
    val read_fixext1 : int -> int -> bytes -> fragment -> fragment
    val read_fixext2 : int -> int -> bytes -> fragment -> fragment
    val read_fixext4 : int -> int32 -> bytes -> fragment -> fragment
    val read_fixext8 : int -> int64 -> bytes -> fragment -> fragment
    val read_fixext16 : int -> int64 -> int64 -> bytes -> fragment -> fragment
    val read_ext8 : int -> bytes -> int -> int -> fragment -> fragment
    val read_ext16 : int -> bytes -> int -> int -> fragment -> fragment
    val read_ext32 : int -> bytes -> int -> int -> fragment -> fragment
  end

  module type MESSAGE_COMPOSER = sig
    type message
    type fragment

    val create : unit -> fragment
    val compose : fragment -> message

    val write_nil : fragment -> fragment
    val write_bool : bool -> fragment -> fragment
    val write_fixnum : int -> fragment -> fragment
    val write_uint8 : int -> fragment -> fragment
    val write_uint16 : int -> fragment -> fragment
    val write_uint32 : int32 -> fragment -> fragment
    val write_uint64 : int64 -> fragment -> fragment
    val write_int8 : int -> fragment -> fragment
    val write_int16 : int -> fragment -> fragment
    val write_int32 : int32 -> fragment -> fragment
    val write_int64 : int64 -> fragment -> fragment
    val write_int_best : int -> fragment -> fragment
    val write_int64_best : int64 -> fragment -> fragment
    val write_float32 : float -> fragment -> fragment
    val write_float64 : float -> fragment -> fragment
    val write_fixstr : string -> int -> int -> fragment -> fragment
    val write_str8 : string -> int -> int -> fragment -> fragment
    val write_str16 : string -> int -> int -> fragment -> fragment
    val write_str32 : string -> int -> int -> fragment -> fragment
    val write_str_best : string -> int -> int -> fragment -> fragment
    val write_bin8 : string -> int -> int -> fragment -> fragment
    val write_bin16 : string -> int -> int -> fragment -> fragment
    val write_bin32 : string -> int -> int -> fragment -> fragment
    val write_bin_best : string -> int -> int -> fragment -> fragment
    val write_bin32_big : bigstring -> int -> int -> fragment -> fragment
    val write_bin_best_big : bigstring -> int -> int -> fragment -> fragment
    val write_fixarray_start : int -> fragment -> fragment
    val write_fixarray_end : int -> fragment -> fragment
    val write_array16_start : int -> fragment -> fragment
    val write_array16_end : int -> fragment -> fragment
    val write_array32_start : int -> fragment -> fragment
    val write_array32_end : int -> fragment -> fragment
    val write_array_best : int ->
                           (int -> fragment -> fragment) *
                             (int -> fragment -> fragment)
    val write_fixmap_start : int -> fragment -> fragment
    val write_fixmap_next : fragment -> fragment
    val write_fixmap_end : int -> fragment -> fragment
    val write_map16_start : int -> fragment -> fragment
    val write_map16_next : fragment -> fragment
    val write_map16_end : int -> fragment -> fragment
    val write_map32_start : int -> fragment -> fragment
    val write_map32_next : fragment -> fragment
    val write_map32_end : int -> fragment -> fragment
    val write_map_best : int ->
                         (int -> fragment -> fragment) *
                           (fragment -> fragment) *
                             (int -> fragment -> fragment)
    val write_fixext1 : int -> int -> fragment -> fragment
    val write_fixext2 : int -> int -> fragment -> fragment
    val write_fixext4 : int -> int32 -> fragment -> fragment
    val write_fixext8 : int -> int64 -> fragment -> fragment
    val write_fixext16 : int -> int64 -> int64 -> fragment -> fragment
    val write_fixext : int -> string -> fragment -> fragment
    val write_ext8 : int -> string -> int -> int -> fragment -> fragment
    val write_ext16 : int -> string -> int -> int -> fragment -> fragment
    val write_ext32 : int -> string -> int -> int -> fragment -> fragment
    val write_ext_best : int -> string -> int -> int -> fragment -> fragment

    val write_message : message -> fragment -> fragment
  end

end


module Extract : sig
  exception Error
  module Make(X : Types.MESSAGE_EXTRACTOR) : sig
    val extract_string : string -> int -> int -> X.message
    val extract_bytes : bytes -> int -> int -> X.message
    val extract_big : Types.bigstring -> int -> int -> X.message
  end
end

module Composer : sig
  exception Error
  module Bytes : Types.MESSAGE_COMPOSER with type message = bytes
  module Bigstring : Types.MESSAGE_COMPOSER with type message = Types.bigstring
  module Checker(C : Types.MESSAGE_COMPOSER) :
           Types.MESSAGE_COMPOSER with type message = C.message
end


module Yojson : sig
  exception Cannot_represent of string
  module Safe : sig

    type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      | `Tuple of json list
      | `Variant of (string * json option)
      ]

    module Extractor : Types.MESSAGE_EXTRACTOR
                       with type message = json
                       and type fragment = json list

    val extract_bytes : bytes -> int -> int -> json
    val extract_string : string -> int -> int -> json
    val extract_big : Types.bigstring -> int -> int -> json
    module Compose(C : Types.MESSAGE_COMPOSER) : sig
      val compose : json -> C.message
    end

    val compose_bytes : json -> bytes
    val compose_big : json -> Types.bigstring
  end
end
