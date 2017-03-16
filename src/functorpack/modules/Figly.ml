exception Cannot_represent of string

module type DATA = sig
  module SMap : Map.S with type key = string

  type data =
    | Dnull
    | Dbool of bool
    | Dint of int64
    | Dfloat of float
    | Dstring of string
    | Dbinary of string
    | Dobject of data SMap.t
    | Darray of data array
    | Dref of string

end

module Data : DATA = struct
  module SMap = Map.Make(String)

  type data =
    | Dnull
    | Dbool of bool
    | Dint of int64
    | Dfloat of float
    | Dstring of string
    | Dbinary of string
    | Dobject of data SMap.t
    | Darray of data array
    | Dref of string
end


module Make(D:DATA) = struct
  open D

  module type EB = sig
    val extract_bytes : bytes -> int -> int -> data
  end


  module rec Extractor : Types.MESSAGE_EXTRACTOR = struct
    type message = data
    type fragment = data list  (* a stack in reverse order *)

    let create() =
      []

    let extract =
      function
      | [ top ] -> top
      | _ -> failwith "FPack.Figly.Make.Extractor.extract"

    let read_nil frag = Dnull :: frag
    let read_bool b frag = Dbool b :: frag
    let read_fixnum n frag = Dint (Int64.of_int n) :: frag
    let read_uint8 = read_fixnum
    let read_uint16 = read_fixnum

    let read_uint32 n frag =
      if n >= 0l then
        Dint (Int64.of_int32 n) :: frag
      else
        Dint (Int64.logand (Int64.of_int32 n) 0xffff_ffffL) :: frag

    let read_uint64 n frag =
      if n >= 0L then
        Dint n :: frag
      else
        raise (Cannot_represent "integer outside int64 range")

    let read_int8 = read_fixnum
    let read_int16 = read_fixnum

    let read_int32 n frag =
      Dint (Int64.of_int32 n) :: frag

    let read_int64 n frag =
      Dint n :: frag

    let read_float32 x frag = Dfloat x :: frag
    let read_float64 = read_float32

    let read_fixstr by pos len frag =
      Dstring (Bytes.sub_string by pos len) :: frag

    let read_str8 = read_fixstr
    let read_str16 = read_fixstr
    let read_str32 = read_fixstr

    let read_bin8 by pos len frag =
      Dbinary (Bytes.sub_string by pos len) :: frag

    let read_bin16 = read_bin8
    let read_bin32 = read_bin8

    let rec extract_array n l acc =
      if n = 0 then
        (acc, l)
      else
        match l with
          | [] -> assert false
          | x :: l' -> extract_array (n-1) l' (x :: acc)

    let read_fixarray_start n frag = frag
    let read_fixarray_end n frag =
      let l, frag = extract_array n frag [] in
      Darray (Array.of_list l) :: frag

    let read_array16_start = read_fixarray_start
    let read_array16_end = read_fixarray_end

    let read_array32_start = read_fixarray_start
    let read_array32_end = read_fixarray_end

    let rec extract_map n l acc =
      if n = 0 then
        (acc, l)
      else
        match l with
          | [] -> assert false
          | [ _ ] -> assert false
          | y :: x :: l' ->
              ( match x with
                  | Dstring s ->
                      extract_map (n-1) l' ((s,y) :: acc)
                  | _ ->
                      raise(Cannot_represent "found map with non-string key")
              )

    let read_fixmap_start n frag = frag
    let read_fixmap_next frag = frag
    let read_fixmap_end n frag =
      let l, frag = extract_map n frag [] in
      let m = List.fold_left (fun m (k,v) -> SMap.add k v m) SMap.empty l in
      Dobject m :: frag

    let read_map16_start = read_fixmap_start
    let read_map16_next = read_fixmap_next
    let read_map16_end = read_fixmap_end

    let read_map32_start = read_fixmap_start
    let read_map32_next = read_fixmap_next
    let read_map32_end = read_fixmap_end

    let read_ext8 t b pos len frag =
      if t = 0x22 then
        match Eb.extract_bytes b pos len with
          | Dstring s ->
              Dref s :: frag
          | _ ->
              raise(Cannot_represent "found Ref with non-string content")
      else
        raise(Cannot_represent "unknown extension")

    let read_ext16 = read_ext8
    let read_ext32 = read_ext8

    let read_fixext1 t n b frag =
      read_ext8 t b 0 (Bytes.length b) frag

    let read_fixext2 = read_fixext1
    let read_fixext4 = read_fixext1
    let read_fixext8 = read_fixext1

    let read_fixext16 t n1 n2 b frag =
      read_ext8 t b 0 (Bytes.length b) frag
  end

  and Eb : EB = struct
    let extract_bytes by pos len =
      let module E = Extract.Make(Extractor) in
      E.extract_bytes by pos len
  end

  let extract_bytes = Eb.extract_bytes

  module Compose(C : Types.MESSAGE_COMPOSER) = struct
    let compose j =
      let rec recurse frag j =
        match j with
          | Dnull ->
              C.write_nil frag
          | Dbool b ->
              C.write_bool b frag
          | Dint n ->
              C.write_int64_best n frag
          | Dfloat x ->
              C.write_float64 x frag
          | Dstring s ->
              C.write_str_best s 0 (String.length s) frag
          | Dbinary s ->
              C.write_bin_best s 0 (String.length s) frag
          | Darray l ->
              let n = Array.length l in
              let f_start, f_end = C.write_array_best n in
              let frag = f_start n frag in
              let frag = Array.fold_left recurse frag l in
              f_end n frag
          | Dobject l ->
              let n = SMap.cardinal l in
              let f_start, f_next, f_end = C.write_map_best n in
              let frag = f_start n frag in
              let _, frag =
                SMap.fold
                  (fun key value (i,frag) ->
                    let frag = if i > 0 then f_next frag else frag in
                    let frag = recurse frag (Dstring key) in
                    let frag = recurse frag value in
                    (i+1, frag)
                  )
                  l
                  (0,frag) in
              f_end n frag
          | Dref id ->
              let ifrag = Composer.Bytes.create() in
              let ifrag =
                Composer.Bytes.write_str_best id 0 (String.length id) ifrag in
              let bytes = Composer.Bytes.compose ifrag in
              let str = Bytes.to_string bytes in
              C.write_ext_best 0x22 str 0 (String.length str) frag
      in
      C.compose (recurse (C.create()) j)
  end

  let compose_bytes json =
    let module C = Composer.Checker(Composer.Bytes) in
    let module P = Compose(C) in
    P.compose json


end
