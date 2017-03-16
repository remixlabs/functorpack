exception Cannot_represent of string

module Safe = struct
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

  module Extractor = struct
    type message = json
    type fragment = json list  (* a stack in reverse order *)

    let create() =
      []

    let extract =
      function
      | [ top ] -> top
      | _ -> failwith "FPack.Yojson.Safe.Extractor.extract"

    let read_nil frag = `Null :: frag
    let read_bool b frag = `Bool b :: frag
    let read_fixnum n frag = `Int n :: frag
    let read_uint8 = read_fixnum
    let read_uint16 = read_fixnum

    let read_uint32 =
      if Sys.word_size = 32 then
        (fun n frag ->
          if n >= 0l then
            if n <= Int32.of_int max_int then
              `Int (Int32.to_int n) :: frag
            else
              `Intlit (Int32.to_string n) :: frag
          else
            `Intlit
               (Int64.to_string
                  (Int64.logand (Int64.of_int32 n) 0xffff_ffffL)) :: frag
        )
      else
        (fun n frag ->
          if n >= 0l then
            `Int (Int32.to_int n) :: frag
          else
            `Int ((Int32.to_int n) land ((0x3fff_ffff lsl 2) lor 3)) :: frag
            (* NB 0xffff_ffff is meant but gives an error on 32 bit *)
        )

    let uint64_divmod10 n =
      (* Computes (n/10, n mod 10) for an int64 that is interpreted as
         an unsigned number. OCaml only has signed int64.
       *)
      (* inspired by http://www.hackersdelight.org/divcMore.pdf *)
      let ( >> ) = Int64.shift_right_logical in
      let ( <|< ) = Int64.shift_left in
      let ( ++ ) = Int64.add in
      let ( -- ) = Int64.sub in
      let q = (n >> 1) ++ (n >> 2) in
      let q = q ++ (q >> 4) in
      let q = q ++ (q >> 8) in
      let q = q ++ (q >> 16) in
      let q = q ++ (q >> 32) in
      let q = q >> 3 in   (* q is now a potentially too small approximation *)
      let qx10 = (q <|< 3) ++ (q <|< 1) in
      let r = n -- qx10 in
      let q = q ++ (Int64.div r 10L) in  (* the exact result *)
      let qx10 = (q <|< 3) ++ (q <|< 1) in
      let r = n -- qx10 in
      (q, r)

    let uint64_to_string n =
      if n >= 0L then
        Int64.to_string n
      else
        let q, r = uint64_divmod10 n in
        let s1 = Int64.to_string q in
        let s2 = Int64.to_string r in
        s1 ^ s2

    let read_uint64 n frag =
      if n >= 0L then
        if n <= Int64.of_int max_int then
          `Int (Int64.to_int n) :: frag
        else
          `Intlit (uint64_to_string n) :: frag
      else
        `Intlit (uint64_to_string n) :: frag

    let read_int8 = read_fixnum
    let read_int16 = read_fixnum

    let read_int32 =
      if Sys.word_size = 32 then
        (fun n frag ->
          if n >= Int32.of_int min_int && n <= Int32.of_int max_int then
            `Int (Int32.to_int n) :: frag
          else
            `Intlit (Int32.to_string n) :: frag
        )
      else
        (fun n frag ->
          `Int (Int32.to_int n) :: frag
        )

    let read_int64 n frag =
      if n >= Int64.of_int min_int && n <= Int64.of_int max_int then
        `Int (Int64.to_int n) :: frag
      else
        `Intlit (Int64.to_string n) :: frag

    let read_float32 x frag = `Float x :: frag
    let read_float64 = read_float32

    let read_fixstr by pos len frag =
      `String (Bytes.sub_string by pos len) :: frag

    let read_str8 = read_fixstr
    let read_str16 = read_fixstr
    let read_str32 = read_fixstr
    let read_bin8 = read_fixstr
    let read_bin16 = read_fixstr
    let read_bin32 = read_fixstr

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
      `List l :: frag

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
                  | `String s ->
                      extract_map (n-1) l' ((s,y) :: acc)
                  | _ ->
                      raise(Cannot_represent "found map with non-string key")
              )

    let read_fixmap_start n frag = frag
    let read_fixmap_next frag = frag
    let read_fixmap_end n frag =
      let l, frag = extract_map n frag [] in
      `Assoc l :: frag

    let read_map16_start = read_fixmap_start
    let read_map16_next = read_fixmap_next
    let read_map16_end = read_fixmap_end

    let read_map32_start = read_fixmap_start
    let read_map32_next = read_fixmap_next
    let read_map32_end = read_fixmap_end

    let read_fixext1 t n _ frag =
      raise(Cannot_represent "fixext1")

    let read_fixext2 t n _ frag =
      raise(Cannot_represent "fixext2")

    let read_fixext4 t n _ frag =
      raise(Cannot_represent "fixext4")

    let read_fixext8 t n _ frag =
      raise(Cannot_represent "fixext8")

    let read_fixext16 t n1 n2 _ rag =
      raise(Cannot_represent "fixext16")

    let read_ext8 t by pos len frag =
      raise(Cannot_represent "ext8")

    let read_ext16 t by pos len frag =
      raise(Cannot_represent "ext16")

    let read_ext32 t by pos len frag =
      raise(Cannot_represent "ext32")

  end

  let extract_bytes by pos len =
    let module E = Extract.Make(Extractor) in
    E.extract_bytes by pos len

  module Compose(C : Types.MESSAGE_COMPOSER) = struct
    let compose j =
      let rec recurse frag j =
        match j with
          | `Null ->
              C.write_nil frag
          | `Bool b ->
              C.write_bool b frag
          | `Int n ->
              C.write_int_best n frag
          | `Intlit s ->
              ( try
                  let n = Int64.of_string s in
                  C.write_int64 n frag
                with
                  | Failure _ ->
                      (* TODO: also support uint64 *)
                      raise Composer.Error
              )
          | `Float x ->
              C.write_float64 x frag
          | `String s ->
              C.write_str_best s 0 (String.length s) frag
          | `List l ->
              let n = List.length l in
              let f_start, f_end = C.write_array_best n in
              let frag = f_start n frag in
              let frag = List.fold_left recurse frag l in
              f_end n frag
          | `Assoc l ->
              let n = List.length l in
              let f_start, f_next, f_end = C.write_map_best n in
              let frag = f_start n frag in
              let _, frag =
                List.fold_left
                  (fun (i,frag) (key,value) ->
                    let frag = if i > 0 then f_next frag else frag in
                    let frag = recurse frag (`String key) in
                    let frag = recurse frag value in
                    (i+1, frag)
                  )
                  (0,frag)
                  l in
              f_end n frag
          | `Tuple _ ->
              failwith "FPack.Yojson.Safe.compose_bytes: `Tuple not supported"
          | `Variant _ ->
              failwith "FPack.Yojson.Safe.compose_bytes: `Variant not supported"
      in
      C.compose (recurse (C.create()) j)
  end

  let compose_bytes json =
    let module C = Composer.Checker(Composer.Bytes) in
    let module P = Compose(C) in
    P.compose json

end
