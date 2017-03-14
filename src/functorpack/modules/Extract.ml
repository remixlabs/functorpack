exception Error

module Make(X : Types.MESSAGE_EXTRACTOR) = struct
  let read_uint8 by pos endpos =
    if !pos >= endpos then raise Error;
    let n = Char.code(Bytes.get by !pos) in
    incr pos;
    n

  let read_uint16 by pos endpos =
    if !pos >= endpos-1 then raise Error;
    let n =
      (Char.code(Bytes.get by !pos) lsl 8) lor
        (Char.code(Bytes.get by (!pos+1))) in
    pos := !pos + 2;
    n

  let read_int32 by pos endpos =
    let n1 = read_uint16 by pos endpos in
    let n2 = read_uint16 by pos endpos in
    Int32.logor
      (Int32.shift_left (Int32.of_int n1) 16)
      (Int32.of_int n2)

  let read_int64 by pos endpos =
    let n1 = read_int32 by pos endpos in
    let n2 = read_int32 by pos endpos in
    Int64.logor
      (Int64.shift_left (Int64.of_int32 n1) 32)
      (Int64.of_int32 n2)

  let read_int8 by pos endpos =
    let n = read_uint8 by pos endpos in
    if n >= 128 then n - 256 else n

  let read_int16 by pos endpos =
    let n = read_uint16 by pos endpos in
    if n >= 32768 then n - 65536 else n

  let read_string f by pos endpos n frag =
    if !pos > endpos - n then raise Error;
    let p = !pos in
    pos := !pos + n;
    f by p n frag

  let read_size32 =
    if Sys.word_size = 32 then
      (fun by pos endpos m ->
        let n = read_int32 by pos endpos in
        if n < 0l || n >= Int32.of_int m then
          raise Error;
        Int32.to_int n
      )
    else
      (fun by pos endpos _ ->
        let n = read_int32 by pos endpos in
        if n < 0l then
          Int32.to_int n + (Int32.to_int Int32.max_int + 1) lsl 1
        else
          Int32.to_int n
      )

  let rec loop n f x =
    if n=0 then x else
      loop (n-1) f (f x)

  let loopi n f x =
    let rec loop i x =
      if i=n then x else
        loop (i+1) (f i x) in
    loop 0 x

  let rec xbytes by pos endpos frag =
    if !pos >= endpos then raise Error;
    let c = Bytes.get by !pos in
    incr pos;
    match c with  (* the following in the order of the spec *)
      | '\xc0' ->
          X.read_nil frag
      | '\xc2' ->
          X.read_bool false frag
      | '\xc3' ->
          X.read_bool true frag
      | '\x00' .. '\x7f' as c ->
          X.read_fixnum (Char.code c) frag
      | '\xe0' .. '\xff' as c ->
          X.read_fixnum (Char.code c - 256) frag
      | '\xcc' ->
          let n = read_uint8 by pos endpos in
          X.read_uint8 n frag
      | '\xcd' ->
          let n = read_uint16 by pos endpos in
          X.read_uint16 n frag
      | '\xce' ->
          let n = read_int32 (* sic! *) by pos endpos in
          X.read_uint32 n frag
      | '\xcf' ->
          let n = read_int64 (* sic! *) by pos endpos in
          X.read_uint64 n frag
      | '\xd0' ->
          let n = read_int8 by pos endpos in
          X.read_int8 n frag
      | '\xd1' ->
          let n = read_int16 by pos endpos in
          X.read_int16 n frag
      | '\xd2' ->
          let n = read_int32 by pos endpos in
          X.read_int32 n frag
      | '\xd3' ->
          let n = read_int64 by pos endpos in
          X.read_int64 n frag
      | '\xca' ->
          let n = read_int32 by pos endpos in
          X.read_float32 (Int32.float_of_bits n) frag
      | '\xcb' ->
          let n = read_int64 by pos endpos in
          X.read_float64 (Int64.float_of_bits n) frag
      | '\xa0' .. '\xbf' ->
          let n = Char.code c land 0x1f in
          read_string X.read_fixstr by pos endpos n frag
      | '\xd9' ->
          let n = read_uint8 by pos endpos in
          read_string X.read_str8 by pos endpos n frag
      | '\xda' ->
          let n = read_uint16 by pos endpos in
          read_string X.read_str16 by pos endpos n frag
      | '\xdb' ->
          let n = read_size32 by pos endpos Sys.max_string_length in
          read_string X.read_str32 by pos endpos n frag
      | '\xc4' ->
          let n = read_uint8 by pos endpos in
          read_string X.read_bin8 by pos endpos n frag
      | '\xc5' ->
          let n = read_uint16 by pos endpos in
          read_string X.read_bin16 by pos endpos n frag
      | '\xc6' ->
          let n = read_size32 by pos endpos Sys.max_string_length in
          read_string X.read_bin32 by pos endpos n frag
      | '\x90' .. '\x9f' ->
          let n = Char.code c land 0xf in
          let frag = X.read_fixarray_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_fixarray_end n frag
      | '\xdc' ->
          let n = read_uint16 by pos endpos in
          let frag = X.read_array16_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_array16_end n frag
      | '\xdd' ->
          let n = read_size32 by pos endpos Sys.max_array_length in
          let frag = X.read_array32_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_array32_end n frag
      | '\x80' .. '\x8f' ->
          let n = Char.code c land 0xf in
          xbytes_map
            by pos endpos frag n
            X.read_fixmap_start X.read_fixmap_next X.read_fixmap_end
      | '\xde' ->
          let n = read_uint16 by pos endpos in
          xbytes_map
            by pos endpos frag n
            X.read_map16_start X.read_map16_next X.read_map16_end
      | '\xdf' ->
          let n = read_size32 by pos endpos max_int in
          xbytes_map
            by pos endpos frag n
            X.read_map16_start X.read_map16_next X.read_map16_end
      | '\xd4' ->
          let t = read_uint8 by pos endpos in
          let n = read_uint8 by pos endpos in
          X.read_fixext1 t n frag
      | '\xd5' ->
          let t = read_uint8 by pos endpos in
          let n = read_uint16 by pos endpos in
          X.read_fixext2 t n frag
      | '\xd6' ->
          let t = read_uint8 by pos endpos in
          let n = read_int32 by pos endpos in
          X.read_fixext4 t n frag
      | '\xd7' ->
          let t = read_uint8 by pos endpos in
          let n = read_int64 by pos endpos in
          X.read_fixext8 t n frag
      | '\xd8' ->
          let t = read_uint8 by pos endpos in
          let n1 = read_int64 by pos endpos in
          let n2 = read_int64 by pos endpos in
          X.read_fixext16 t n1 n2 frag
      | '\xc7' ->
          let n = read_uint8 by pos endpos in
          let t = read_uint8 by pos endpos in
          read_string (X.read_ext8 t) by pos endpos n frag
      | '\xc8' ->
          let n = read_uint16 by pos endpos in
          let t = read_uint8 by pos endpos in
          read_string (X.read_ext16 t) by pos endpos n frag
      | '\xc9' ->
          let n = read_size32 by pos endpos Sys.max_string_length in
          let t = read_uint8 by pos endpos in
          read_string (X.read_ext32 t) by pos endpos n frag
      | _ ->
          raise Error

  and xbytes_map by pos endpos frag n f_start f_next f_end =
    let frag = f_start n frag in
    let frag =
      loopi
        n
        (fun i frag ->
          let frag = if i > 0 then f_next frag else frag in
          let frag = xbytes by pos endpos frag in
          let frag = xbytes by pos endpos frag in
          frag
        )
        frag in
    f_end n frag

  and extract_bytes by pos len =
    if len < 0 || pos < 0 || pos > Bytes.length by - len then
      invalid_arg "FPack.Extract.Make.extract_bytes";
    let frag = X.create() in
    let rpos = ref pos in
    let endpos = pos + len in
    let frag' = xbytes by rpos (pos+len) frag in
    if !rpos < endpos then raise Error;
    X.extract frag'


end
