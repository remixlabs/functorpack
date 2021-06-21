exception Error

module type READER = sig
  type binmsg
  type position

  val length : binmsg -> int
  val get : binmsg -> int -> char
  val sub_bytes : binmsg -> int -> int -> bytes
  val move : binmsg -> int -> position
  val offset : position -> int
  val read_char : binmsg -> position -> int -> char
  val read_int8 : binmsg -> position -> int -> int
  val read_uint8 : binmsg -> position -> int -> int
  val read_int16 : binmsg -> position -> int -> int
  val read_uint16 : binmsg -> position -> int -> int
  val read_int32 : binmsg -> position -> int -> int32
  val read_int64 : binmsg -> position -> int -> int64
  val read_size32 : binmsg -> position -> int -> int -> int
  val read_string : (bytes -> int -> int -> 'a -> 'b) ->
                    binmsg -> position -> int -> int -> 'a -> 'b
end

let make_read_size32 read_int32 =
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

module String_reader = struct
  type binmsg = string
  type position = int ref

  let length = String.length
  let get = String.get
  let sub_bytes s k len =
    let by = Bytes.create len in
    Bytes.blit_string s k by 0 len;
    by

  let move by pos =
    ref pos

  let offset pos = !pos

  let read_uint8 by pos endpos =
    if !pos >= endpos then raise Error;
    let n = Char.code(String.get by !pos) in
    incr pos;
    n

  let read_char by pos endpos =
    read_uint8 by pos endpos |> Char.unsafe_chr

  let read_uint16 by pos endpos =
    if !pos >= endpos-1 then raise Error;
    let n =
      (Char.code(String.get by !pos) lsl 8) lor
        (Char.code(String.get by (!pos+1))) in
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
      (Int64.logand (Int64.of_int32 n2) 0xFFFF_FFFFL)

  let read_int8 by pos endpos =
    let n = read_uint8 by pos endpos in
    if n >= 128 then n - 256 else n

  let read_int16 by pos endpos =
    let n = read_uint16 by pos endpos in
    if n >= 32768 then n - 65536 else n

  let read_string f (by:binmsg) pos endpos n frag =
    if !pos > endpos - n then raise Error;
    let p = !pos in
    pos := !pos + n;
    f (sub_bytes by p n) 0 n frag

  let read_size32 = make_read_size32 read_int32

end

module Bytes_reader = struct
  type binmsg = bytes
  type position = int ref

  let length = Bytes.length
  let get = Bytes.get
  let sub_bytes = Bytes.sub

  let move by pos =
    ref pos

  let offset pos = !pos

  let read_uint8 by pos endpos =
    if !pos >= endpos then raise Error;
    let n = Char.code(Bytes.get by !pos) in
    incr pos;
    n

  let read_char by pos endpos =
    read_uint8 by pos endpos |> Char.unsafe_chr

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
      (Int64.logand (Int64.of_int32 n2) 0xFFFF_FFFFL)

  let read_int8 by pos endpos =
    let n = read_uint8 by pos endpos in
    if n >= 128 then n - 256 else n

  let read_int16 by pos endpos =
    let n = read_uint16 by pos endpos in
    if n >= 32768 then n - 65536 else n

  let read_string f (by:binmsg) pos endpos n frag =
    if !pos > endpos - n then raise Error;
    let p = !pos in
    pos := !pos + n;
    f by p n frag

  let read_size32 = make_read_size32 read_int32
end

module Rope_reader = struct
  type binmsg = Rope.t
  type position = Rope.Iterator.t

  let length = Rope.length
  let get = Rope.get
  let sub_bytes rope k len =
    let by = Bytes.create len in
    Rope.blit rope k by 0 len;
    by

  let move = Rope.Iterator.make
  let offset ri = Rope.Iterator.pos ri

  let read_uint8 by pos endpos =
    if Rope.Iterator.pos pos >= endpos then raise Error;
    let n = Char.code(Rope.Iterator.get pos) in
    Rope.Iterator.incr pos;
    n

  let read_char by pos endpos =
    read_uint8 by pos endpos |> Char.unsafe_chr

  let read_uint16 by pos endpos =
    if Rope.Iterator.pos pos >= endpos-1 then raise Error;
    let c0 = Rope.Iterator.get pos in
    Rope.Iterator.incr pos;
    let c1 = Rope.Iterator.get pos in
    Rope.Iterator.incr pos;
    let n = (Char.code c0 lsl 8) lor (Char.code c1) in
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
      (Int64.logand (Int64.of_int32 n2) 0xFFFF_FFFFL)

  let read_int8 by pos endpos =
    let n = read_uint8 by pos endpos in
    if n >= 128 then n - 256 else n

  let read_int16 by pos endpos =
    let n = read_uint16 by pos endpos in
    if n >= 32768 then n - 65536 else n

  let read_string f rope pos endpos n frag =
    if offset pos > endpos - n then raise Error;
    let by = sub_bytes rope (offset pos) n in
    Rope.Iterator.move pos n;
    f by 0 n frag

  let read_size32 = make_read_size32 read_int32
end

module MakeForReader(X : Types.MESSAGE_EXTRACTOR)(R : READER) = struct
  let rec loop n f x =
    if n=0 then x else
      loop (n-1) f (f x)

  let loopi n f x =
    let rec loop i x =
      if i=n then x else
        loop (i+1) (f i x) in
    loop 0 x

  let rec xbytes by pos endpos frag =
    let c = R.read_char by pos endpos in
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
          let n = R.read_uint8 by pos endpos in
          X.read_uint8 n frag
      | '\xcd' ->
          let n = R.read_uint16 by pos endpos in
          X.read_uint16 n frag
      | '\xce' ->
          let n = R.read_int32 (* sic! *) by pos endpos in
          X.read_uint32 n frag
      | '\xcf' ->
          let n = R.read_int64 (* sic! *) by pos endpos in
          X.read_uint64 n frag
      | '\xd0' ->
          let n = R.read_int8 by pos endpos in
          X.read_int8 n frag
      | '\xd1' ->
          let n = R.read_int16 by pos endpos in
          X.read_int16 n frag
      | '\xd2' ->
          let n = R.read_int32 by pos endpos in
          X.read_int32 n frag
      | '\xd3' ->
          let n = R.read_int64 by pos endpos in
          X.read_int64 n frag
      | '\xca' ->
          let n = R.read_int32 by pos endpos in
          X.read_float32 (Int32.float_of_bits n) frag
      | '\xcb' ->
          let n = R.read_int64 by pos endpos in
          X.read_float64 (Int64.float_of_bits n) frag
      | '\xa0' .. '\xbf' ->
          let n = Char.code c land 0x1f in
          R.read_string X.read_fixstr by pos endpos n frag
      | '\xd9' ->
          let n = R.read_uint8 by pos endpos in
          R.read_string X.read_str8 by pos endpos n frag
      | '\xda' ->
          let n = R.read_uint16 by pos endpos in
          R.read_string X.read_str16 by pos endpos n frag
      | '\xdb' ->
          let n = R.read_size32 by pos endpos Sys.max_string_length in
          R.read_string X.read_str32 by pos endpos n frag
      | '\xc4' ->
          let n = R.read_uint8 by pos endpos in
          R.read_string X.read_bin8 by pos endpos n frag
      | '\xc5' ->
          let n = R.read_uint16 by pos endpos in
          R.read_string X.read_bin16 by pos endpos n frag
      | '\xc6' ->
          let n = R.read_size32 by pos endpos Sys.max_string_length in
          R.read_string X.read_bin32 by pos endpos n frag
      | '\x90' .. '\x9f' ->
          let n = Char.code c land 0xf in
          let frag = X.read_fixarray_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_fixarray_end n frag
      | '\xdc' ->
          let n = R.read_uint16 by pos endpos in
          let frag = X.read_array16_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_array16_end n frag
      | '\xdd' ->
          let n = R.read_size32 by pos endpos Sys.max_array_length in
          let frag = X.read_array32_start n frag in
          let frag = loop n (xbytes by pos endpos) frag in
          X.read_array32_end n frag
      | '\x80' .. '\x8f' ->
          let n = Char.code c land 0xf in
          xbytes_map
            by pos endpos frag n
            X.read_fixmap_start X.read_fixmap_next X.read_fixmap_end
      | '\xde' ->
          let n = R.read_uint16 by pos endpos in
          xbytes_map
            by pos endpos frag n
            X.read_map16_start X.read_map16_next X.read_map16_end
      | '\xdf' ->
          let n = R.read_size32 by pos endpos max_int in
          xbytes_map
            by pos endpos frag n
            X.read_map16_start X.read_map16_next X.read_map16_end
      | '\xd4' ->
          let t = R.read_uint8 by pos endpos in
          let p = R.offset pos in
          let n = R.read_uint8 by pos endpos in
          X.read_fixext1 t n (R.sub_bytes by p 1) frag
      | '\xd5' ->
          let t = R.read_uint8 by pos endpos in
          let p = R.offset pos in
          let n = R.read_uint16 by pos endpos in
          X.read_fixext2 t n (R.sub_bytes by p 2) frag
      | '\xd6' ->
          let t = R.read_uint8 by pos endpos in
          let p = R.offset pos in
          let n = R.read_int32 by pos endpos in
          X.read_fixext4 t n (R.sub_bytes by p 4) frag
      | '\xd7' ->
          let t = R.read_uint8 by pos endpos in
          let p = R.offset pos in
          let n = R.read_int64 by pos endpos in
          X.read_fixext8 t n (R.sub_bytes by p 8) frag
      | '\xd8' ->
          let t = R.read_uint8 by pos endpos in
          let p = R.offset pos in
          let n1 = R.read_int64 by pos endpos in
          let n2 = R.read_int64 by pos endpos in
          X.read_fixext16 t n1 n2 (R.sub_bytes by p 16) frag
      | '\xc7' ->
          let n = R.read_uint8 by pos endpos in
          let t = R.read_uint8 by pos endpos in
          R.read_string (X.read_ext8 t) by pos endpos n frag
      | '\xc8' ->
          let n = R.read_uint16 by pos endpos in
          let t = R.read_uint8 by pos endpos in
          R.read_string (X.read_ext16 t) by pos endpos n frag
      | '\xc9' ->
          let n = R.read_size32 by pos endpos Sys.max_string_length in
          let t = R.read_uint8 by pos endpos in
          R.read_string (X.read_ext32 t) by pos endpos n frag
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

  and extract by pos len =
    if len < 0 || pos < 0 || pos > R.length by - len then
      invalid_arg "FPack.Extract.Make.extract";
    let frag = X.create() in
    let rpos = R.move by pos in
    let endpos = pos + len in
    let frag' = xbytes by rpos (pos+len) frag in
    let p = R.offset rpos in
    if p < endpos then raise Error;
    X.extract frag'
end

module Make(X : Types.MESSAGE_EXTRACTOR) = struct
  module ForString = MakeForReader(X)(String_reader)
  module ForBytes = MakeForReader(X)(Bytes_reader)
  module ForRope = MakeForReader(X)(Rope_reader)

  let extract_string = ForString.extract
  let extract_bytes = ForBytes.extract
  let extract_rope = ForRope.extract
end
