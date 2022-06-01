exception Error

module type WRITER = sig
  type buffer
  type message
  val create : unit -> buffer
  val compose : buffer -> message
  val add_char : buffer -> char -> unit
  val add_int8 : buffer -> int -> unit
  val add_int16 : buffer -> int -> unit
  val add_int32 : buffer -> int32 -> unit
  val add_int64 : buffer -> int64 -> unit
  val add_size32 : buffer -> int -> unit
  val add_substring : buffer -> string -> int -> int -> unit
  val add_subrope : buffer -> Rope.t -> int -> int -> unit
  val add_message : buffer -> message -> unit
end

let make_add_size32 add_int32 =
  if Sys.word_size = 32 then
    (fun buf n ->
      assert(n >= 0);
      add_int32 buf (Int32.of_int n)
    )
  else
    (fun buf n ->
      assert(n >= 0);
      if n > Int32.to_int Int32.max_int then raise Error;
      add_int32 buf (Int32.of_int n)
    )

module Bytes_writer = struct
  type buffer = Buffer.t
  type message = bytes

  let create() = Buffer.create 100
  let compose frag = Buffer.to_bytes frag

  let add_char = Buffer.add_char

  let add_int8 buf n =
    Buffer.add_char buf (Char.unsafe_chr (n land 0xff))

  let add_int16 buf n =
    add_int8 buf (n lsr 8);
    add_int8 buf n

  let add_int32 buf n =
    add_int16 buf (Int32.to_int (Int32.shift_right_logical n 16));
    add_int16 buf (Int32.to_int n)

  let add_int64 buf n =
    add_int32 buf (Int64.to_int32 (Int64.shift_right n 32));
    add_int32 buf (Int64.to_int32 n)

  let add_size32 =
    make_add_size32 add_int32

  let add_substring =
    Buffer.add_substring

  let add_subrope buf rope p len =
    let srope = Rope.sub rope p len in
    Buffer.add_string buf (Rope.to_string srope)

  let add_message =
    Buffer.add_bytes
end

module Rope_writer = struct
  type buffer = Rope.Buffer.t
  type message = Rope.t

  let create() = Rope.Buffer.create 100
  let compose frag = Rope.Buffer.contents frag

  let add_char = Rope.Buffer.add_char

  let add_int8 buf n =
    Rope.Buffer.add_char buf (Char.unsafe_chr (n land 0xff))

  let add_int16 buf n =
    let by = Bytes.create 2 in
    Bytes.set by 0 (Char.unsafe_chr ((n lsr 8) land 0xff));
    Bytes.set by 1 (Char.unsafe_chr (n land 0xff));
    Rope.Buffer.add_string buf (Bytes.unsafe_to_string by)

  let add_int32 buf n =
    let by = Bytes.create 4 in
    let k0 = Int32.shift_right_logical n 24 |> Int32.to_int in
    let k1 = Int32.shift_right_logical n 16 |> Int32.to_int in
    let k2 = Int32.shift_right_logical n 8 |> Int32.to_int in
    let k3 = Int32.to_int n in
    Bytes.set by 0 (Char.unsafe_chr (k0 land 0xff));
    Bytes.set by 1 (Char.unsafe_chr (k1 land 0xff));
    Bytes.set by 2 (Char.unsafe_chr (k2 land 0xff));
    Bytes.set by 3 (Char.unsafe_chr (k3 land 0xff));
    Rope.Buffer.add_string buf (Bytes.unsafe_to_string by)

  let add_int64 buf n =
    add_int32 buf (Int64.to_int32 (Int64.shift_right n 32));
    add_int32 buf (Int64.to_int32 n)

  let add_size32 =
    make_add_size32 add_int32

  let add_substring =
    Rope.Buffer.add_substring

  let add_subrope buf rope p len =
    let srope = Rope.sub rope p len in
    Rope.Buffer.add_rope buf srope

  let add_message =
    Rope.Buffer.add_rope
end

module Serialize(W : WRITER) = struct
  type message = W.message
  type fragment = W.buffer

  let create = W.create
  let compose = W.compose

  let write_nil buf =
    W.add_char buf '\xc0';
    buf

  let write_bool b buf =
    W.add_char buf (if b then '\xc3' else '\xc2');
    buf

  let write_fixnum n buf =
    if n < (-32) || n > 127 then raise Error;
    if n >= 0 then
      W.add_char buf (Char.unsafe_chr (n land 0x7f))
    else
      W.add_char buf (Char.unsafe_chr (0xe0 + n land 0x1f));
    buf

  let write_uint8 n buf =
    if n < 0 || n > 255 then raise Error;
    W.add_char buf '\xcc';
    W.add_int8 buf n;
    buf

  let write_uint16 n buf =
    if n < 0 || n > 65535 then raise Error;
    W.add_char buf '\xcd';
    W.add_int16 buf n;
    buf
    
  let write_uint32 n buf =
    W.add_char buf '\xce';
    W.add_int32 buf n;   (* sic! *)
    buf

  let write_uint64 n buf =
    W.add_char buf '\xcf';
    W.add_int64 buf n;   (* sic! *)
    buf

  let write_int8 n buf =
    if n < (-128) || n > 127 then raise Error;
    W.add_char buf '\xd0';
    W.add_int8 buf n;
    buf

  let write_int16 n buf =
    if n < (-32768) || n > 32767 then raise Error;
    W.add_char buf '\xd1';
    W.add_int16 buf n;
    buf

  let write_int32 n buf =
    W.add_char buf '\xd2';
    W.add_int32 buf n;
    buf
    
  let write_int64 n buf =
    W.add_char buf '\xd3';
    W.add_int64 buf n;
    buf

  let write_int_best n buf =
    if n >= 0 then (
      if n <= 127 then
        write_fixnum n buf
      else if n <= 32767 then
        write_int16 n buf
      else if Sys.word_size=32 || n <= Int32.to_int Int32.max_int then
        write_int32 (Int32.of_int n) buf
      else
        write_int64 (Int64.of_int n) buf
    )
    else (
      if n >= (-32) then
        write_fixnum n buf
      else if n >= (-32768) then
        write_int16 n buf
      else if Sys.word_size=32 || n >= Int32.to_int Int32.min_int then
        write_int32 (Int32.of_int n) buf
      else
        write_int64 (Int64.of_int n) buf
    )

  let write_int64_best n buf =
    if n >= 0L then (
      if n <= 127L then
        write_fixnum (Int64.to_int n) buf
      else if n <= 32767L then
        write_int16 (Int64.to_int n) buf
      else if n <= Int64.of_int32 Int32.max_int then
        write_int32 (Int64.to_int32 n) buf
      else
        write_int64 n buf
    )
    else (
      if n >= (-32L) then
        write_fixnum (Int64.to_int n) buf
      else if n >= (-32768L) then
        write_int16 (Int64.to_int n) buf
      else if n >= Int64.of_int32 Int32.min_int then
        write_int32 (Int64.to_int32 n) buf
      else
        write_int64 n buf
    )
    
  let write_float32 x buf =
    W.add_char buf '\xca';
    W.add_int32 buf (Int32.bits_of_float x);
    buf

  let write_float64 x buf =
    W.add_char buf '\xcb';
    W.add_int64 buf (Int64.bits_of_float x);
    buf

  let write_fixstr s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_fixstr";
    if len > 31 then raise Error;
    W.add_char buf (Char.unsafe_chr (0xa0 + len));
    W.add_substring buf s pos len;
    buf

  let write_str8 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_str8";
    if len > 255 then raise Error;
    W.add_char buf '\xd9';
    W.add_int8 buf len;
    W.add_substring buf s pos len;
    buf

  let write_str16 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_str16";
    if len > 65535 then raise Error;
    W.add_char buf '\xda';
    W.add_int16 buf len;
    W.add_substring buf s pos len;
    buf

  let write_str32 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_str32";
    W.add_char buf '\xdb';
    W.add_size32 buf len;
    W.add_substring buf s pos len;
    buf

  let write_str_best s pos n buf =
    if n <= 31 then
      write_fixstr s pos n buf
    else if n <= 255 then
      write_str8 s pos n buf
    else if n <= 65535 then
      write_str16 s pos n buf
    else (
      if Sys.word_size=64 && n > Int32.to_int Int32.max_int then
        raise Error;
      write_str32 s pos n buf
    )

  let write_bin8 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_bin8";
    if len > 255 then raise Error;
    W.add_char buf '\xc4';
    W.add_int8 buf len;
    W.add_substring buf s pos len;
    buf

  let write_bin16 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_bin16";
    if len > 65535 then raise Error;
    W.add_char buf '\xc5';
    W.add_int16 buf len;
    W.add_substring buf s pos len;
    buf

  let write_bin32 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_bin32";
    W.add_char buf '\xc6';
    W.add_size32 buf len;
    W.add_substring buf s pos len;
    buf

  let write_bin32_rope s pos len buf =
    if pos < 0 || len < 0 || pos > Rope.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_bin32_rope";
    W.add_char buf '\xc6';
    W.add_size32 buf len;
    W.add_subrope buf s pos len;
    buf

  let write_bin_best s pos n buf =
    if n <= 255 then
      write_bin8 s pos n buf
    else if n <= 65535 then
      write_bin16 s pos n buf
    else (
      if Sys.word_size=64 && n > Int32.to_int Int32.max_int then
        raise Error;
      write_bin32 s pos n buf
    )

  let write_bin_best_rope s pos n buf =
    if n <= 255 then
      write_bin8 (Rope.to_string s) pos n buf
    else if n <= 65535 then
      write_bin16 (Rope.to_string s) pos n buf
    else (
      if Sys.word_size=64 && n > Int32.to_int Int32.max_int then
        raise Error;
      write_bin32_rope s pos n buf
    )

  let write_fixarray_start n buf =
    if n < 0 || n > 15 then raise Error;
    W.add_char buf (Char.unsafe_chr (0x90 + n));
    buf

  let write_fixarray_end n buf = buf

  let write_array16_start n buf =
    if n < 0 || n > 65535 then raise Error;
    W.add_char buf '\xdc';
    W.add_int16 buf n;
    buf

  let write_array16_end n buf = buf

  let write_array32_start n buf =
    if n < 0 then raise Error;
    W.add_char buf '\xdd';
    W.add_size32 buf n;
    buf

  let write_array32_end n buf = buf

  let write_array_best n =
    if n <= 15 then
      write_fixarray_start, write_fixarray_end
    else if n <= 65535 then
      write_array16_start, write_array16_end
    else
      write_array32_start, write_array32_end

  let write_fixmap_start n buf =
    if n < 0 || n > 15 then raise Error;
    W.add_char buf (Char.unsafe_chr (0x80 + n));
    buf

  let write_fixmap_next buf = buf
  let write_fixmap_end n buf = buf

  let write_map16_start n buf =
    if n < 0 || n > 65535 then raise Error;
    W.add_char buf '\xde';
    W.add_int16 buf n;
    buf

  let write_map16_next buf = buf
  let write_map16_end n buf = buf
    
  let write_map32_start n buf =
    if n < 0 then raise Error;
    W.add_char buf '\xdf';
    W.add_size32 buf n;
    buf

  let write_map32_next buf = buf
  let write_map32_end n buf = buf

  let write_map_best n =
    if n <= 15 then
      write_fixmap_start, write_fixmap_next, write_fixmap_end
    else if n <= 65535 then
      write_map16_start, write_map16_next, write_map16_end
    else
      write_map32_start, write_map32_next, write_map32_end

  let write_fixext1 t n buf =
    if t < 0 || t > 255 || n < 0 || n > 255 then raise Error;
    W.add_char buf '\xd4';
    W.add_int8 buf t;
    W.add_int8 buf n;
    buf

  let write_fixext2 t n buf =
    if t < 0 || t > 255 || n < 0 || n > 65535 then raise Error;
    W.add_char buf '\xd5';
    W.add_int8 buf t;
    W.add_int16 buf n;
    buf

  let write_fixext4 t n buf =
    if t < 0 || t > 255 then raise Error;
    W.add_char buf '\xd6';
    W.add_int8 buf t;
    W.add_int32 buf n;
    buf

  let write_fixext8 t n buf =
    if t < 0 || t > 255 then raise Error;
    W.add_char buf '\xd7';
    W.add_int8 buf t;
    W.add_int64 buf n;
    buf

  let write_fixext16 t n1 n2 buf =
    if t < 0 || t > 255 then raise Error;
    W.add_char buf '\xd8';
    W.add_int8 buf t;
    W.add_int64 buf n1;
    W.add_int64 buf n2;
    buf

  let write_fixext t str buf =
    if t < 0 || t > 255 then raise Error;
    let n = String.length str in
    let tag =
      match n with
        | 1 -> '\xd4'
        | 2 -> '\xd5'
        | 4 -> '\xd6'
        | 8 -> '\xd7'
        | 16 -> '\xd8'
        | _ -> invalid_arg "FPack.Compose.Serialize.write_fixext: bad length" in
    W.add_char buf tag;
    W.add_int8 buf t;
    W.add_substring buf str 0 (String.length str);
    buf

  let write_ext8 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_ext8";
    if t < 0 || t > 255 || len > 255 then raise Error;
    W.add_char buf '\xc7';
    W.add_int8 buf len;
    W.add_int8 buf t;
    W.add_substring buf s pos len;
    buf

  let write_ext16 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_ext16";
    if t < 0 || t > 255 || len > 65535 then raise Error;
    W.add_char buf '\xc8';
    W.add_int16 buf len;
    W.add_int8 buf t;
    W.add_substring buf s pos len;
    buf

  let write_ext32 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_ext32";
    if t < 0 || t > 255 then raise Error;
    W.add_char buf '\xc9';
    W.add_size32 buf len;
    W.add_int8 buf t;
    W.add_substring buf s pos len;
    buf

  let write_ext_best t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Serialize.write_ext_best";
    if t < 0 || t > 255 then raise Error;
    if len=1 || len=2 || len=4 || len=8 || len=16 then
      write_fixext t (String.sub s pos len) buf
    else
      if len <= 255 then
        write_ext8 t s pos len buf
      else if len <= 65535 then
        write_ext16 t s pos len buf
      else
        write_ext32 t s pos len buf

  let write_message msg buf =
    W.add_message buf msg;
    buf

end

module Bytes = Serialize(Bytes_writer)
module Rope = Serialize(Rope_writer)

module Checker(C : Types.MESSAGE_COMPOSER) = struct
  type message = C.message
  type fragment = C.fragment * construction list
   and construction =
     | Simple
     | Fixarray of int * int
     | Array16 of int * int
     | Array32 of int * int
     | Fixmap of pos * int * int
     | Map16 of pos * int * int
     | Map32 of pos * int * int
   and pos =
     | Key
     | Value
     | Next

  let create() = (C.create(), [])

  let compose (frag, cl) =
    if cl <> [Simple] then raise Error;
    C.compose frag

  let record cl =
    match cl with
      | Simple :: _ ->
          raise Error
      | Fixarray(k,n) :: cl' ->
          if k=n then raise Error;
          Fixarray(k+1,n) :: cl'
      | Array16(k,n) :: cl' ->
          if k=n then raise Error;
          Array16(k+1,n) :: cl'
      | Array32(k,n) :: cl' ->
          if k=n then raise Error;
          Array32(k+1,n) :: cl'
      | Fixmap(Key,k,n) :: cl' ->
          Fixmap(Value,k,n) :: cl'
      | Fixmap(Value,k,n) :: cl' ->
          Fixmap(Next,k,n) :: cl'
      | Fixmap(Next,_,_) :: cl' ->
          raise Error
      | Map16(Key,k,n) :: cl' ->
          Map16(Value,k,n) :: cl'
      | Map16(Value,k,n) :: cl' ->
          Map16(Next,k,n) :: cl'
      | Map16(Next,_,_) :: cl' ->
          raise Error
      | Map32(Key,k,n) :: cl' ->
          Map32(Value,k,n) :: cl'
      | Map32(Value,k,n) :: cl' ->
          Map32(Next,k,n) :: cl'
      | Map32(Next,_,_) :: cl' ->
          raise Error
      | [] ->
          [ Simple ]


  let write_nil (frag,cl) = (C.write_nil frag, record cl)
  let write_bool b (frag,cl) = (C.write_bool b frag, record cl)
  let write_fixnum n (frag,cl) = (C.write_fixnum n frag, record cl)
  let write_uint8 n (frag,cl) = (C.write_uint8 n frag, record cl)
  let write_uint16 n (frag,cl) = (C.write_uint16 n frag, record cl)
  let write_uint32 n (frag,cl) = (C.write_uint32 n frag, record cl)
  let write_uint64 n (frag,cl) = (C.write_uint64 n frag, record cl)
  let write_int8 n (frag,cl) = (C.write_int8 n frag, record cl)
  let write_int16 n (frag,cl) = (C.write_int16 n frag, record cl)
  let write_int32 n (frag,cl) = (C.write_int32 n frag, record cl)
  let write_int64 n (frag,cl) = (C.write_int64 n frag, record cl)
  let write_int_best n (frag,cl) = (C.write_int_best n frag, record cl)
  let write_int64_best n (frag,cl) = (C.write_int64_best n frag, record cl)
  let write_float32 n (frag,cl) = (C.write_float32 n frag, record cl)
  let write_float64 n (frag,cl) = (C.write_float64 n frag, record cl)
  let write_fixstr s p l (frag,cl) = (C.write_fixstr s p l frag, record cl)
  let write_str8 s p l (frag,cl) = (C.write_str8 s p l frag, record cl)
  let write_str16 s p l (frag,cl) = (C.write_str16 s p l frag, record cl)
  let write_str32 s p l (frag,cl) = (C.write_str32 s p l frag, record cl)
  let write_str_best s p l (frag,cl) = (C.write_str_best s p l frag, record cl)
  let write_bin8 s p l (frag,cl) = (C.write_bin8 s p l frag, record cl)
  let write_bin16 s p l (frag,cl) = (C.write_bin16 s p l frag, record cl)
  let write_bin32 s p l (frag,cl) = (C.write_bin32 s p l frag, record cl)
  let write_bin_best s p l (frag,cl) = (C.write_bin_best s p l frag, record cl)
  let write_bin32_rope s p l (frag,cl) = (C.write_bin32_rope s p l frag, record cl)
  let write_bin_best_rope s p l (frag,cl) = (C.write_bin_best_rope s p l frag, record cl)
  let write_fixext1 t n (frag,cl) = (C.write_fixext1 t n frag, record cl)
  let write_fixext2 t n (frag,cl) = (C.write_fixext2 t n frag, record cl)
  let write_fixext4 t n (frag,cl) = (C.write_fixext4 t n frag, record cl)
  let write_fixext8 t n (frag,cl) = (C.write_fixext8 t n frag, record cl)
  let write_fixext16 t n1 n2 (frag,cl) = (C.write_fixext16 t n1 n2 frag,
                                          record cl)
  let write_fixext t s (frag,cl) = (C.write_fixext t s frag, record cl)
  let write_ext8 t s p l (frag,cl) = (C.write_ext8 t s p l frag, record cl)
  let write_ext16 t s p l (frag,cl) = (C.write_ext16 t s p l frag, record cl)
  let write_ext32 t s p l (frag,cl) = (C.write_ext32 t s p l frag, record cl)
  let write_ext_best t s p l (frag,cl) = (C.write_ext_best t s p l frag,
                                          record cl)
  let write_message msg (frag,cl) = (C.write_message msg frag, record cl)

  let write_fixarray_start n (frag,cl) =
    (C.write_fixarray_start n frag,
     Fixarray(0,n) :: cl
    )

  let write_fixarray_end n0 (frag,cl) =
    (C.write_fixarray_end n0 frag,
     match cl with
       | Fixarray(k,n) :: cl' ->
           if k <> n || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_array16_start n (frag,cl) =
    (C.write_array16_start n frag,
     Array16(0,n) :: cl
    )

  let write_array16_end n0 (frag,cl) =
    (C.write_array16_end n0 frag,
     match cl with
       | Array16(k,n) :: cl' ->
           if k <> n || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_array32_start n (frag,cl) =
    (C.write_array32_start n frag,
     Array32(0,n) :: cl
    )

  let write_array32_end n0 (frag,cl) =
    (C.write_array32_end n0 frag,
     match cl with
       | Array32(k,n) :: cl' ->
           if k <> n || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_array_best n =
    if n <= 15 then
      write_fixarray_start, write_fixarray_end
    else if n <= 65535 then
      write_array16_start, write_array16_end
    else
      write_array32_start, write_array32_end

  let write_fixmap_start n (frag,cl) =
    (C.write_fixmap_start n frag,
     Fixmap(Key,0,n) :: cl
    )

  let write_fixmap_next (frag,cl) =
    (C.write_fixmap_next frag,
     match cl with
       | Fixmap(Next,k,n) :: cl' ->
           if k=n-1 then raise Error;
           Fixmap(Key,k+1,n) :: cl'
       | _ ->
           raise Error
    )

  let write_fixmap_end n0 (frag,cl) =
    (C.write_fixmap_end n0 frag,
     match cl with
       | Fixmap(Next,k,n) :: cl' ->
           if k+1 <> n || n <> n0 then raise Error;
           record cl'
       | Fixmap(Key,0,n) :: cl' ->
           if n <> 0 || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_map16_start n (frag,cl) =
    (C.write_map16_start n frag,
     Map16(Key,0,n) :: cl
    )

  let write_map16_next (frag,cl) =
    (C.write_map16_next frag,
     match cl with
       | Map16(Next,k,n) :: cl' ->
           if k=n-1 then raise Error;
           Map16(Key,k+1,n) :: cl'
       | _ ->
           raise Error
    )

  let write_map16_end n0 (frag,cl) =
    (C.write_map16_end n0 frag,
     match cl with
       | Map16(Next,k,n) :: cl' ->
           if k+1 <> n || n <> n0 then raise Error;
           record cl'
       | Map16(Key,0,n) :: cl' ->
           if n <> 0 || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_map32_start n (frag,cl) =
    (C.write_map32_start n frag,
     Map32(Key,0,n) :: cl
    )

  let write_map32_next (frag,cl) =
    (C.write_map32_next frag,
     match cl with
       | Map32(Next,k,n) :: cl' ->
           if k=n-1 then raise Error;
           Map32(Key,k+1,n) :: cl'
       | _ ->
           raise Error
    )

  let write_map32_end n0 (frag,cl) =
    (C.write_map32_end n0 frag,
     match cl with
       | Map32(Next,k,n) :: cl' ->
           if k+1 <> n || n <> n0 then raise Error;
           record cl'
       | Map32(Key,0,n) :: cl' ->
           if n <> 0 || n <> n0 then raise Error;
           record cl'
       | _ ->
           raise Error
    )

  let write_map_best n =
    if n <= 15 then
      write_fixmap_start, write_fixmap_next, write_fixmap_end
    else if n <= 65535 then
      write_map16_start, write_map16_next, write_map16_end
    else
      write_map32_start, write_map32_next, write_map32_end


end
