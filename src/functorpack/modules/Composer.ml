exception Error

module Bytes = struct
  type message = bytes
  type fragment = Buffer.t

  let create() = Buffer.create 100

  let compose frag = Buffer.to_bytes frag

  let write_nil buf =
    Buffer.add_char buf '\xc0';
    buf

  let write_bool b buf =
    Buffer.add_char buf (if b then '\xc3' else '\xc2');
    buf

  let write_fixnum n buf =
    if n < (-32) || n > 127 then raise Error;
    if n >= 0 then
      Buffer.add_char buf (Char.chr (n land 0x7f))
    else
      Buffer.add_char buf (Char.chr (0xe0 + n land 0x1f));
    buf

  let add_uint8 buf n =
    Buffer.add_char buf (Char.chr (n land 0xff))

  let add_uint16 buf n =
    add_uint8 buf (n lsr 8);
    add_uint8 buf n

  let add_int8 = add_uint8
  let add_int16 = add_uint16


  let add_int32 buf n =
    add_uint16 buf (Int32.to_int (Int32.shift_right_logical n 16));
    add_uint16 buf (Int32.to_int n)

  let add_int64 buf n =
    add_int32 buf (Int64.to_int32 (Int64.shift_right n 32));
    add_int32 buf (Int64.to_int32 n)

  let add_size32 =
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

  let write_uint8 n buf =
    if n < 0 || n > 255 then raise Error;
    Buffer.add_char buf '\xcc';
    add_uint8 buf n;
    buf

  let write_uint16 n buf =
    if n < 0 || n > 65535 then raise Error;
    Buffer.add_char buf '\xcd';
    add_uint16 buf n;
    buf
    
  let write_uint32 n buf =
    Buffer.add_char buf '\xce';
    add_int32 buf n;   (* sic! *)
    buf

  let write_uint64 n buf =
    Buffer.add_char buf '\xcf';
    add_int64 buf n;   (* sic! *)
    buf

  let write_int8 n buf =
    if n < (-128) || n > 127 then raise Error;
    Buffer.add_char buf '\xd0';
    add_int8 buf n;
    buf

  let write_int16 n buf =
    if n < (-32768) || n > 32767 then raise Error;
    Buffer.add_char buf '\xd1';
    add_int16 buf n;
    buf

  let write_int32 n buf =
    Buffer.add_char buf '\xd2';
    add_int32 buf n;
    buf
    
  let write_int64 n buf =
    Buffer.add_char buf '\xd3';
    add_int64 buf n;
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
      else if Sys.word_size=32 || n <= Int64.of_int32 Int32.max_int then
        write_int32 (Int64.to_int32 n) buf
      else
        write_int64 n buf
    )
    else (
      if n >= (-32L) then
        write_fixnum (Int64.to_int n) buf
      else if n >= (-32768L) then
        write_int16 (Int64.to_int n) buf
      else if Sys.word_size=32 || n >= Int64.of_int32 Int32.min_int then
        write_int32 (Int64.to_int32 n) buf
      else
        write_int64 n buf
    )
    
  let write_float32 x buf =
    Buffer.add_char buf '\xca';
    add_int32 buf (Int32.bits_of_float x);
    buf

  let write_float64 x buf =
    Buffer.add_char buf '\xcb';
    add_int64 buf (Int64.bits_of_float x);
    buf

  let write_fixstr s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_fixstr";
    if len > 31 then raise Error;
    Buffer.add_char buf (Char.chr (0xa0 + len));
    Buffer.add_substring buf s pos len;
    buf

  let write_str8 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_str8";
    if len > 255 then raise Error;
    Buffer.add_char buf '\xd9';
    add_uint8 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_str16 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_str16";
    if len > 65535 then raise Error;
    Buffer.add_char buf '\xda';
    add_uint16 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_str32 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_str32";
    Buffer.add_char buf '\xdb';
    add_size32 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_str_best s pos n buf =
    if n <= 31 then
      write_fixstr s pos n buf
    else if n <= 255 then
      write_str8 s pos n buf
    else if n <= 65536 then
      write_str16 s pos n buf
    else (
      if Sys.word_size=64 && n > Int32.to_int Int32.max_int then
        raise Error;
      write_str32 s pos n buf
    )

  let write_bin8 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_bin8";
    if len > 255 then raise Error;
    Buffer.add_char buf '\xc4';
    add_uint8 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_bin16 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_bin16";
    if len > 65535 then raise Error;
    Buffer.add_char buf '\xc5';
    add_uint16 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_bin32 s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_bin32";
    Buffer.add_char buf '\xc6';
    add_size32 buf len;
    Buffer.add_substring buf s pos len;
    buf

  let write_bin_best s pos n buf =
    if n <= 255 then
      write_bin8 s pos n buf
    else if n <= 65536 then
      write_bin16 s pos n buf
    else (
      if Sys.word_size=64 && n > Int32.to_int Int32.max_int then
        raise Error;
      write_bin32 s pos n buf
    )

  let write_fixarray_start n buf =
    if n < 0 || n > 15 then raise Error;
    Buffer.add_char buf (Char.chr (0x90 + n));
    buf

  let write_fixarray_end n buf = buf

  let write_array16_start n buf =
    if n < 0 || n > 65535 then raise Error;
    Buffer.add_char buf '\xdc';
    add_uint16 buf n;
    buf

  let write_array16_end n buf = buf

  let write_array32_start n buf =
    if n < 0 then raise Error;
    Buffer.add_char buf '\xdd';
    add_size32 buf n;
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
    Buffer.add_char buf (Char.chr (0x80 + n));
    buf

  let write_fixmap_next buf = buf
  let write_fixmap_end n buf = buf

  let write_map16_start n buf =
    if n < 0 || n > 65535 then raise Error;
    Buffer.add_char buf '\xde';
    add_uint16 buf n;
    buf

  let write_map16_next buf = buf
  let write_map16_end n buf = buf
    
  let write_map32_start n buf =
    if n < 0 then raise Error;
    Buffer.add_char buf '\xdf';
    add_size32 buf n;
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
    Buffer.add_char buf '\xd4';
    add_uint8 buf t;
    add_uint8 buf n;
    buf

  let write_fixext2 t n buf =
    if t < 0 || t > 255 || n < 0 || n > 65535 then raise Error;
    Buffer.add_char buf '\xd5';
    add_uint8 buf t;
    add_uint16 buf n;
    buf

  let write_fixext4 t n buf =
    if t < 0 || t > 255 then raise Error;
    Buffer.add_char buf '\xd6';
    add_uint8 buf t;
    add_int32 buf n;
    buf

  let write_fixext8 t n buf =
    if t < 0 || t > 255 then raise Error;
    Buffer.add_char buf '\xd7';
    add_uint8 buf t;
    add_int64 buf n;
    buf

  let write_fixext16 t n1 n2 buf =
    if t < 0 || t > 255 then raise Error;
    Buffer.add_char buf '\xd8';
    add_uint8 buf t;
    add_int64 buf n1;
    add_int64 buf n2;
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
        | _ -> invalid_arg "FPack.Compose.Bytes.write_fixext: bad length" in
    Buffer.add_char buf tag;
    add_uint8 buf t;
    Buffer.add_string buf str;
    buf

  let write_ext8 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_ext8";
    if t < 0 || t > 255 || len > 255 then raise Error;
    Buffer.add_char buf '\xc7';
    add_uint8 buf len;
    add_uint8 buf t;
    Buffer.add_substring buf s pos len;
    buf

  let write_ext16 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_ext16";
    if t < 0 || t > 255 || len > 65535 then raise Error;
    Buffer.add_char buf '\xc8';
    add_uint16 buf len;
    add_uint8 buf t;
    Buffer.add_substring buf s pos len;
    buf

  let write_ext32 t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_ext32";
    if t < 0 || t > 255 then raise Error;
    Buffer.add_char buf '\xc9';
    add_size32 buf len;
    add_uint8 buf t;
    Buffer.add_substring buf s pos len;
    buf

  let write_ext_best t s pos len buf =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "FPack.Composer.Bytes.write_ext_best";
    if t < 0 || t > 255 then raise Error;
    if len=1 || len=2 || len=4 || len=8 || len=16 then
      write_fixext t (String.sub s pos len) buf
    else
      if len <= 255 then
        write_ext8 t s pos len buf
      else if len <= 65536 then
        write_ext16 t s pos len buf
      else
        write_ext32 t s pos len buf

end

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
