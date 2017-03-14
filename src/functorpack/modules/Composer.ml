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
    add_int32 buf (Int64.to_int32 (Int64.shift_right_logical n 32));
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
    
    
end
