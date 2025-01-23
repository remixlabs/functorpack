open FPack
open T_frame.Util

type 'a compose_extract =
  { compose : Yojson.Safe.json -> 'a;
    extract : 'a -> Yojson.Safe.json
  }

let test001 ce () =
  let j =
    `Assoc [ "key1", `Null;
             "key2", `Bool false;
             "key3", `Bool true;
             "key4", `Int 5;
             "key5", `Int (-5);
             "key6", `Int 200;
             "key7", `Int (-200);
             "key8", `Int 20000;
             "key9", `Int (-20000);
             "key10", `Int 200000;
             "key11", `Int (-200000);
             "key12", `String "short";
             "key13", `String (String.make 200 'X');
             "key14", `String (String.make 20000 'X');
             "key15", `String (String.make 200000 'X');
             "key16", `Float 3.14
           ] in
  let by = ce.compose j in
  let j' = ce.extract by in
  j = j'

let test002 ce () =
  let j = `Assoc [] in
  let by = ce.compose j in
  let j' = ce.extract by in
  j = j'

let test003 ce () =
  let j =
    `Assoc [ "c", `Int 7;
             "d", `Int 5;
             "b", `Int 6;
             "a", `Int 10
           ] in
  let by = ce.compose j in
  let j' = ce.extract by in
  j = j'

let test004 ce () =
  let j =
    `Assoc [ "a", `List [ `Int 56; `String "abc"; `Null ];
             "b", `List [ `Null; `String "abc"; `Int 56; ];
             "c", `List (Array.to_list (Array.make 20000 (`Int 42)));
             "d", `List (Array.to_list (Array.make 200000 (`Int 42)));
           ] in
  let by = ce.compose j in
  let j' = ce.extract by in
  j = j'

let test005 ce () =
  let l =
    [ "key4", `Intlit "5";
      "key5", `Intlit "-5";
      "key6", `Intlit "200";
      "key7", `Intlit "-200";
      "key8", `Intlit "20000";
      "key9", `Intlit "-20000";
      "key10", `Intlit "200000";
      "key11", `Intlit "-200000";
    ] in
  let j = `Assoc l in
  let j1 =
    `Assoc (List.map (fun (s, `Intlit n) -> (s, `Int (int_of_string n))) l) in
  let by = ce.compose j in
  let j' = ce.extract by in
  j1 = j'

let test006 ce () =
  let j = `Intlit "9223372036854775807" in  (* = Int64.max_int *)
  let by = ce.compose j in
  let j' = ce.extract by in
(*
  for i = 0 to Bytes.length by - 1 do
    Printf.printf "by[%d] = %d\n%!" i (Char.code (Bytes.get by i))
  done;
 *)
  j = j'

let test007 ce () =
  let j = `Intlit "-9223372036854775808" in  (* = Int64.min_int *)
  let by = ce.compose j in
  let j' = ce.extract by in
  j = j'

let ce_bytes =
  { compose = Yojson.Safe.compose_bytes;
    extract = (fun s -> Yojson.Safe.extract_bytes s 0 (Bytes.length s));
  }

let ce_big =
  { compose = Yojson.Safe.compose_big;
    extract = (fun s -> Yojson.Safe.extract_big s 0 (Bigarray.Array1.dim s));
  }
  
let tests =
  [ "test001", test001 ce_bytes;
    "test002", test002 ce_bytes;
    "test003", test003 ce_bytes;
    "test004", test004 ce_bytes;
    "test005", test005 ce_bytes;
    "test006", test006 ce_bytes;
    "test007", test007 ce_bytes;

    "test201", test001 ce_big;
    "test202", test002 ce_big;
    "test203", test003 ce_big;
    "test204", test004 ce_big;
    "test205", test005 ce_big;
    "test206", test006 ce_big;
    "test207", test007 ce_big;
  ]

let () =
  T_frame.Main.register "FPack" tests;
  T_frame.Main.main()
  
