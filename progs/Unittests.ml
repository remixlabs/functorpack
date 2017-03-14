open FPack

let test001() =
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
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'

let test002() =
  let j = `Assoc [] in
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'

let test003() =
  let j =
    `Assoc [ "c", `Int 7;
             "d", `Int 5;
             "b", `Int 6;
             "a", `Int 10
           ] in
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'

let test004() =
  let j =
    `Assoc [ "a", `List [ `Int 56; `String "abc"; `Null ];
             "b", `List [ `Null; `String "abc"; `Int 56; ];
             "c", `List (Array.to_list (Array.make 20000 (`Int 42)));
             "d", `List (Array.to_list (Array.make 200000 (`Int 42)));
           ] in
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'

let test005() =
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
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j1 = j'

let test006() =
  let j = `Intlit "9223372036854775807" in  (* = Int64.max_int *)
  let by = Yojson.Safe.compose_bytes j in
(*
  for i = 0 to Bytes.length by - 1 do
    Printf.printf "by[%d] = %d\n%!" i (Char.code (Bytes.get by i))
  done;
 *)
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'

let test007() =
  let j = `Intlit "-9223372036854775808" in  (* = Int64.min_int *)
  let by = Yojson.Safe.compose_bytes j in
  let j' = Yojson.Safe.extract_bytes by 0 (Bytes.length by) in
  j = j'
  

let tests =
  [ "test001", test001;
    "test002", test002;
    "test003", test003;
    "test004", test004;
    "test005", test005;
    "test006", test006;
    "test007", test007;
  ]

let () =
  T_frame.Main.register "FPack" tests;
  T_frame.Main.main()
  
