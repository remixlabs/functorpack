open FPack
open T_frame.Util

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
  

module F = Figly.Make(Figly.Data)


let test100() =
  let open Figly.Data in
  let s = "\135\163age!\164city\169palo alto\164name\165vijay\165:type\164user\165group\166{ref}5\169:figly/id\1617\170:full-name\178Vijay Chakravarthy" in
  let b = Bytes.of_string s in
  let d = F.extract_bytes b 0 (Bytes.length b) in
  match d with
    | Dobject o ->
        SMap.bindings o =
          [ ":figly/id", Dstring "7";
            ":full-name", Dstring "Vijay Chakravarthy";
            ":type", Dstring "user";
            "age", Dint 33L;
            "city", Dstring "palo alto";
            "group", Dref "5";
            "name", Dstring "vijay";
          ]
    | _ ->
        false

let bytes_of_list l =
  let n = List.length l in
  let b = Bytes.create n in
  List.iteri (fun i x -> Bytes.set b i (Char.chr x)) l;
  b

let test101() =
  (* some test cases *)
  let open Figly.Data in
  let okl =
    List.map
      (fun (name, data, encoded) ->
        let b1 = F.compose_bytes data in
        let b2 = bytes_of_list encoded in
        let ok1 = (b1 = b2) in
        let data' = F.extract_bytes b1 0 (Bytes.length b1) in
        let ok2 = (data = data') in
        named (name ^ "_1") ok1 &&& named(name ^ "_2") ok2
      )
      [ "scalar0", Dnull, [0xc0];
        "scalar1", Dfloat 1.5, [0xcb; 0x3f; 0xf8; 0; 0; 0; 0; 0; 0];
        "scalar2", Dbool true, [0xc3];
        "scalar3", Dbool false, [0xc2];
        "scalar4", Dint 2L, [2];
        "scalar5", Dstring "foo", [0xa3; 102; 111; 111];
        "scalar6", Dstring "{bar}", [0xaa; 123; 115; 116; 114; 125; 123; 98; 97; 114; 125];
        "scalar7", Dbinary "foo", [0xc4; 3; 102; 111; 111];
        "scalar8", Dbinary "{bar}", [0xc4; 5; 123; 98; 97; 114; 125];
        "scalar9", Dref "12038", [0xaa; 123; 114; 101; 102; 125; 49; 50; 48; 51; 56]
      ] in
  List.for_all (fun ok -> ok) okl


let tests =
  [ "test001", test001;
    "test002", test002;
    "test003", test003;
    "test004", test004;
    "test005", test005;
    "test006", test006;
    "test007", test007;
    "test100", test100;
    "test101", test101;
  ]

let () =
  T_frame.Main.register "FPack" tests;
  T_frame.Main.main()
  
