open Printf

module Util = struct
  let ( &&& ) x y = x && y
  let ( ||| ) x y = x || y

  let named n b =
    if not b then eprintf "[fail: %s] %!" n;
    b
end

type tester = string * (unit -> bool)

module Main = struct
  let testers = ref []

  let register prefix (l : tester list) =
    let l' =
      List.map
        (fun (n,f) -> (prefix ^ "." ^ n, f))
        l in
    testers := !testers @ l'


  let dot_re = Str.regexp "[.]"

  let matches pat name =
    let rec loop pl nl =
      match pl, nl with
        | "*" :: _, _ -> true
        | p :: pl', n :: nl' -> p=n && loop pl' nl'
        | [], [] -> true
        | _ -> false in
    let pl = Str.split dot_re pat in
    let nl = Str.split dot_re name in
    loop pl nl


  let main() =
    Printexc.record_backtrace true;
    let p_list = ref false in
    let pats = ref [] in
    Arg.parse
      [ "-list", Arg.Set p_list,
        "  Output a list of tests";
      ]
      (fun pat -> pats := pat :: !pats)
      (sprintf "usage: %s [pattern]" Sys.argv.(0));
    let n_selected = ref 0 in
    let n_pass = ref 0 in
    let n_fail = ref 0 in
    let n_exn = ref 0 in
    if not !p_list && !pats = [] then
      pats := List.map fst !testers;
    List.iter
      (fun (name, f) ->
         if !p_list then
           print_endline name;
         if !pats <> [] then (
           if List.exists (fun pat -> matches pat name) !pats then (
             incr n_selected;
             printf "Test %s: %!" name;
             ( try
                 let ok = f() in
                 if ok then (
                   printf "passed\n%!"; incr n_pass
                 ) else (
                   printf "FAILED\n%!"; incr n_fail
                 )
               with
                 | exn ->
                     incr n_exn;
                     let bt = Printexc.get_backtrace() in
                     printf "FAILED WITH EXCEPTION: %s\n%s\n%!"
                            (Printexc.to_string exn)
                            bt
             )
           )
         )
      )
      !testers;
    if !pats <> [] then (
      printf "SUMMARY: %d total, %d pass, %d fail, %d with exception\n%!"
             !n_selected !n_pass !n_fail !n_exn;
      if !n_fail > 0 || !n_exn > 0 then
        exit 1
    );
    exit 0
end
