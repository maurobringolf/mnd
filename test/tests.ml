module CustomOptionMonad = Mnd.Make (struct
  type 'a t = 'a option

  let return _ = None
  let bind _ _ = None
  let map _ _ = None
end)

module CustomResultMonad = Mnd.Make2 (struct
  type ('a, 'b) t = ('a, 'b) result

  let return = Result.ok
  let bind = Result.bind
  let map = Result.map
end)

let () =
  let open Alcotest in
  run "Monad"
    [
      ( "Mnd.Make",
        [
          test_case "Mnd.Make: custom option instance" `Quick (fun () ->
              Alcotest.(check (option int))
                "let*" None
                (let open CustomOptionMonad in
                let* x = Some 17 in
                Some (x + 3)));
        ] );
      ( "Mnd.Make2",
        [
          test_case "Mnd.Make2: custom result instance" `Quick (fun () ->
              Alcotest.(check (result int string))
                "let*" (Result.Ok 21)
                (let open CustomResultMonad in
                let* x = return 17 in
                return (4 + x)));
        ] );
      ( "Mnd.Instances.Option",
        let open Mnd.Instances.Option in
        [
          test_case "let*" `Quick (fun () ->
              Alcotest.(check (option int))
                "let*" (Some 20)
                (let* x = Some 17 in
                 Some (x + 3)));
          test_case "let+" `Quick (fun () ->
              Alcotest.(check (option int))
                "let+" (Some 20)
                (let+ x = Some 17 in
                 x + 3));
          test_case "Some >> Some" `Quick (fun () ->
              Alcotest.(check (option string))
                ">>" (Some "second")
                (Some "first" >> Some "second"));
          test_case "Some >> None" `Quick (fun () ->
              Alcotest.(check (option string)) ">>" None (Some "first" >> None));
          test_case "None >> None" `Quick (fun () ->
              Alcotest.(check (option string)) ">>" None (None >> None));
          test_case "None >> Some" `Quick (fun () ->
              Alcotest.(check (option int)) ">>" None (None >> Some 17));
          test_case "Some |>>" `Quick (fun () ->
              Alcotest.(check (option int)) "|>>" (Some 19) (Some 18 |>> ( + ) 1));
          test_case "None |>>" `Quick (fun () ->
              Alcotest.(check (option int)) "|>>" None (None |>> ( + ) 1));
          test_case "mapM: some over empty list" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM" (Some []) (mapM Option.some []));
          test_case "mapM: none over empty list" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM" (Some [])
                (mapM (fun _ -> None) []));
          test_case "mapM: list of all somes" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM"
                (Some [ 1; 2; 3 ])
                (mapM Option.some [ 1; 2; 3 ]));
          test_case "mapM: list of all nones" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM" None
                (mapM (fun _ -> None) [ 1; 2; 3 ]));
          test_case "mapM: none for last element" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM" None
                (mapM (fun n -> if n > 2 then None else Some n) [ 1; 2; 3 ]));
          test_case "foldM: some on all elements" `Quick (fun () ->
              Alcotest.(check (option int))
                "foldM" (Some 6)
                (foldM (fun acc x -> Some (acc + x)) 0 [ 1; 2; 3 ]));
          test_case "fold1M: some on all elements" `Quick (fun () ->
              Alcotest.(check (option int))
                "fold1M" (Some (-4))
                (fold1M (fun acc x -> Some (acc - x)) [ 1; 2; 3 ]));
          test_case "fold1M: raises on empty list" `Quick (fun () ->
              Alcotest.check_raises "fold1M"
                (Invalid_argument "fold1M of empty list") (fun () ->
                  ignore (fold1M (fun _ x -> Some x) [])));
        ] );
      ( "Mnd.Instances.Result",
        let open Mnd.Instances.Result in
        [
          test_case "let+" `Quick (fun () ->
              Alcotest.(check (result string string))
                "let+" (Result.Ok "firstsecond")
                (let+ x = return "first" in
                 x ^ "second"));
          test_case "and+" `Quick (fun () ->
              Alcotest.(check (result string string))
                "and+" (Result.Error "second")
                (let+ x = return "first" and+ y = error "second" in
                 x ^ y));
          test_case "and+" `Quick (fun () ->
              Alcotest.(check (result string string))
                "and+" (Result.Error "first")
                (let+ x = error "first" and+ y = error "second" in
                 x ^ y));
          test_case "and+" `Quick (fun () ->
              Alcotest.(check (result string string))
                "and+" (Result.Ok "firstsecond")
                (let+ x = return "first" and+ y = return "second" in
                 x ^ y));
          test_case "Ok >> Ok" `Quick (fun () ->
              Alcotest.(check (result string string))
                ">>" (Result.Ok "second")
                (return "first" >> return "second"));
          test_case "Error >> Ok" `Quick (fun () ->
              Alcotest.(check (result string string))
                ">>" (Result.Error "first")
                (error "first" >> return "second"));
          test_case "Ok >> Error" `Quick (fun () ->
              Alcotest.(check (result int string))
                ">>" (Result.Error "second")
                (return 17 >> error "second"));
          test_case "Error >> Error" `Quick (fun () ->
              Alcotest.(check (result unit string))
                ">>" (Result.Error "first")
                (error "first" >> error "second"));
          test_case "Ok |>>" `Quick (fun () ->
              Alcotest.(check (result string unit))
                "|>>" (Result.Ok "42")
                (return 42 |>> string_of_int));
          test_case "Error |>>" `Quick (fun () ->
              Alcotest.(check (result string int))
                "|>>" (Result.Error 42)
                (error 42 |>> string_of_int));
          test_case "mapM/forM: success on all elements" `Quick (fun () ->
              Alcotest.(check (result (list int) unit))
                "mapM"
                (Result.Ok [ 1; 2; 3 ])
                (mapM return [ 1; 2; 3 ]);
              Alcotest.(check (result (list int) unit))
                "forM"
                (Result.Ok [ 1; 2; 3 ])
                (forM [ 1; 2; 3 ] return));
          test_case "mapM/forM: error on first element" `Quick (fun () ->
              Alcotest.(check (result (list int) int))
                "mapM" (Result.Error 1)
                (mapM error [ 1; 2; 3 ]);

              Alcotest.(check (result (list int) int))
                "forM" (Result.Error 1)
                (forM [ 1; 2; 3 ] error));
          test_case "mapM/forM: error on intermediate element" `Quick (fun () ->
              Alcotest.(check (result (list int) string))
                "mapM" (Result.Error "two")
                (mapM (function 2 -> error "two" | n -> return n) [ 1; 2; 3 ]);
              Alcotest.(check (result (list int) string))
                "forM" (Result.Error "two")
                (forM [ 1; 2; 3 ] (function 2 -> error "two" | n -> return n)));
          test_case "mapM/forM: error on last element" `Quick (fun () ->
              Alcotest.(check (result (list int) string))
                "mapM" (Result.Error "three")
                (mapM
                   (function 3 -> error "three" | n -> return n)
                   [ 1; 2; 3 ]);
              Alcotest.(check (result (list int) string))
                "forM" (Result.Error "three")
                (forM [ 1; 2; 3 ] (function
                  | 3 -> error "three"
                  | n -> return n)));
          test_case "iterM: success on all elements" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "iterM" (Result.Ok ())
                (iterM (fun s -> return (print_endline s)) [ "a"; "b"; "c" ]));
          test_case "iterM: error on first element" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "iterM" (Result.Error "a")
                (iterM (fun s -> error s) [ "a"; "b"; "c" ]));
          test_case "ifM: true -> Ok -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int string))
                "ifM" (Result.Ok 2)
                (ifM true (fun () -> return (x := 2)) >>= fun () -> return !x));
          test_case "ifM: false -> Ok -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int string))
                "ifM" (Result.Ok 1)
                (ifM false (fun () -> return (x := 2)) >>= fun () -> return !x));
          test_case "ifM: true -> Error -> Error" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "ifM" (Result.Error "err")
                (ifM true (fun () -> error "err")));
          test_case "ifM: false -> Error -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int unit))
                "ifM" (Result.Ok 1)
                (ifM false (fun () -> error (x := 2)) >>= fun () -> return !x));
          test_case "foldM: successful on all elements" `Quick (fun () ->
              Alcotest.(check (result int unit))
                "foldM" (Result.Ok 6)
                (foldM (fun acc x -> return (acc + x)) 0 [ 1; 2; 3 ]));
          test_case "foldM: error on some element" `Quick (fun () ->
              Alcotest.(check (result int int))
                "foldM" (Result.Error 3)
                (foldM
                   (fun acc x ->
                     match x with 3 -> error acc | n -> return (acc + n))
                   0 [ 1; 2; 3 ]));
          test_case "fold1M: successful on all elements" `Quick (fun () ->
              Alcotest.(check (result int unit))
                "fold1M" (Result.Ok 6)
                (fold1M (fun acc x -> return (acc + x)) [ 1; 2; 3 ]));
          test_case "fold1M: error on some element" `Quick (fun () ->
              Alcotest.(check (result int int))
                "fold1M" (Result.Error 3)
                (fold1M
                   (fun acc x ->
                     match x with 3 -> error acc | n -> return (acc + n))
                   [ 1; 2; 3 ]));
          test_case "fold1M: raises on empty list" `Quick (fun () ->
              Alcotest.check_raises "fold1M"
                (Invalid_argument "fold1M of empty list") (fun () ->
                  ignore (fold1M (fun _ x -> return x) [])));
        ] );
      ( "Mnd.Instances.State",
        let open Mnd.Instances.State in
        [
          test_case "State" `Quick (fun () ->
              Alcotest.(check string)
                "run" "first8"
                (run 0
                @@ let* x = return "first" in
                   let* n = get in
                   let* () = put (n + 8) in
                   let+ n = get in
                   x ^ string_of_int n));
        ] );
      ( "Mnd.Instances.Reader",
        let open Mnd.Instances.Reader in
        [
          test_case "Reader" `Quick (fun () ->
              let flip f x y = f y x in
              Alcotest.(check (list (option int)))
                "run" [ Some 1; Some 2; None ]
                (run [ 1; 2 ]
                @@ let* x = read |>> flip List.nth_opt 0 in
                   let* y = read |>> flip List.nth_opt 1 in
                   let* z = read |>> flip List.nth_opt 2 in
                   return [ x; y; z ]));
        ] );
      ( "Mnd.Instances.Writer",
        let open Mnd.Instances.Writer (struct
          type t = string list

          let mempty = []
          let mappend = ( @ )
        end) in
        [
          test_case "Writer" `Quick (fun () ->
              Alcotest.(check (pair int (list string)))
                "run"
                (75, [ "log17"; "another log"; "last one" ])
                (let* x = return 17 in
                 let* () = write @@ [ "log" ^ string_of_int x ] in
                 let* y = return (x + 8) in
                 let+ () = write [ "another log"; "last one" ] in
                 y * 3));
        ] );
    ]
