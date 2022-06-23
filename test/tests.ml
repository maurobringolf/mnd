let () =
  let open Alcotest in
  run "Monad"
    [
      ( "Mnd.Instances.Option",
        let open Mnd.Instances.Option in
        [
          test_case "let*" `Quick (fun () ->
              Alcotest.(check (option int))
                "let*"
                (let* x = Some 17 in
                 Some (x + 3))
                (Some 20));
          test_case "let+" `Quick (fun () ->
              Alcotest.(check (option int))
                "let+"
                (let+ x = Some 17 in
                 x + 3)
                (Some 20));
          test_case "Some >> Some" `Quick (fun () ->
              Alcotest.(check (option string))
                ">>"
                (Some "first" >> Some "second")
                (Some "second"));
          test_case "Some >> None" `Quick (fun () ->
              Alcotest.(check (option string)) ">>" (Some "first" >> None) None);
          test_case "None >> None" `Quick (fun () ->
              Alcotest.(check (option string)) ">>" (None >> None) None);
          test_case "None >> Some" `Quick (fun () ->
              Alcotest.(check (option int)) ">>" (None >> Some 17) None);
          test_case "Some |>>" `Quick (fun () ->
              Alcotest.(check (option int))
                "|>>"
                (Some 18 |>> ( + ) 1)
                (Some 19));
          test_case "None |>>" `Quick (fun () ->
              Alcotest.(check (option int)) "|>>" (None |>> ( + ) 1) None);
          test_case "mapM: some over empty list" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM" (mapM Option.some []) (Some []));
          test_case "mapM: none over empty list" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM"
                (mapM (fun _ -> None) [])
                (Some []));
          test_case "mapM: list of all somes" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM"
                (mapM Option.some [ 1; 2; 3 ])
                (Some [ 1; 2; 3 ]));
          test_case "mapM: list of all nones" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM"
                (mapM (fun _ -> None) [ 1; 2; 3 ])
                None);
          test_case "mapM: none for last element" `Quick (fun () ->
              Alcotest.(check (option (list int)))
                "mapM"
                (mapM (fun n -> if n > 2 then None else Some n) [ 1; 2; 3 ])
                None);
          test_case "foldM: some on all elements" `Quick (fun () ->
              Alcotest.(check (option int))
                "foldM"
                (foldM (fun acc x -> Some (acc + x)) 0 [ 1; 2; 3 ])
                (Some 6));
          test_case "fold1M: some on all elements" `Quick (fun () ->
              Alcotest.(check (option int))
                "fold1M"
                (fold1M (fun acc x -> Some (acc - x)) [ 1; 2; 3 ])
                (Some (-4)));
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
                "let+"
                (let+ x = return "first" in
                 x ^ "second")
                (Result.Ok "firstsecond"));
          test_case "Ok >> Ok" `Quick (fun () ->
              Alcotest.(check (result string string))
                ">>"
                (return "first" >> return "second")
                (Result.Ok "second"));
          test_case "Error >> Ok" `Quick (fun () ->
              Alcotest.(check (result string string))
                ">>"
                (error "first" >> return "second")
                (Result.Error "first"));
          test_case "Ok >> Error" `Quick (fun () ->
              Alcotest.(check (result int string))
                ">>"
                (return 17 >> error "second")
                (Result.Error "second"));
          test_case "Error >> Error" `Quick (fun () ->
              Alcotest.(check (result unit string))
                ">>"
                (error "first" >> error "second")
                (Result.Error "first"));
          test_case "Ok |>>" `Quick (fun () ->
              Alcotest.(check (result string unit))
                "|>>"
                (return 42 |>> string_of_int)
                (Result.Ok "42"));
          test_case "Error |>>" `Quick (fun () ->
              Alcotest.(check (result string int))
                "|>>"
                (error 42 |>> string_of_int)
                (Result.Error 42));
          test_case "mapM/forM: success on all elements" `Quick (fun () ->
              Alcotest.(check (result (list int) unit))
                "mapM"
                (mapM return [ 1; 2; 3 ])
                (Result.Ok [ 1; 2; 3 ]);
              Alcotest.(check (result (list int) unit))
                "forM"
                (forM [ 1; 2; 3 ] return)
                (Result.Ok [ 1; 2; 3 ]));
          test_case "mapM/forM: error on first element" `Quick (fun () ->
              Alcotest.(check (result (list int) int))
                "mapM"
                (mapM error [ 1; 2; 3 ])
                (Result.Error 1);

              Alcotest.(check (result (list int) int))
                "forM"
                (forM [ 1; 2; 3 ] error)
                (Result.Error 1));
          test_case "mapM/forM: error on intermediate element" `Quick (fun () ->
              Alcotest.(check (result (list int) string))
                "mapM"
                (mapM (function 2 -> error "two" | n -> return n) [ 1; 2; 3 ])
                (Result.Error "two");
              Alcotest.(check (result (list int) string))
                "forM"
                (forM [ 1; 2; 3 ] (function 2 -> error "two" | n -> return n))
                (Result.Error "two"));
          test_case "mapM/forM: error on last element" `Quick (fun () ->
              Alcotest.(check (result (list int) string))
                "mapM"
                (mapM
                   (function 3 -> error "three" | n -> return n)
                   [ 1; 2; 3 ])
                (Result.Error "three");
              Alcotest.(check (result (list int) string))
                "forM"
                (forM [ 1; 2; 3 ] (function
                  | 3 -> error "three"
                  | n -> return n))
                (Result.Error "three"));
          test_case "iterM: success on all elements" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "iterM"
                (iterM (fun s -> return (print_endline s)) [ "a"; "b"; "c" ])
                (Result.Ok ()));
          test_case "iterM: error on first element" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "iterM"
                (iterM (fun s -> error s) [ "a"; "b"; "c" ])
                (Result.Error "a"));
          test_case "ifM: true -> Ok -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int string))
                "ifM"
                (ifM true (fun () -> return (x := 2)) >>= fun () -> return !x)
                (Result.Ok 2));
          test_case "ifM: false -> Ok -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int string))
                "ifM"
                (ifM false (fun () -> return (x := 2)) >>= fun () -> return !x)
                (Result.Ok 1));
          test_case "ifM: true -> Error -> Error" `Quick (fun () ->
              Alcotest.(check (result unit string))
                "ifM"
                (ifM true (fun () -> error "err"))
                (Result.Error "err"));
          test_case "ifM: false -> Error -> Ok" `Quick (fun () ->
              let x = ref 1 in
              Alcotest.(check (result int unit))
                "ifM"
                (ifM false (fun () -> error (x := 2)) >>= fun () -> return !x)
                (Result.Ok 1));
          test_case "foldM: successful on all elements" `Quick (fun () ->
              Alcotest.(check (result int unit))
                "foldM"
                (foldM (fun acc x -> return (acc + x)) 0 [ 1; 2; 3 ])
                (Result.Ok 6));
          test_case "foldM: error on some element" `Quick (fun () ->
              Alcotest.(check (result int int))
                "foldM"
                (foldM
                   (fun acc x ->
                     match x with 3 -> error acc | n -> return (acc + n))
                   0 [ 1; 2; 3 ])
                (Result.Error 3));
          test_case "fold1M: successful on all elements" `Quick (fun () ->
              Alcotest.(check (result int unit))
                "fold1M"
                (fold1M (fun acc x -> return (acc + x)) [ 1; 2; 3 ])
                (Result.Ok 6));
          test_case "fold1M: error on some element" `Quick (fun () ->
              Alcotest.(check (result int int))
                "fold1M"
                (fold1M
                   (fun acc x ->
                     match x with 3 -> error acc | n -> return (acc + n))
                   [ 1; 2; 3 ])
                (Result.Error 3));
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
                "run"
                (run 0
                @@ let* x = return "first" in
                   let* n = get in
                   let* () = put (n + 8) in
                   let+ n = get in
                   x ^ string_of_int n)
                "first8");
        ] );
      ( "Mnd.Instances.Reader",
        let open Mnd.Instances.Reader in
        [
          test_case "Reader" `Quick (fun () ->
              let flip f x y = f y x in
              Alcotest.(check (list (option int)))
                "run"
                (run [ 1; 2 ]
                @@ let* x = read |>> flip List.nth_opt 0 in
                   let* y = read |>> flip List.nth_opt 1 in
                   let* z = read |>> flip List.nth_opt 2 in
                   return [ x; y; z ])
                [ Some 1; Some 2; None ]);
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
                (let* x = return 17 in
                 let* () = write @@ [ "log" ^ string_of_int x ] in
                 let* y = return (x + 8) in
                 let+ () = write [ "another log"; "last one" ] in
                 y * 3)
                (75, [ "log17"; "another log"; "last one" ]));
        ] );
    ]
