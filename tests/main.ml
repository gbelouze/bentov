let () = Printexc.record_backtrace true

let () =
  Random.init 43;
  let t = Sys.time () in
  let h = Mybentov.create 100 in
  for _i = 1 to 10_000_000 do
    Mybentov.add (Random.float 1.) h
  done;
  Mybentov.bins h |> Fmt.pr "@[%a@]@." (Fmt.list @@ Repr.pp @@ Mybentov.bin_t);

  Fmt.pr "[Total Mybentov %fs]@." (Sys.time () -. t)

let convert (bin : Bentov.bin) : Mybentov.bin = { center = bin.center; count = bin.count }

let () =
  Random.init 43;
  let t = Sys.time () in
  let h = ref (Bentov.create 100) in
  for _i = 1 to 10_000_000 do
    h := Bentov.add (Random.float 1.) !h
  done;
  Bentov.bins !h |> List.map convert |> Fmt.pr "@[%a@]@." (Fmt.list @@ Repr.pp @@ Mybentov.bin_t);

  Fmt.pr "[Total Bentov %fs]@." (Sys.time () -. t)
