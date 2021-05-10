type mut_bin = { mutable mcenter : float; mutable mcount : int }

type bin = { center : float; count : int } [@@deriving repr]

type histogram = { max_bins : int; bins : mut_bin array; mutable total_count : int }

let bins h =
  h.bins
  |> Array.to_list
  |> List.map (fun { mcenter; mcount } -> { center = mcenter; count = mcount })

let bin_search arr ?ceil x =
  (* find [i] such that [arr.(i-1).mcenter <= x < arr.(i).mcenter] *)
  let rec aux i j =
    if j - i <= 1 then if x < arr.(i).mcenter then i else j
    else
      let mid = i + ((j - i) / 2) in
      if x < arr.(mid).mcenter then aux i mid else aux mid j
  in
  aux 0 (match ceil with None -> Array.length arr | Some index -> index)

let arg_merge bins center insert_at =
  (* compute the index of the bin that will be merged with the one below it, after [{mcenter, mcount=1}] is inserted at [inserted_at] *)
  let argmin = ref `None in
  let min_diff = ref (bins.(1).mcenter -. bins.(0).mcenter) in
  for i = 0 to Array.length bins - 1 do
    let diff, index =
      match i - insert_at with
      | 0 -> (bins.(i).mcenter -. center, `Inserted_above insert_at)
      | -1 -> (center -. bins.(i).mcenter, `Inserted_below insert_at)
      | x when x < 0 -> (bins.(i + 1).mcenter -. bins.(i).mcenter, `Below i)
      | _ -> (bins.(i).mcenter -. bins.(i - 1).mcenter, `Above (i - 1))
    in
    if diff <= !min_diff then (
      argmin := index;
      min_diff := diff)
  done;
  !argmin

let merge src dist =
  let sum_count = src.mcount + dist.mcount in
  dist.mcenter <-
    (* weighted average of centers *)
    ((src.mcenter *. float src.mcount) +. (dist.mcenter *. float dist.mcount)) /. float sum_count;
  dist.mcount <- sum_count

let create max_bins =
  if max_bins < 2 then raise (Invalid_argument (Printf.sprintf "max_bins: %d" max_bins))
  else { max_bins; bins = Array.make max_bins { mcenter = 0.; mcount = 0 }; total_count = 0 }

let add value histogram =
  let bin = { mcenter = value; mcount = 1 } in

  (if histogram.total_count = 0 then histogram.bins.(0) <- bin
  else if histogram.total_count < histogram.max_bins then (
    let insert_at = bin_search histogram.bins ~ceil:histogram.total_count value in
    Array.blit histogram.bins insert_at histogram.bins (insert_at + 1)
      (histogram.total_count - insert_at);
    histogram.bins.(insert_at) <- bin)
  else
    let insert_at = bin_search histogram.bins value in
    let arg_merge = arg_merge histogram.bins value insert_at in
    match arg_merge with
    | `None -> assert false
    | `Below i ->
        merge histogram.bins.(i + 1) histogram.bins.(i);
        if insert_at = histogram.max_bins then (
          Array.blit histogram.bins (i + 2) histogram.bins (i + 1) (insert_at - i - 2);
          histogram.bins.(insert_at - 1) <- bin)
        else (
          Array.blit histogram.bins (i + 2) histogram.bins (i + 1) (insert_at - i - 1);
          histogram.bins.(insert_at) <- bin)
    | `Above i ->
        merge histogram.bins.(i) histogram.bins.(i + 1);
        Array.blit histogram.bins insert_at histogram.bins (insert_at + 1) (i - insert_at);
        histogram.bins.(insert_at) <- bin
    | `Inserted_below i -> merge bin histogram.bins.(i - 1)
    | `Inserted_above i -> merge bin histogram.bins.(i));

  histogram.total_count <- histogram.total_count + 1
