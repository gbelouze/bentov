type bin = {
  center : float;  (** the center of the bin *)
  count : int;  (** the number of values in the bin *)
}
[@@deriving repr]
(** [bin] represents one of the bins in a 1D histogram. The bin is centered in [center] and its mass
    is [count]. *)

type histogram

val bins : histogram -> bin list
(** [bins h] returns the list of bins, sorted by the bin center, comprising histogram [h] *)

val create : int -> histogram
(** [create max_bins] creates a histogram with up to [max_bins] bins *)

val add : float -> histogram -> unit
(** [add v h] adds a value to [v] to histogram [h] *)

val clear : histogram -> unit