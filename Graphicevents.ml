

open Core.Std
(** An Interface for events. *)
module type EVENT =
sig
  (** The event listener identifier type. *)
  type id

  (** The event type. *)
  type 'a event

  (** Create a new event. *)
  val new_event : unit -> 'a event

  (** Signal that an event has occurred.  The 'a value is passed to each
      function waiting for the event. *)
  val fire_event : 'a event -> 'a -> unit

end

module Event : EVENT =
struct

  type id = int
  type 'a waiter = {id : id ; action : 'a -> unit}
  type 'a event = 'a waiter list ref

  let id_counter = ref 0

  let new_id () : id =
    let i = !id_counter in
    incr id_counter ; i

  let new_event : unit -> 'a event = fun () -> ref []

  let fire_event (e:'a event) (v:'a) : unit =
    let waiters = !e in
    List.iter ~f:(fun w -> w.action v) waiters

end
