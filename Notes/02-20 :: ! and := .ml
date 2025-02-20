let c = ref 0
let n = !c
let _ = (c := 5)
let n' = !c
    
type state = Open | Closed
type door = {
  opendoor : unit -> unit ;
  closedoor : unit -> unit ;
  
  (* 
  
  EXAMPLE TEST QUESTION : 
  
  add "locked" and lock() and unlock() methods, that takes/receives a password 
  - unlock only works on locked doors, etc. 
     - string -> unit
  - lock only works in a closed door 
  
  *)
}

exception InvalidState

let make_door () = 
  let state = ref Closed in (* Initially closed *)
  let is_open () = !state = Open in (* returns bool *)
  
  {
    is_open ; (* this is equal to : is_open = is_open *)
    opendoor = (fun () -> 
        if !state = Closed then state := Open 
        else raise InvalidState
      );
    closedoor = (fun () -> 
        if !state = Open then state := Closed 
        else raise InvalidState
      );
  }
  
  
