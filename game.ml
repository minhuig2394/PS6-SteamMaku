open Definitions
open Constants
open Util

type game = 
    {redlife : int;
     redbombs : int;
     redscore : int;
     redpower : int;
     redcharge : int;
     redpos : int;
     redfoc : bool;
     bluelife : int;
     bluebombs : int;
     bluescore : int;
     bluepower : int;
     bluecharge : int;
     bluepos : int;
     bluefoc : bool;
     ufotimer : int;
     numufo : int;
     numbullet : int;
     numpower : int;
     ufopos : (int * int) list;
     bulletpos : (int * int) list;
     powerpos : (int * int) list;
     timer : int;
   }


let init_game () : game =
  failwith "U.N. Owen wasn't her"

let handle_time game =
  failwith "Charisma BREAK"

let handle_action game col act =
  failwith "A myon sort of day"

let get_data game =
  failwith "I'm the strongest!"

