open Definitions
open Constants
open Util

type game = 
    {redlife : int;
     redscore : int;
     redcharge : int;
     redpos : position;
     redfoc : bool;
     bluelife : int;
     bluescore : int;
     bluecharge : int;
     bluepos : position;
     bluefoc : bool;
     redbullets : (bullet_type * position * velocity) list;
     bluebullets : (bullet_type * position * velocity) list;
     timer : int;
     (*version 2 only
     redbombs : int;
     bluebombs : int;
     redinvinc : int;
     blueinvinc : int;
     version 4 only
     redpower : int;
     bluepower : int;
     ufotimer : int;
     numufo : int;
     numpower : int;
     ufopos : position list;
     powerpos : power list;*)
   }


let init_game () : game =
  let init : game = 
    {init.redlife = cINITIAL_LIVES;
     init.redscore = 0;
     init.redcharge = 0;
     init.redpos = 
     ((float_of_int c_BOARD_WIDTH) /. 8., 
      (float_of_int cBOARD_HEIGHT) /. 2.);
     init.redfoc = false;
     init.bluelife = cINITIAL_LIVES;
     init.bluescore = 0;
     init.bluecharge = 0;
     init.bluepos =
     (((float_of_int c_BOARD_WIDTH) /. 8.) *. 7., 
      (float_of_int cBOARD_HEIGHT) /. 2.)
     init.bluefoc = false;
     init.redbullets = [];
     init.bluebullets = [];
     init.timer = cTIME_LIMIT;
     (*version 2 only
     init.redbombs = cINITIAL_BOMBS;
     init.bluebombs = cINITIAL_BOMBS;
     init.redinvinc = cBOMB_DURATION;
     init.blueinvinc = cBOMB_DURATION;
     version 4 only
     init.redpower = 0;
     init.bluepower = 0;
     init.ufotimer : int;
     init.numufo = 0;
     init.numpower = 0;
     init.ufopos : position list;
     init.powerpos : position list;*)
   } in
  init

let handle_time game =
  failwith "Charisma BREAK"

let handle_action game col act =
  failwith "A myon sort of day"

let get_data game =
  failwith "I'm the strongest!"
