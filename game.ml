open Definitions
open Constants
open Util

type game = 
    {redid : int;
     blueid : int;
     redlife : int;
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
     (*version 2 only*)
     redbombs : int;
     bluebombs : int;
     redinvinc : int;
     blueinvinc : int;
     (*version 4 only*)
     redpower : int;
     bluepower : int;
     ufotimer : int;
     numufo : int;
     numpower : int;
     ufopos : position list;
     powerpos : power list;
   }


let init_game () : game =
  let init : game = 
    {init.redid = 1;
     init.blueid = 2;
     init.redlife = cINITIAL_LIVES;
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
     (*version 2 only*)
     init.redbombs = cINITIAL_BOMBS;
     init.bluebombs = cINITIAL_BOMBS;
     init.redinvinc = cBOMB_DURATION;
     init.blueinvinc = cBOMB_DURATION;
     (*version 4 only*)
     init.redpower = 0;
     init.bluepower = 0;
     init.ufotimer : int;
     init.numufo = 0;
     init.numpower = 0;
     init.ufopos = [];
     init.powerpos = [];
   } in
  init

let handle_time game =
  failwith "Charisma BREAK"

let handle_action game col act =
  failwith "A myon sort of day"

let get_data game =
  let redp : player_char = 
    (p_id = game.redid; 
     p_pos = game.redpos; 
     p_focused = game.redfoc;
     p_radius = cHITBOX_RADIUS;
     p_color = RED;} in
  let bluep : player_char = 
    {p_id = game.blueid; 
     p_pos = game.bluepos; 
     p_focused = game.bluefoc;
     p_radius = cHITBOX_RADIUS;
     p_color = BLUE;} in
  let red : team_data = 
  (game.redlife, 
   game.redbombs, 
   game.redscore, 
   game.redpower, 
   game.redcharge, 
   redp) in
  let blue : team_data = 
  (game.bluelife, 
   game.bluebombs, 
   game.bluescore, 
   game.bluepower, 
   game.bluecharge, 
   bluep) in
  let ufos : ufo list = [] in (*temporary until implemented*)
  let bullets : bullet list = [] in 
  let powers : power list = [] in (*temporary until implemented*)
  let data : game_data = 
    red * blue * ufos * bullets * powers
