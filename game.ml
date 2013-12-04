open Definitions
open Constants
open Util
open Netgraphics

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
     redbullets : bullet list;
     bluebullets : bullet list;
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
     numpower : int;
     ufos : ufo list;
     powers : power list;
   }


let init_game () : game =
  let init : game = 
    {redid = 1;
     blueid = 2;
     redlife = cINITIAL_LIVES;
     redscore = 0;
     redcharge = 0;
     redpos = 
     ((float_of_int c_BOARD_WIDTH) /. 8., 
      (float_of_int cBOARD_HEIGHT) /. 2.);
     redfoc = false;
     bluelife = cINITIAL_LIVES;
     bluescore = 0;
     bluecharge = 0;
     bluepos =
     (((float_of_int c_BOARD_WIDTH) /. 8.) *. 7., 
      (float_of_int cBOARD_HEIGHT) /. 2.)
     bluefoc = false;
     redbullets = [];
     bluebullets = [];
     timer = cTIME_LIMIT;
     (*version 2 only*)
     redbombs = cINITIAL_BOMBS;
     bluebombs = cINITIAL_BOMBS;
     redinvinc = cBOMB_DURATION;
     blueinvinc = cBOMB_DURATION;
     (*version 4 only*)
     redpower = 0;
     bluepower = 0;
     ufotimer : int;
     numpower = 0;
     ufos = [];
     powers = [];
   } in
  add_update (AddPlayer (init.redid, Red, init.redpos));
  add_update (AddPlayer (init.blueid, Blue, init.bluepos));
  add_update (SetBomb (Red, init.redbombs));
  add_update (SetBomb (Blue, init.bluebombs));
  add_update (SetLives (Red, init.redlife));
  add_update (SetLives (Blue, init.bluelife));
  add_update (SetScore (Red, init.redscore));
  add_update (SetScore (Blue, init.bluescore));
  add_update (SetPower (Red, init.redpower));
  add_update (SetPower (Blue, init.bluepower));
  add_update (SetCharge (Red, init.redcharge));
  add_update (SetCharge (Blue, init.bluecharge));
  init

let handle_time game =
  (*check if red player has no more life
    check if blue player has no more life
    check if red or blue player has more points if no more time*)
  failwith "not implemented"

let handle_action game col act =
  match col, act with
  |Blue, Move _ -> game
  |Red, Move _ -> game
  |Blue, Shoot (b, p, a) -> game
  |Red, Shoot (b, p, a)-> game
  |Blue, Focus x ->
      let updated = 
	{game with 
	 redfocus = x
       } in updated
  |Red, Focus x -> 
      let updated = 
	{game with 
	 redfocus = x
       } in updated
  |Blue, Bomb -> 
      let updated = 
	{game with 
	 redbullets = []; 
	 bluebullets = [];
	 blueinvinc = cBomb_Duration;
	 bluebombs = game.bluebombs - 1
       } in 
  add_update (UseBomb (Blue));
  updated
  |Red, Bomb -> 
      let updated = 
	{game with 
	 redbullets = []; 
	 bluebullets = [];
	 redinvinc = cBomb_Duration;
	 redbombs = game.redbombs - 1
       } in 
  add_update (UseBomb (Red));
  updated
(*action -> Move of (direction * direction) list
            Shoot of (bullet_type * position * acceleration
            bullet_type can be Spread | Bubble | Power | Trail*)

(*For Move: Need to use MovePlayer (id, pos) graphic update

  For Shoot: Deplete Charge
  Change bullet list

  For Bomb:    
  Remove any bullets that the invincible user grazes during duration of bomb
  No charge accumulated
*)


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
  let totalbullets : bullet list = 
    List.append redbullets bluebullets in
  let data : game_data = 
    red * blue * game.ufos * totalbullets * game.powers
