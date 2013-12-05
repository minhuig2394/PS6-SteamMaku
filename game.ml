open Definitions
open Constants
open Util
open Netgraphics
open Player
open Ufo
open Bullet

type game = 
    {redid : int;
     blueid : int;
     redlife : int;
     redscore : int;
     redcharge : int;
     redpos : position;
     redmove : (direction * direction) list;
     bluemove : (direction * direction) list;
     redfoc : bool;
     bluelife : int;
     bluescore : int;
     bluecharge : int;
     bluepos : position;
     bluefoc : bool;
     bullets : bullet list;
     timer : float;
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
  let width = float_of_int cBOARD_WIDTH in
  let height = float_of_int cBOARD_HEIGHT in
  let init : game = 
    {redid = 1;
     blueid = 2;
     redlife = cINITIAL_LIVES;
     redscore = 0;
     redcharge = 0;
     redpos = 
     (width /. 8., height /. 2.);
     redmove = [];
     bluemove = [];
     redfoc = false;
     bluelife = cINITIAL_LIVES;
     bluescore = 0;
     bluecharge = 0;
     bluepos =
     ((width /. 8.) *. 7., height /. 2.);
     bluefoc = false;
     bullets = [];
     timer = cTIME_LIMIT;
     (*version 2 only*)
     redbombs = cINITIAL_BOMBS;
     bluebombs = cINITIAL_BOMBS;
     redinvinc = cBOMB_DURATION;
     blueinvinc = cBOMB_DURATION;
     (*version 4 only*)
     redpower = 0;
     bluepower = 0;
     ufotimer = 0;
     numpower = 0;
     ufos = [];
     powers = [];
   } in
  add_update (AddPlayer (init.redid, Red, init.redpos));
  add_update (AddPlayer (init.blueid, Blue, init.bluepos));
  add_update (SetBombs (Red, init.redbombs));
  add_update (SetBombs (Blue, init.bluebombs));
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
  (*1. Update positions and velocities of all bullets + UFOs
    2. Update positions of all players (depending on normal or focused)
    3. Compile a list of all bullet/player collisions
    and all bullet/UFO collisions simultaneously
    3a. process a hit on each UFO for each collision 
    - If UFO was destroyed, remove it and add powers as discussed
    3b. if either player was hit (while not invincible), 
    deduct a life from their total, refill their bomb stock, 
    clear the screen of bullets 
    (If both players were hit, do the same for both). 
    Do not deduct more than 1 life in one timestep
    5. check for player/power collision + process power collection
   *)
  let (redhead, redtail) = 
    match game.redmove with 
    |h::t -> (h, t)
    |[] -> ((Neutral, Neutral), []) in
  let (bluehead, bluetail) =
    match game.bluemove with
    |h::t -> (h, t)
    |[] -> ((Neutral, Neutral), []) in
  let updated =
    {game with
     redmove = redtail;
     bluemove = bluetail;
     redpos = if redhead = (Neutral, Neutral) then game.redpos 
     else
       move_player game.redpos redhead game.redfoc;
     bluepos = if bluehead = (Neutral, Neutral) then game.bluepos
     else 
       move_player game.bluepos bluehead game.bluefoc;
     timer = game.timer -. cUPDATE_TIME} in
  add_update (MovePlayer (updated.redid, updated.redpos));
  add_update (MovePlayer (updated.blueid, updated.bluepos));
  let r = (g_result updated.redlife updated.bluelife updated.redscore
      updated.bluescore updated.timer) in
  match r with
  |Unfinished -> 
      (updated, Unfinished)
  |_ -> 
      add_update (GameOver r);
      (updated, r)


let handle_action game col act =
  match col, act with
  |Blue, Move m -> 
      let updated = 
	{game with
	 bluemove = m} in updated
  |Red, Move m -> 
      let updated =
	{game with
	 redmove = m} in updated
  |Blue, Shoot (b, p, a) -> 
      let (nbullets, c) = shoot b p game.bluepos  a game.redcharge Blue in
      let updated = 
	{game with
	 bullets = List.append game.bullets nbullets;
	 bluecharge = game.bluecharge - c} in 
      add_update (SetCharge (Blue, updated.bluecharge));
      updated
  |Red, Shoot (b, p, a)-> 
      let (nbullets, c) = shoot b p game.redpos a game.redcharge Red in
      let updated = 
	{game with
	 bullets = List.append game.bullets nbullets;
	 redcharge = game.redcharge - c} in 
      add_update (SetCharge (Red, updated.redcharge));
      updated
  |Blue, Focus x ->
      let updated = 
	{game with 
	 bluefoc = x
       } in updated
  |Red, Focus x -> 
      let updated = 
	{game with 
	 redfoc = x
       } in updated
  |Blue, Bomb -> 
      let updated = 
	{game with 
	 bullets = [];
	 blueinvinc = cBOMB_DURATION;
	 bluebombs = game.bluebombs - 1
       } in 
  add_update (UseBomb (Blue));
  add_update (SetBombs (Blue, updated.bluebombs));
  updated
  |Red, Bomb -> 
      let updated = 
	{game with 
	 bullets = [];
	 redinvinc = cBOMB_DURATION;
	 redbombs = game.redbombs - 1
       } in 
  add_update (UseBomb (Red));
  add_update (SetBombs (Red, updated.redbombs));
  updated

(*For Bomb:    
  Remove any bullets that the invincible user grazes during duration of bomb
  No charge accumulated
*)


let get_data game =
  let redp : player_char = 
    {p_id = game.redid; 
     p_pos = game.redpos; 
     p_focused = game.redfoc;
     p_radius = cHITBOX_RADIUS;
     p_color = Red} in
  let bluep : player_char = 
    {p_id = game.blueid; 
     p_pos = game.bluepos; 
     p_focused = game.bluefoc;
     p_radius = cHITBOX_RADIUS;
     p_color = Blue} in
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
  let data : game_data = 
    (red, blue, game.ufos, game.bullets, game.powers) in data
