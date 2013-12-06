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
     redinvinc = 0;
     blueinvinc = 0;
     (*version 4 only*)
     redpower = 0;
     bluepower = 0;
     ufotimer = 0;
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


(*For Bomb:    
  Remove any bullets that the invincible user grazes during duration of bomb
  No charge accumulated
*)
  let (redhead, redtail) = 
    match game.redmove with 
    |h::t -> (h, t)
    |[] -> ((Neutral, Neutral), []) in
  let (bluehead, bluetail) =
    match game.bluemove with
    |h::t -> (h, t)
    |[] -> ((Neutral, Neutral), []) in
  let redp : player_char = 
    {p_id = game.redid; 
     (*red player position updated*)
     p_pos = move_player game.redpos redhead game.redfoc; 
     p_focused = game.redfoc;
     p_radius = cHITBOX_RADIUS;
     p_color = Red} in
  let bluep : player_char = 
    {p_id = game.blueid; 
     (*blue player position updated*)
     p_pos = move_player game.bluepos bluehead game.bluefoc;
     p_focused = game.bluefoc;
     p_radius = cHITBOX_RADIUS;
     p_color = Blue} in
  let ui : update_info = 
    {red = redp;
     blue = bluep;
     rinvincible = if game.redinvinc <> 0 then true else false;
     binvincible = if game.blueinvinc <> 0 then true else false;
     blst = game.bullets;
     ufolst = game.ufos;
     pwrlst = game.powers;
   } in 
  let urecord = update_all ui in 
  let updated =
    {game with
     redmove = redtail;
     bluemove = bluetail;

     redpower = newpower game.redpower urecord.rlost urecord.rpower_pts;
     bluepower = newpower game.bluepower urecord.blost urecord.bpower_pts;

     redcharge = 
     charge game.redcharge game.redpower urecord.rlost urecord.rpower_pts;
     bluecharge = 
     charge game.bluecharge game.bluepower urecord.blost urecord.bpower_pts;

     redpos = redp.p_pos;
     bluepos = bluep.p_pos;
     
     redlife = newlife game.redlife urecord.rlost;
     bluelife = newlife game.bluelife urecord.blost;
     
     redscore = newscore urecord.blost urecord.rgraze_pts 
       urecord.rpower_pts game.redscore;
     bluescore = newscore urecord.rlost urecord.bgraze_pts
       urecord.bpower_pts game.bluescore;
     
     redinvinc = newinvinc urecord.rlost game.redinvinc;
     blueinvinc = newinvinc urecord.blost game.blueinvinc;
     
     bullets = urecord.bullet_lst;
     timer = game.timer -. cUPDATE_TIME;
     
     (*Version 4*)
     ufos = urecord.ulst;
     powers = urecord.powerlst
   } in
  add_update (MovePlayer (updated.redid, updated.redpos));
  add_update (MovePlayer (updated.blueid, updated.bluepos));
  add_update (SetLives (Red, updated.redlife));
  add_update (SetLives (Blue, updated.bluelife));
  add_update (SetScore (Red, updated.redscore));
  add_update (SetScore (Blue, updated.bluescore));
  add_update (SetPower (Red, updated.redpower));
  add_update (SetPower (Blue, updated.bluepower));
  add_update (SetCharge (Red, updated.redcharge));
  add_update (SetCharge (Blue, updated.bluecharge));
	    
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
      if game.bluebombs = 0 then game
      else
	let updated = 
	  {game with 
	   bullets = [];
	   blueinvinc = cBOMB_DURATION;
	   bluebombs = game.bluebombs - 1
	 } in 
	delete_all game.bullets;
	add_update (UseBomb (Blue));
	add_update (SetBombs (Blue, updated.bluebombs));
	updated
  |Red, Bomb -> 
      if game.redbombs = 0 then game
      else
	let updated = 
	  {game with 
	   bullets = [];
	   redinvinc = cBOMB_DURATION;
	   redbombs = game.redbombs - 1
	 } in 
	delete_all game.bullets;
	add_update (UseBomb (Red));
	add_update (SetBombs (Red, updated.redbombs));
	updated


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
