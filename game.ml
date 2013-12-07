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
     redbombinv : bool;
     bluebombinv : bool;
     redmercinv : bool;
     bluemercinv : bool;
     (*version 4 only*)
     redpower : int;
     bluepower : int;
     ufospawntimer : int;
     ufomovetimer : int;
     ufos : ufo list;
     powers : power list;
   }


let init_game () : game =
  let width = float_of_int cBOARD_WIDTH in
  let height = float_of_int cBOARD_HEIGHT in
  let init : game = 
    {redid = next_available_id ();
     blueid = next_available_id ();
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
     redbombinv = false;
     bluebombinv = false;
     redmercinv = false;
     bluemercinv = false;
     (*version 4 only*)
     redpower = 0;
     bluepower = 0;
     ufospawntimer = cUFO_SPAWN_INTERVAL;
     ufomovetimer = cUFO_MOVE_INTERVAL;
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
     rinvincible = game.redbombinv;
     binvincible = game.bluebombinv;
     blst = game.bullets;
     ufolst = game.ufos;
     pwrlst = game.powers;
   } in 
  let urecord = update_all ui in 
  let updated =
    {game with
     redmove = redtail;
     bluemove = bluetail;
     
     redpos = redp.p_pos;
     bluepos = bluep.p_pos;
     
     redpower = 
     	newpower urecord.rlost game.redmercinv game.redpower urecord.rpower_pts;
     bluepower = 
     	newpower urecord.blost game.bluemercinv game.bluepower urecord.bpower_pts;

     redcharge = 
     charge game.redcharge game.redpower urecord.rlost game.redmercinv
       urecord.rpower_pts game.redbombinv;
     bluecharge = 
     charge game.bluecharge game.bluepower urecord.blost game.bluemercinv
       urecord.bpower_pts game.bluebombinv;
     
     redlife = 
     	if (urecord.rlost = true && game.redmercinv = false) 
     	  then game.redlife - 1 
     	else game.redlife;
     bluelife = 
     	if (urecord.blost = true && game.bluemercinv = false) 
     	  then game.bluelife - 1 
     	else game.bluelife;
     
     redscore = newscore urecord.blost game.bluemercinv urecord.rgraze_pts 
       urecord.rpower_pts game.redscore;
     bluescore = newscore urecord.rlost game.redmercinv urecord.bgraze_pts
       urecord.bpower_pts game.bluescore;
     
     redinvinc = newinvinc urecord.rlost game.redmercinv game.redinvinc;
     blueinvinc = newinvinc urecord.blost game.bluemercinv game.blueinvinc;
     
     redmercinv = 
     	if (urecord.rlost = true && game.redmercinv = false) then true else false;
     bluemercinv = 
     	if (urecord.blost = true && game.bluemercinv = false) then true else false;
     
     redbombs = 
     	if (urecord.rlost = true && game.redmercinv = false) 
     	then cINITIAL_BOMBS else game.redbombs;
     bluebombs = 
     	if (urecord.blost = true && game.bluemercinv = false) 
     	then cINITIAL_BOMBS else game.bluebombs;

     redbombinv = 
     	newbombinv game.redbombinv urecord.rlost game.redmercinv game.redinvinc;
     bluebombinv = 
     	newbombinv game.bluebombinv urecord.blost game.bluemercinv game.blueinvinc;
     
     bullets = if (urecord.rlost = true && game.redmercinv = false) || 
     	(urecord.blost = true && game.bluemercinv = false) 
     	then [] else urecord.bullet_lst;
     	
     timer = game.timer -. cUPDATE_TIME;
     
     (*Version 4*)
     ufospawntimer = if game.ufospawntimer = 0 then cUFO_SPAWN_INTERVAL
     else game.ufospawntimer - 1;
     ufomovetimer = if game.ufomovetimer = 0 then cUFO_MOVE_INTERVAL 
     else game.ufomovetimer - 1;
     
     ufos = 
     (let lst = if (game.ufomovetimer = 0) then (update_ufo urecord.ulst) 
     else urecord.ulst in 
     let newufos = if (game.ufospawntimer = 0) 
     then (create_ufo()::lst) else lst in newufos);
     
     powers = urecord.powerlst
   } in
   delete_all (if (urecord.rlost = true && game.redmercinv = false) || 
     	(urecord.blost = true && game.bluemercinv = false) 
  then urecord.bullet_lst else []);
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
  add_update (SetBombs (Red, updated.redbombs));
  add_update (SetBombs (Blue, updated.bluebombs));
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
	   bluebombinv = true;
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
	   redbombinv = true;
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
