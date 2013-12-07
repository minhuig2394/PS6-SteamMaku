open Definitions
open Constants
open Util 
open Ufo
open Netgraphics

type total_update = 
  {mutable rlost:bool; 
   mutable blost:bool; 
    mutable rgraze_pts: int; 
    mutable bgraze_pts: int; 
    mutable bullet_lst: bullet list;
    mutable delete_bullets: bullet list; 
    mutable ulst: ufo list; 
    mutable powerlst: power list;
    mutable rpower_pts: int;
    mutable bpower_pts: int;
    }

  type update_info = 
    {red: player_char; 
    blue: player_char;
    rinvincible :bool; 
    binvincible:bool; 
    blst: bullet list; 
    ufolst: ufo list; 
    pwrlst: power list}

(*determines if two circles intersect*)
let collide (center1:position) (r1:int) (center2:position) (r2:int):bool = 
  let radius = float_of_int (r1 + r2) in 
  let diff = subt_v center1 center2 in 
  ((fst diff)*.(fst diff) +. (snd diff)*.(snd diff)) < radius*.radius 
(*sets the postion of a bullet*)
let set_pos (b:bullet):bullet = 
  let new_pos = add_v b.b_pos b.b_vel in 
    (add_update (MoveBullet (b.b_id,new_pos)));
  {b with 
    b_pos = new_pos;
    b_vel = add_v b.b_vel b.b_accel}

(*determines if a bullet is out of bounds*)
let out (b:bullet) :bool= not(
  (((fst b.b_pos)+. (float b.b_radius)) >= 0. && 
  ((snd b.b_pos)+. (float b.b_radius)) >= 0. && 
  ((fst b.b_pos)-. (float b.b_radius)) <= float cBOARD_WIDTH && 
  ((snd b.b_pos)-. (float b.b_radius)) <= float cBOARD_HEIGHT))
(*determines if a bullet's radius and a player's radius intersect*)
let hit (b:bullet) (p:player_char) :bool= 
  collide b.b_pos b.b_radius p.p_pos p.p_radius
(*if a player is not hit but is within graze radius of a bullet returns true*)
let grazed (b:bullet) (p:player_char):bool= 
  (not (hit b p)) && (collide b.b_pos cGRAZE_RADIUS p.p_pos p.p_radius) 
(*returns the initial velocity given a speed (scalar),an enemy's position
* and the player's position*)
let init_vel (speed:int) (target:position) (pos:position):velocity= 
  let sp = float_of_int speed in 
  let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let cosine,sine = unit_v (x,y) in 
  (sp*.cosine,sp*.sine)
(*helper function that creates cSPREAD_NUM spreads and returns as a list
* of spread bullets*)
let create_spread (target:position) (plpos:position) 
  (pcolor:color) (acc:acceleration): bullet list = 
  let ang = deg_to_rad (float_of_int (360/cSPREAD_NUM)) in 
  let initial_velocity = init_vel cSPREAD_SPEED target plpos in
  let rec screate n angle lst = 
    let new_ang = angle+.ang in 
    if n > 0 then 
      let s = {b_type = Spread;
        b_id = next_available_id ();
        b_pos = plpos;
        b_vel = rotate initial_velocity new_ang;
        b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
        b_radius = cSPREAD_RADIUS;
        b_color = pcolor} in 
        let new_bullst = s::lst
      in add_update (AddBullet (s.b_id, s.b_color,s.b_type,s.b_pos));
        screate (n - 1) new_ang new_bullst 
    else lst 
  in screate cSPREAD_NUM 0.0 [] 

(*helper function creates 1 bubble and returns as a list with a single bubble*)
let create_bubble  (target:position) (plpos:position) 
  (pcolor:color) (acc:acceleration): bullet list= 
  let b = {b_type = Bubble;
        b_id = next_available_id ();
        b_pos = plpos;
        b_vel = init_vel cBUBBLE_SPEED target plpos;
        b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
        b_radius = cBUBBLE_RADIUS;
        b_color = pcolor} in 
        add_update (AddBullet (b.b_id, b.b_color,b.b_type,b.b_pos));
        [b]
(*helper function creates cTRAIL_NUM*3 trail bullets. 
* Creates cTRAIL_NUM series of trails, such that the speeds of 
* the trails are progressively increasing*)
let create_trail (plpos:position) (pcolor:color) 
  (target:position) (acc:acceleration): bullet list= 
  let ang = deg_to_rad (float_of_int cTRAIL_ANGLE) in 
  let rec trail nth angle lst= 
    if nth > 0 then 
      let t = 
          {b_type = Trail;
          b_id = next_available_id ();
          b_pos = plpos;
          b_vel = 
            (let next_vel = (init_vel (cTRAIL_SPEED_STEP*nth) target plpos) in 
            rotate next_vel angle); 
          b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc)<=cACCEL_LIMIT then 
          acc else (0.,0.));
          b_radius = cTRAIL_RADIUS;
          b_color = pcolor} in 
          add_update (AddBullet (t.b_id, t.b_color,t.b_type,t.b_pos));
          if nth mod 3 = 0 then trail (nth-1) (0.) (t::lst)
          else 
            if nth mod 3 = 1 then trail (nth-1) (ang) (t::lst)
            else trail (nth-1) (~-.ang) (t::lst)
     else lst 
       in trail (cTRAIL_NUM*3) ang []

(*create_bullet b target plpos pcolor acc returns a list of bullet(s)
*of the given bullet type, some or all of which aim toward the target position
*Bullets have the given acceleration unless it is too large.*)
let create_bullet (b:bullet_type) (target:position) 
    (plpos:position) (pcolor:color) (acc:acceleration)= 
  match b with 
  |Spread -> create_spread  target plpos pcolor acc 
  |Bubble -> create_bubble  target plpos pcolor acc  
  |Trail ->  create_trail  plpos pcolor target acc
  |Power -> failwith "Error: Power not a bullet"

(*updates ufos, given a bullet and parameters in the form of an total_update
*type *)
let update_ufos (bul:bullet) (updates:total_update)= 
    let p,u = List.fold_left (fun acc elem -> 
      if (hit_ufo bul elem) then 
      let updated_ufo = 
        (print_endline "ufo hit!";if bul.b_color = Red then 
        {elem with u_red_hits = (elem.u_red_hits + 1)} 
        else {elem with u_blue_hits = (elem.u_blue_hits + 1)})
      in 
        if (updated_ufo.u_red_hits + updated_ufo.u_blue_hits) >= cUFO_HITS then
        ((add_update (DeleteUFO updated_ufo.u_id));print_endline "ufo took ";
            print_int (elem.u_red_hits + elem.u_blue_hits);
        (updated_ufo::(fst acc)),(snd acc))
        else (fst acc),(updated_ufo)::(snd acc) 
      else 
        (fst acc, elem::snd acc)) ([],[]) updates.ulst in 
        if List.length p > 0 then true,p,u else false,p,u 

let delete (bul:bullet) = add_update (DeleteBullet bul.b_id)
(*delete_all clears the gui of bullets*)
let delete_all (blst:bullet list): unit = 
  List.iter (fun elem -> delete elem) blst

(*helper functions determines which bullets are kept, what points to add*)
let determine (hitufo: bool)(h:bullet) (player:player_char) 
                (invincible:bool) (update:'a) (updates:total_update) 
                (t:bullet list) (pwr:power list) (bullet_lst:bullet list) 
                (delete_bullets:bullet list)= 
  if (out h) then 
    (update t pwr bullet_lst (h::delete_bullets))
  else 
    if hit h player then 
      if invincible = true then (
      update t pwr bullet_lst (h::delete_bullets))
      else 
        if player.p_color = Blue then 
          (updates.blost <- true;update t pwr bullet_lst (h::delete_bullets))
        else (
        updates.rlost <- true;update t pwr bullet_lst (h::delete_bullets))
    else 
        if grazed h player then 
          (add_update (Graze);
          if invincible = true || hitufo = true then 
            if player.p_color = Blue then 
            (updates.bgraze_pts <- updates.bgraze_pts + 1;
            update t pwr bullet_lst (h::delete_bullets))
            else (updates.rgraze_pts <- updates.rgraze_pts + 1;
            update t pwr bullet_lst (h::delete_bullets))
          else 
            (
            if player.p_color = Blue then 
              (updates.bgraze_pts <- updates.bgraze_pts + 1;
              update t pwr ((set_pos h)::bullet_lst) delete_bullets)
            else (updates.rgraze_pts <- updates.rgraze_pts + 1;
            update t pwr ((set_pos h)::bullet_lst) delete_bullets)))
        else
          if hitufo = true then 
            (update t pwr bullet_lst (h::delete_bullets)) 
          else 
            (update t pwr ((set_pos h)::bullet_lst) delete_bullets)
            
let update_all_ufos (ulst:ufo list) = 
    List.fold_left (fun acc elem -> (set_pos_ufo elem)::acc) [] ulst

(*helper function handles all bullet collisions*)
let update_bullets (rp:player_char) (bp:player_char) (rinvincible :bool) 
  (binvincible:bool) (blst: bullet list) (ufolst: ufo list) (pwr: power list) =
  let updates = 
  {rlost =false; blost =false; rgraze_pts = 0; bgraze_pts= 0; 
    bullet_lst = []; delete_bullets = []; ulst = ufolst; powerlst= [];rpower_pts= 0;
    bpower_pts= 0;} in 
  let rec update bulletlst pwr bullst dlst= 
    match bulletlst with 
    |h::t -> 
      let hitufo,pow,u = (update_ufos h updates) in
      let pwr =       
      (List.fold_left 
        (fun acc elem -> (create_power elem rp.p_pos bp.p_pos)@acc) pwr pow) 
    in updates.ulst <- u;
    begin 
      match h.b_color with
      |Red -> determine hitufo h bp binvincible update updates t pwr bullst dlst
      |Blue -> determine hitufo h rp rinvincible update updates t pwr bullst dlst
      end
    |[] -> (updates.ulst <- (update_all_ufos updates.ulst);updates.delete_bullets <- dlst);
      (updates.bullet_lst <- bullst); (updates,pwr)
  in update blst pwr [] []

(*update_all updlst takes in an update_info list and returns a total_update record
*of all updated information*)
let update_all (updlst: update_info): total_update= 
print_endline "updating powers";
  let updates,pwers = 
    (update_bullets updlst.red updlst.blue updlst.rinvincible 
      updlst.binvincible updlst.blst updlst.ufolst updlst.pwrlst) in 
  let rec update_powers pwrs plst= 
    match pwrs with 
    |h::t -> 
    if out h then (delete h;update_powers t plst)
    else 
      if hit h updlst.red && not (hit h updlst.blue) then 
        (delete h;
        updates.rpower_pts <- updates.rpower_pts + 1;
        update_powers t plst)
      else
        if hit h updlst.blue && not (hit h updlst.red) then 
          (delete h;
          updates.bpower_pts <- updates.bpower_pts + 1;
        update_powers t plst)
        else 
        if (hit h updlst.blue) && (hit h updlst.red) then 
          (delete h;
        updates.rpower_pts <- updates.rpower_pts + 1;
        updates.bpower_pts <- updates.bpower_pts + 1;
        update_powers t plst)
        else
        update_powers t ((set_pos h)::plst)
    |[] -> updates.powerlst <- plst;updates
  in update_powers (pwers) []

