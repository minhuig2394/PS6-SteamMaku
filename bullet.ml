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
let collide (center1:position) (r1:int) (center2:position) (r2:int) = 
  let radius = float_of_int (r1 + r2) in 
  let diff = subt_v center1 center2 in 
  ((fst diff)*.(fst diff) +. (snd diff)*.(snd diff)) < radius*.radius 

let set_pos (b:bullet) = 
  let new_pos = add_v b.b_pos b.b_vel in 
    (add_update (MoveBullet (b.b_id,new_pos)));
	{b with 
		b_pos = new_pos;
		b_vel = add_v b.b_vel b.b_accel}

(*determines if a bullet is out of bounds*)
let out (b:bullet) = not(
  (((fst b.b_pos)+. (float b.b_radius)) >= 0. && 
  ((snd b.b_pos)+. (float b.b_radius)) >= 0. && 
  ((fst b.b_pos)+. (float b.b_radius)) <= float cBOARD_WIDTH && 
  ((snd b.b_pos)+. (float b.b_radius)) <= float cBOARD_HEIGHT))
(*determines if a bullet's radius and a player's radius intersect*)
let hit (b:bullet) (p:player_char) = 
  collide b.b_pos b.b_radius p.p_pos p.p_radius
(*if a player is not hit but is within graze radius of a bullet returns true*)
let grazed (b:bullet) (p:player_char)= 
  (not (hit b p)) && (collide b.b_pos cGRAZE_RADIUS p.p_pos p.p_radius) 

let init_vel speed target pos= 
	let sp = float_of_int speed in 
	let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let cosine,sine = unit_v (x,y) in 
  (sp*.cosine,sp*.sine)

let create_spread target plpos pcolor acc = 
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
        add_update (AddBullet (s.b_id, s.b_color,s.b_type,s.b_pos));
				let new_bullst = s::lst
			in screate (n - 1) new_ang new_bullst 
		else lst 
	in screate cSPREAD_NUM 0.0 [] 

let create_bubble  target plpos pcolor acc= 
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

let create_trail plpos pcolor target acc= 
  let ang = deg_to_rad (float_of_int cTRAIL_ANGLE) in 
  let rec trail n lst= 
    if n > 0 then 
    let rec trail_helper n angle lst = 
      if n > 0 then 
        let t = 
          {b_type = Trail;
          b_id = next_available_id ();
          b_pos = plpos;
          b_vel = 
            (let next_vel = (init_vel (cTRAIL_SPEED_STEP*n) target plpos) in 
            rotate next_vel angle); 
          b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc)<=cACCEL_LIMIT then 
          acc else (0.,0.));
          b_radius = cTRAIL_RADIUS;
          b_color = pcolor} in 
          add_update (AddBullet (t.b_id, t.b_color,t.b_type,t.b_pos));
          trail_helper (n-1) (angle-.ang) (t::lst)
      else lst 
      in let newlst = trail_helper 3 ang lst 
      in trail (n - 1) newlst
    else lst 
  in trail cTRAIL_NUM []

let create_bullet (b:bullet_type) (target:position) 
    (plpos:position) (pcolor:color) (acc:acceleration)= 
	match b with 
	|Spread -> create_spread  target plpos pcolor acc 
	|Bubble -> create_spread  target plpos pcolor acc  
	|Trail ->  create_trail  plpos pcolor target acc
  |Power -> failwith "Error: Power not a bullet"

let update_ufos bul updates= 
  let remove_ufo updated_ufo acc= 
    if (updated_ufo.u_red_hits + updated_ufo.u_blue_hits) >= cUFO_HITS then
    ((add_update (DeleteUFO updated_ufo.u_id));
      (updated_ufo::(fst acc)),(snd acc))
    else (fst acc),(updated_ufo::(snd acc)) in
    let p,u = List.fold_left (fun acc elem -> 
      if (hit_ufo bul elem) then 
      let updated_ufo = 
        (if bul.b_color = Red then 
        {elem with u_red_hits = (elem.u_red_hits + 1)} 
        else {elem with u_blue_hits = (elem.u_blue_hits + 1)})
      in 
        (remove_ufo updated_ufo acc)
      else (fst acc),elem::(snd acc)) ([],[]) updates.ulst in 
        if List.length p > 0 then true,p,u else false,p,u 

let determine (hitufo: bool)(h:bullet) (player:player_char) 
                (invincible:bool) (update:'a) (updates:total_update) 
                (t:bullet list) (pwr:power list)= 
  if (out h) then 
    (add_update (DeleteBullet h.b_id);update t pwr)
  else 
    if invincible = true then 
      if (hit h player) then 
        (add_update (DeleteBullet h.b_id);update t pwr)
       else 
        if (grazed h player) then (add_update (Graze);
          if player.p_color = Blue then 
          (updates.bgraze_pts <- updates.bgraze_pts + 1;update t pwr)
          else (updates.rgraze_pts <- updates.rgraze_pts + 1;update t pwr))
        else 
          if hitufo = true then (add_update (DeleteBullet h.b_id);update t pwr)
          else 
            (updates.bullet_lst <- (set_pos h)::updates.bullet_lst; update t pwr)
    else 
      if hit h player then 
        if player.p_color = Blue then (updates.blost <- true;update t pwr)
        else (updates.rlost <- true; update t pwr)
      else 
        if grazed h player then 
          (add_update (Graze);
          if player.p_color = Blue then 
            (updates.bgraze_pts <- updates.bgraze_pts + 1;
          updates.bullet_lst <- (set_pos h)::updates.bullet_lst; update t pwr)
           else (updates.rgraze_pts <- updates.rgraze_pts + 1;
           updates.bullet_lst <- (set_pos h)::updates.bullet_lst; update t pwr))
         else
          if hitufo = true then (add_update (DeleteBullet h.b_id);update t pwr) 
          else
            (updates.bullet_lst <- (set_pos h)::updates.bullet_lst; update t pwr)

let update_bullets (rp:player_char) (bp:player_char) (rinvincible :bool) 
  (binvincible:bool) (blst: bullet list) (ufolst: ufo list) (pwr: power list)=
  let updates = 
  {rlost =false; blost =false; rgraze_pts = 0; bgraze_pts= 0; 
    bullet_lst = []; ulst = ufolst; powerlst= [];rpower_pts= 0;
    bpower_pts= 0;} in 
  let rec update bulletlst pwr= 
    match bulletlst with 
    |h::t -> 
      let hitufo,pow,u = (update_ufos h updates) in
      let pwr =       
      (List.fold_left 
        (fun acc elem -> (create_power elem rp.p_pos bp.p_pos)@acc) pwr pow) 
    in updates.ulst <- u; begin 
      match h.b_color with
      |Red -> determine hitufo h bp binvincible update updates t pwr
      |Blue -> determine hitufo h rp binvincible update updates t pwr
      end
    |[] -> (updates,pwr)
  in update blst pwr

let update_all (updlst: update_info)= 
  let updates,pwers = 
    (update_bullets updlst.red updlst.blue updlst.rinvincible 
      updlst.binvincible updlst.blst updlst.ufolst updlst.pwrlst) in 
  let rec update_powers pwrs plst= 
    match pwrs with 
    |h::t -> 
      if hit h updlst.red then 
        (add_update (DeleteBullet h.b_id);
        updates.rpower_pts <- updates.rpower_pts + 1;
        update_powers t plst)
      else
        if hit h updlst.blue then 
          (add_update (DeleteBullet h.b_id);
          updates.bpower_pts <- updates.bpower_pts + 1;
        update_powers t plst)
        else update_powers t (h::plst)
    |[] -> updates.powerlst <- plst;updates
  in update_powers ((updlst.pwrlst)@pwers) []

let delete_all blst = 
  List.iter (fun elem -> add_update (DeleteBullet elem.b_id)) blst
