open Definitions
open Constants
open Game
open Util 
open Player 

(*determines if two circles intersect*)
let collide (point:position) (r1:int) (center:position) (r2:int) = 
  let radius = float_of_int (r1 + r2) in 
  let diff = subt_v point center in 
  ((fst diff)*.(fst diff) +. (snd diff)*.(snd diff)) < radius*.radius 

let set_pos (b:bullet) = 
	{b with 
		b_pos = add_v b.b_pos b.b_vel;
		b_vel = add_v b.b_vel b.b_accel}

(*determines if a bullet is out of bounds*)
let out (b:bullet) = 
  not (in_bounds b.b_pos)
(*determines if a bullet's radius and a player's radius intersect*)
let hit (b:bullet) (p:player_char) = 
  collide b.b_pos b.b_radius p.p_pos p.p_radius
(*if a player is not hit but is within graze radius of a bullet returns true*)
let grazed (b:bullet) (p:player_char)= 
  (not (hit b p)) && (collide b.b_pos cGRAZE_RADIUS p.p_pos p.p_radius) 

let update_blives (blst: bullet list) (p:player_char) =
  let rec update ulst lost graze rlst= 
  match ulst with 
  |h::t -> 
    if out h then 
      if hit h p then 
        update t true graze rlst 
      else 
        if grazed h p then 
          update t lost (graze + 1) rlst
        else update t lost graze rlst
    else 
      if hit h p then 
        update t true graze ((set_pos h)::rlst)
      else 
        if grazed h p then 
          update t lost (graze + 1) ((set_pos h)::rlst)
        else  update t lost graze ((set_pos h)::rlst)
  |[] -> lost,graze,rlst
in update blst false 0 []

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
          b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
          b_radius = cTRAIL_RADIUS;
          b_color = pcolor} in 
          trail_helper (n-1) (angle-.ang) (t::lst)
      else lst 
      in let newlst = trail_helper 3 ang lst 
      in trail (n - 1) newlst
    else lst 
  in trail cTRAIL_NUM []

let create_bullet b target plpos pcolor acc= 
	match b with 
	|Spread -> create_spread  target plpos pcolor acc 
	|Bubble -> create_spread  target plpos pcolor acc  
	|Trail ->  create_trail  plpos pcolor target acc
  |Power -> failwith "Error: Power not a bullet"

