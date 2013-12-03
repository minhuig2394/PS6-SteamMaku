open Definitions
open Constants
open Game
open Util 
open Player 

let add_vector a b = 
	((fst a) +. (fst b)),((snd a) +. (snd b))
let sub_vector a b = 
	((fst a) -. (fst b)),((snd a) -. (snd b))

(*determines if two circles intersect*)
let collide (point:position) (r1:int) (center:position) (r2:int) = 
  let radius = float_of_int (r1 + r2) in 
  let diff = sub_vector point center in 
  ((fst diff)*.(fst diff) +. (snd diff)*.(snd diff)) < radius*.radius 

let set_pos (b:bullet) = 
	{b with 
		b_pos = add_vector b.b_pos b.b_vel;
		b_vel = add_vector b.b_vel b.b_accel}

(*determines if a bullet is out of bounds*)
let out (b:bullet) = 
  b.b_pos <= (float_of_int cBOARD_WIDTH,float_of_int cBOARD_HEIGHT) 

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
  let hyp = sqrt (x*.x +. y*.y) in 
  let sine = y/.hyp in 
  let cosine = x/.hyp in 
  (sp*.cosine,sp*.sine)

let create_spread id target plpos pcolor acc = 
  let x = ((fst target) -. (fst plpos)) in
  let y = ((snd target) -. (snd plpos)) in 
  let or_angle = atan2 y x in
  let sp = float_of_int cSPREAD_SPEED in 
  let pi = 4.0 *. atan 1.0 in 
  let ang = (float_of_int (360/cSPREAD_NUM))*.(pi /. 180.) in 
	let rec screate n angle id lst = 
    let new_ang = angle+.ang in 
		if not (n = 0) then 
			let s = {b_type = Spread;
				b_id = id;
				b_pos = plpos;
				b_vel = sp*.(cos new_ang),sp*.(sin new_ang);
				b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
				b_radius = cSPREAD_RADIUS;
				b_color = pcolor} in 
				let new_bullst = s::lst
			in screate (n - 1) new_ang (id + 1) new_bullst 
		else lst 
	in screate cSPREAD_NUM or_angle id [] 

let create_bubble id target plpos pcolor acc= 
	let b = {b_type = Bubble;
				b_id = id;
				b_pos = plpos;
				b_vel = init_vel cBUBBLE_SPEED target plpos;
				b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
				b_radius = cBUBBLE_RADIUS;
				b_color = pcolor} in 
				[b]

let create_trail id plpos pcolor target acc= 
  let ang = (float_of_int cTRAIL_ANGLE)*.(pi /. 180.) in 
  let x = ((fst target) -. (fst plpos)) in
  let y = ((snd target) -. (snd plpos)) in 
  let sp = float_of_int cTRAIL_SPEED_STEP in 
  let angle = atan2 y x in
  let rec trail n lst= 
  if n > 0 then 
		let t = {b_type = Trail;
			b_id = id;
			b_pos = plpos;
			b_vel = (sp*.(cos angle),sp*.(sin angle));
			b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
			b_radius = cTRAIL_RADIUS;
			b_color = pcolor} in 
		  let nlst = t::lst
    in let t2 = 
      {b_type = Trail;
        b_id = id +1;
        b_pos = plpos;
        b_vel =  ((sp+.sp)*.(cos (angle+.ang)),(sp+.sp)*.(sin (angle+.ang)));
        b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
        b_radius = cTRAIL_RADIUS;
        b_color = pcolor} in 
       let mlst = t2::nlst
    in let t3 = 
      {b_type = Trail;
        b_id = id +2;
        b_pos = plpos;
        b_vel =  ((sp+.sp+.sp)*.(cos (angle-.ang)),(sp+.sp+.sp)*.(sin (angle-.ang)));
        b_accel = (if fst(acc) <= cACCEL_LIMIT && fst(acc) <= cACCEL_LIMIT then 
          acc else (0.,0.));
        b_radius = cTRAIL_RADIUS;
        b_color = pcolor} in 
      let rlst = t3::mlst in 
      trail (n - 1) rlst
			else lst 
    in trail cTRAIL_NUM  []

let create_bullet b target id plpos pcolor acc= 
	match b with 
	|Spread -> create_spread id target plpos pcolor acc 
	|Bubble -> create_spread id target plpos pcolor acc  
	|Trail ->  create_trail  id plpos pcolor target acc

