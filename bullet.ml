open Definitions
open Constants
open Game
open Util 
open Player 


let add_vector a b = 
	((fst a) +. (fst b)),((snd a) +. (snd b))
let sub_vector a b = 
	((fst a) -. (fst b)),((snd a) -. (snd b))

let set_pos (b:bullet) = 
	{b with 
		b_pos = add_vector b.b_pos b.b_vel;
		b_vel = add_vector b.b_vel b.b_accel}

let bullet_vel a = 
	match a.b_type with 
	|Spread -> cSPREAD_SPEED 
	|Bubble -> cBUBBLE_SPEED 
	|Trail -> cTRAIL_SPEED_STEP 

let init_vel speed target pos= 
	let sp = float_of_int speed in 
	let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let hyp = sqrt (x*.x +. y*.y) in 
  let sin = y/.hyp in 
  let cos = x/.hyp in 
  (sp*.cos,sp*.sin)

let angled_pos ppos angle = 

let create_spread id plpos pcolor acc = 
	let rec screate n id lst = 
		if not (n = 0) then 
			let s = {b_type = Spread;
				b_id = id;
				b_pos = plpos;
				b_vel = init_vel cSPREAD_SPEED target plpos;
				b_accel = (if acc < cACCEL_LIMIT then acc else cACCEL_LIMIT);
				b_radius = cSPREAD_RADIUS;
				b_color = pcolor} in 
				let new_bullst = s::lst
			in screate (n - 1) (id + 1) new_bullst plpos pcolor acc
		else lst 
	in screate cSPREAD_NUM id [] 

let create_bubble id target plpos pcolor acc= 
	let b = {b_type = Bubble;
				b_id = id;
				b_pos = plpos;
				b_vel = init_vel cBUBBLE_SPEED target plpos;
				b_accel = (if fst(acc) < cACCEL_LIMIT && fst(acc) < cACCEL_LIMIT then 
          acc else (0.,0.));
				b_radius = cBUBBLE_RADIUS;
				b_color = pcolor} in 
				[b]
(*deal with angle shit*)
let create_trail id plpos pcolor target acc= 
	let rec tcreate n id speed tpos lst = 
		if not (n = 0) then 
			let t = {b_type = Trail;
				b_id = id;
				b_pos = plpos;
				b_vel = init_vel (speed*n) target plpos;
				b_accel = (if acc < cACCEL_LIMIT then acc else cACCEL_LIMIT);
				b_radius = cSPREAD_RADIUS;
				b_color = pcolor} in 
				let new_bullst = s::lst
			in tcreate (n - 1) (id + 1) new_bullst plpos pcolor acc
		else lst 
	in tcreate 3*cTRAIL_NUM id cTRAIL_SPEED_STEP [] 

let create_bullet b id plpos pcolor acc= 
	match b with 
	|Spread -> create_spread id plpos pcolor acc
	|Bubble -> create_bubble id plpos pcolor acc
	|Trail ->  create_bullet id plpos pcolor acc
end 