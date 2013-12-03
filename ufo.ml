type ufo = {
  u_id : int;
  u_pos : position;
  u_vel : velocity;
  u_radius : int;
  u_red_hits : int;
  u_blue_hits : int
}


let random_pradius radius =

let create_power id n radius pcolor = 
	let rec create n id plst= 
	if n > 0 then 
		let p = 
		{b_type = Power 
		b_id = id;
  		b_pos = random_pradius;
  		b_vel = velocity;
  		b_accel = (0.,0.);
  		b_radius cPOWER_RADIUS;
  		b_color = pcolor
		} in 
		create (n - 1) (id + 1) (p::plst)
  else plst
  in create n id []

let init_vel speed target pos= 
	let sp = float_of_int speed in 
	let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let hyp = sqrt (x*.x +. y*.y) in 
  let sine = y/.hyp in 
  let cosine = x/.hyp in 
  (sp*.cosine,sp*.sine)


let random_position  = 

let random_direction = 

let create_ufo id= 
let rdir = random_direction
	{u_id = id;
  	u_pos = rdir;
  	u_vel = init_vel cUFO_SPEED rdir random_position 
  	u_radius = cUFO_RADIUS;
  	u_red_hits = 0;
 	 u_blue_hits = 0
	}

let update_ufo ufolst= 
	let rec update ulst rlst = 
	match ulst with 
	|h::t ->
		let rdir = random_direction in 
		if u.u_red_hits + u.u_blue_hits < cUFO_HITS then 
			let u = {h with u_vel = init_vel cUFO_SPEED random_direction random_position}
			in update t (u::rlst) 
		else 
			create_power cUFO_POWER_NUM cUFO_SCATTER_RADIUS
	|[] -> rlst

