type ufo = {
  u_id : int;
  u_pos : position;
  u_vel : velocity;
  u_radius : int;
  u_red_hits : int;
  u_blue_hits : int
}


let init_vel speed target pos= 
  let sp = float_of_int speed in 
  let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let hyp = sqrt (x*.x +. y*.y) in 
  let sine = y/.hyp in 
  let cosine = x/.hyp in 
  (sp*.cosine,sp*.sine)

let random_position = 
  let onef = 1./.4. *. (float_of_int cBOARD_WIDTH) in  
  let threef = 3./.4. *. (float_of_int cBOARD_WIDTH) in 
  let bound = threef -. onef in 
  let randx = Random.float bound +. onef in 
  let halfy= (float_of_int (cBOARD_HEIGHT/2)) in 
  let flip = Random.int 2 in 
    let randy = Random.float halfy in 
    if flip = 1 then 
      (randx,randy+.halfy)
    else 
      (randx, randy)

let roll x= let r = Random.int 2 in 
  if r = 0 then x
  else ~-.x

let random_pradius radius center =
  let randx = Random.float (float_of_int radius) in 
  let randy = Random.float (float_of_int radius) in 
    ((roll randx)+.(fst center)), ((roll randy)+.(snd center))

let create_power (id:int) (u:ufo) (rpos:position) (bpos:position)= 
  let reds = h/u.u_red_hits in 
	let rec create n r id plst= 
	if n > 0 then 
    let pos = random_pradius radius u.u_pos in 
		let p = 
		{b_type = Power 
		b_id = id;
  		b_pos = pos;
  		b_vel = if r < reds then (init_vel cPOWER_SPEED rpos pos)
              else (init_vel cPOWER_SPEED bpos pos)
  		b_accel = (0.,0.);
  		b_radius cPOWER_RADIUS;
  		b_color = pcolor
		} in 
		create (n - 1) (r + 1) (id + 1) (p::plst)
  else plst
  in create cUFO_POWER_NUM 0 id []

let random_vel upos= 
  let r = random_position in 
  init_vel cUFO_SPEED r upos

let create_ufo id= 
let rdir = random_direction
	{u_id = id;
  	u_pos = rdir;
  	u_vel = random_vel rdir
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

