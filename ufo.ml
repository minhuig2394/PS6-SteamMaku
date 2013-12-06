open Definitions
open Constants
open Util 
open Netgraphics 

(*generates an initial velocity*)
let init_vel (speed:int) (target:position) (pos:position):velocity= 
  let sp = float_of_int speed in 
  let x = ((fst target) -. (fst pos)) in
  let y = ((snd target) -. (snd pos)) in 
  let cosine,sine = unit_v (x,y) in 
  (sp*.cosine,sp*.sine)

(*determines if two circles intersect*)
let collide (center1:position) (r1:int) (center2:position) (r2:int):bool = 
  let radius = float_of_int (r1 + r2) in 
  let diff = subt_v center1 center2 in 
  ((fst diff)*.(fst diff) +. (snd diff)*.(snd diff)) < radius*.radius 
(*determines if a ufo is hit by a bullet*)
let hit_ufo (b:bullet) (u:ufo) :bool= 
  collide b.b_pos b.b_radius u.u_pos u.u_radius
(*sets new position for a ufo,returns the ufo with edited postion*)
let set_pos_ufo (b:ufo) :ufo= 
  let new_pos = add_v b.u_pos b.u_vel in 
    (add_update (MoveUFO (b.u_id,new_pos)));
    {b with 
    u_pos = new_pos}
(*generates a random position to spawn a ufo*)
let random_position x=
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
(*gives a random position to create a new power up within
*a certain radius of the ufo*)
let random_pradius (radius:int) (center:position):position =
  let randr = Random.float (float radius) in 
  let randa = Random.int 360 in 
  let randx,randy = rotate_deg (0.,randr) (float randa) in 
  add_v center (randx,randy)
(*generates a random velocity to assign a new ufo*)
let random_vel (upos:position):velocity= 
  let r = random_position 1 in 
  init_vel cUFO_SPEED r upos
(*create_ufo x creates a new ufo.  The parameter is 
*arbitrary.  It just guarantees a new ufo every time you 
*call the function*)
let create_ufo = fun x ->
  let rdir = random_position 0 in 
  let u = 
  {u_id = next_available_id ();
    u_pos = rdir;
    u_vel = random_vel rdir;
    u_radius = cUFO_RADIUS;
    u_red_hits = 0;
   u_blue_hits = 0
  } in add_update (AddUFO (u.u_id, u.u_pos));u
(*create_power creates cUFO_POWER_NUM power ups within a 
*certain radius of the ufo, returns a list of those power ups*)
let create_power  (u:ufo) (rpos:position) (bpos:position): power list= 
  let h = (u.u_red_hits + u.u_blue_hits) in 
  let reds = (float u.u_red_hits/. float h)*.(float cUFO_POWER_NUM) in 
	let rec create n r plst= 
	if n > 0 then 
    let pos = random_pradius cUFO_SCATTER_RADIUS u.u_pos in 
		let p = 
		  {b_type = Power;
		  b_id = next_available_id ();
  		b_pos = pos;
  		b_vel = if r < reds then (init_vel cPOWER_SPEED rpos pos)
        else (init_vel cPOWER_SPEED bpos pos);
  		b_accel = (0.,0.);
  		b_radius = cPOWER_RADIUS;
  		b_color = if r < reds then Red else Blue;
		} in add_update (AddBullet (p.b_id, p.b_color,p.b_type,p.b_pos));
		create (n - 1) (r +. 1.) (p::plst)
  else plst
  in create cUFO_POWER_NUM 0.0 []
(*update_ufo assigns a new velocity for each member of a ufo list.*)
let update_ufo (ufolst:ufo list): ufo list= 
	let rec update ulst rlst = 
	match ulst with 
	|h::t ->
			let u = {h with 
        u_vel = init_vel cUFO_SPEED (random_position 32) (random_position 2)}
			in update t (u::rlst)
	|[] -> rlst
in update ufolst []

