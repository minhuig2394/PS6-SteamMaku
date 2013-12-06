(*unit_test ufo.ml*)
let rplayer = {p_id = 100;
				p_pos = (0.,0.);
				p_focused = true;
				p_radius = cHITBOX_RADIUS;
				p_color = Red}

let bplayer = {p_id = 101;
				p_pos = (float cBOARD_WIDTH -. 1.,float cBOARD_HEIGHT -. 1.);
				p_focused = true;
				p_radius = cHITBOX_RADIUS;
				p_color = Blue}

let ufo1 = {u_id = 50;
			u_pos = (float cBOARD_WIDTH +. 1., float cBOARD_HEIGHT);
			u_vel = (1.,1.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 0;
			u_blue_hits = 0;
			 }

let ufo2 = {u_id = 51;
			u_pos = (0.,0.);
			u_vel = (1.,1.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 7;
			u_blue_hits = 3;
			 }
let ufo3 = {u_id = 52;
			u_pos = (float cBOARD_WIDTH, float cBOARD_HEIGHT);
			u_vel = (25.,25.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 9;
			u_blue_hits = 9;
			 }
let ufo4 = {u_id = 53;
			u_pos = (float cBOARD_WIDTH, float cBOARD_HEIGHT);
			u_vel = (100.,100.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 10;
			u_blue_hits = 0;
			 }
let ulist = []
let ulist1 = [ufo1]
let ulist2 = [ufo1;ufo2;ufo3;ufo4]
let random_position_test = 
	let rp = (random_position 4) in 
		fst rp >= (1./.4. *. (float_of_int cBOARD_WIDTH)) && 
 		fst rp <= (3./.4. *. (float_of_int cBOARD_WIDTH)) 

let random_pradius_test = 
	let radius = Random.int 60 in 
	let center = random_position 4 in 
	let new_position = random_pradius radius center in 
	let dist = magnitude (subt_v new_position center) in
		dist <= (float radius)  

let create_power_test = 
	let plist = (create_power ufo1 rplayer.p_pos bplayer.p_pos) in 
	(List.length plist) = cUFO_POWER_NUM 

let update_ufo_test = (update_ufo ulist) = []
let update_ufo_test1 = List.length (update_ufo ulist1) = 1
let update_ufo_test2 = List.length (update_ufo ulist2) = 4


