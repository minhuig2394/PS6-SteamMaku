(*test bullet.ml*)
let rec get_vel lst rlst =
  	match lst with 
  	|h::t -> get_vel t (h.b_vel::rlst)
  	|[] -> rlst

 let get_ang angle lst = 
 let rec get_ang orang lst rlst = 
  	match lst with 
  	|(a,b)::t -> let res = ((atan2 b a) -. orang)*.(180./.pi) in get_ang orang t (res::rlst)
  	|[] -> rlst
  in get_ang angle (get_vel lst []) []
(*p1 is origin; p2 is target*)
let angle p1 p2 = 
	let x,y = subt_v p1.p_pos p2.p_pos
	in atan2 y x 

(*angles must start at 0*)
let within_bounds anglelst angle = 
	let x,_ = List.fold_left (fun acc elem -> 
		(((elem +. (float angle)) < ((snd acc +. 1.)) || 
			((elem +. (float angle)) > (snd acc -. 1.))||
			 ((elem -. (float angle)) < (snd acc +. 1.)) || 
			 ((elem -. (float angle)) > (snd acc -. 1.))) && 
			 (fst acc)), elem) (true,0.) anglelst  in x 

let bullet1 = {b_type = Spread;
				b_id = 1;
				b_pos = (0.,0.);
				b_vel = (1.,1.);
				b_accel = (0.,0.2);
				b_radius = cSPREAD_RADIUS;
				b_color = Blue}

let bullet2 = {b_type = Spread;
				b_id = 2;
				b_pos = (25., 25.);
				b_vel = (1.,1.);
				b_accel = (0.,0.2);
				b_radius = cSPREAD_RADIUS;
				b_color = Red}

let bullet3 = {b_type = Bubble;
				b_id = 3;
				b_pos = (float cBOARD_WIDTH, float cBOARD_HEIGHT);
				b_vel = (1.,1.);
				b_accel = (0.,0.2);
				b_radius = cBUBBLE_RADIUS;
				b_color = Red}

let bullet4 = {b_type = Bubble;
				b_id = 4;
				b_pos = (float cBOARD_WIDTH +. float cBUBBLE_RADIUS +. 10., float cBOARD_HEIGHT);
				b_vel = (1.,1.);
				b_accel = (0.,0.2);
				b_radius = cBUBBLE_RADIUS;
				b_color = Blue}

let bullet5 = {b_type = Trail;
				b_id = 5;
				b_pos = (float cBOARD_WIDTH +. float cTRAIL_RADIUS, 0.);
				b_vel = (1000.,1.);
				b_accel = (0.,0.2);
				b_radius = cTRAIL_RADIUS;
				b_color = Blue}

let bullet6 = {b_type = Trail;
				b_id = 6;
				b_pos = (15., 100.);
				b_vel = (1000.,1.);
				b_accel = (0.,0.2);
				b_radius = cTRAIL_RADIUS;
				b_color = Blue}

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
			u_red_hits = 7;
			u_blue_hits = 1;
			 }

let ufo2 = {u_id = 51;
			u_pos = (0.,0.);
			u_vel = (1.,1.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 7;
			u_blue_hits = 3;
			 }

let ufo3 = {u_id = 52;
			u_pos = (0.,0.);
			u_vel = (25.,25.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 7;
			u_blue_hits = 0;
			 }

let ufo4 = {u_id = 53;
			u_pos = (float cBOARD_WIDTH, float cBOARD_HEIGHT);
			u_vel = (100.,100.);
			u_radius = cUFO_RADIUS;
			u_red_hits = 7;
			u_blue_hits = 1;
			 }

let update1 = {rlost =false; 
   				blost =false; 
    			rgraze_pts = 2; 
    			bgraze_pts= 0; 
    			bullet_lst = bullets; 
    			ulst = ulist2; 
    			powerlst= [];
    			rpower_pts= 7;
    			bpower_pts= 3;
    			}
let update2 = {rlost =false; 
   				blost =false; 
    			rgraze_pts = 0; 
    			bgraze_pts= 0; 
    			bullet_lst = bullets3; 
    			ulst = [ufo2]; 
    			powerlst= [];
    			rpower_pts= 10;
    			bpower_pts= 0;
    			}
let update3 = {rlost =false; 
   				blost =false; 
    			rgraze_pts = 0; 
    			bgraze_pts= 0; 
    			bullet_lst = bullets2; 
    			ulst = ulist1; 
    			powerlst= [];
    			rpower_pts= 0;
    			bpower_pts= 0;
    			}
let update4 = {rlost =false; 
   				blost =false; 
    			rgraze_pts = 0; 
    			bgraze_pts= 0; 
    			bullet_lst = bullets3; 
    			ulst = ulist; 
    			powerlst= [];
    			rpower_pts= 0;
    			bpower_pts= 0;
    			}
let ulist = []
let ulist1 = [ufo1]
let ulist2 = [ufo1;ufo2;ufo3;ufo4]
let bullets = [bullet1;bullet2;bullet3;bullet4;bullet5;bullet6]
let bullets2 = []
let bullets3 = [bullet1]
let bullets4 = [bullet2;bullet3;bullet4]

let collide_test = (collide (rplayer.p_pos) (rplayer.p_radius) (bullet1.b_pos) (bullet1.b_radius)) = true 
let collide_test2 = (collide (rplayer.p_pos) (rplayer.p_radius) (bullet4.b_pos) (bullet4.b_radius)) = false
let set_pos_test = set_pos bullet1 = {bullet1 with b_pos = 1.,1.; b_vel = 1.,1.2}

let out_test = (out bullet1) = false 
let out_test2 = (out bullet4) = true

let hit_test = (hit bullet1 rplayer) = true
let hit_test2 = (hit bullet3 bplayer) = true
let hit_test3 = (hit bullet2 rplayer) = false

let grazed_test = (grazed bullet1 rplayer) = false 
let grazed_test2 = (grazed bullet1 bplayer) = false
let grazed_test3 = (grazed bullet2 rplayer)  = true 

let create_spread_test = 
	let splst = (create_spread rplayer.p_pos bplayer.p_pos Blue (0.,0.2)) in 
	((List.length splst) = cSPREAD_NUM &&
	(within_bounds (get_ang (angle bplayer rplayer) splst) 
			(360/cSPREAD_NUM)) = true)

let update_ufos_test = 
	let hit,pl,ul = update_ufos bullet2 update1 in 
	(List.length pl = 1) && 
	(List.length ul = 3) && 
	hit = true

let update_ufos_test1 =
	let hit,pl,ul = update_ufos bullet1 update2 in 
	(List.length pl = 1) && 
	(List.length ul = 0) && 
	hit = true


let update_bullets_test = 
	let new_update,pwr = update_bullets rplayer bplayer false
  	false bullets3 ulist1 [] in 
  	new_update = {rlost =true; 
   blost =false; 
    rgraze_pts = 0; 
    bgraze_pts= 0; 
    bullet_lst = []; 
    ulst = [ufo1]; 
    powerlst= pwr;
    rpower_pts= 0;
    bpower_pts= 0;
    }

let update_ufos_test1 =
	let hitu,pl,ul = update_ufos bullet1 update2 in 
	(List.length pl = 1) && 
	(List.length ul = 0) && 
	hitu = true

let update_bullets_test1 = 
	let new_update,pwr = update_bullets rplayer bplayer false
  	false [bullet1] [ufo2] [] in 
  	new_update = {rlost =true; 
   blost =false; 
    rgraze_pts = 0; 
    bgraze_pts= 0; 
    bullet_lst = []; 
    ulst = []; 
    powerlst= [];
    rpower_pts= 0;
    bpower_pts= 0;
    }

let update_bullets_test2 = 
	let new_update,pwr = update_bullets rplayer bplayer false
  	false [bullet1] [ufo3] [] in 
  	new_update = {rlost =true; 
   blost =false; 
    rgraze_pts = 0; 
    bgraze_pts= 0; 
    bullet_lst = []; 
    ulst = [{ufo3 with u_blue_hits = 1}]; 
    powerlst= [];
    rpower_pts= 0;
    bpower_pts= 0;
    }

let update_bullets_test3 = 
	let new_update,pwr = update_bullets rplayer bplayer true
  	false [bullet1] [ufo3] [] in 
  	new_update = {rlost =false; 
   blost =false; 
    rgraze_pts = 0; 
    bgraze_pts= 0; 
    bullet_lst = []; 
    ulst = [{ufo3 with u_blue_hits = 1}]; 
    powerlst= [];
    rpower_pts= 0;
    bpower_pts= 0;
    }

let updlst= 
    {red= rplayer; 
    blue= bplayer;
    rinvincible =false; 
    binvincible=false; 
    blst= [bullet1]; 
    ufolst= [ufo3]; 
    pwrlst= []}

let update_all_test = 
	let u = update_all updlst in 
	u.rlost = true &&
   u.blost =false &&
    u.rgraze_pts = 0 && 
    u.bgraze_pts= 0 && 
    u.bullet_lst = [] &&
    u.ulst = [{ufo3 with u_blue_hits = 1}] && 
    u.powerlst= [] &&
    u.rpower_pts= 0 &&
    u.bpower_pts= 0 
   
let updlst1= 
    {red= rplayer; 
    blue= bplayer;
    rinvincible =false; 
    binvincible=false; 
    blst= [bullet1]; 
    ufolst= [ufo2]; 
    pwrlst= []}

let update_all_test1 = 
	let u = update_all updlst1 in 
	u.rlost = true &&
   u.blost =false &&
    u.rgraze_pts = 0 && 
    u.bgraze_pts= 0 && 
    u.bullet_lst = [] &&
    u.ulst = [] && 
    not(u.powerlst= []) 











