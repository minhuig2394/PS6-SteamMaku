open Definitions
open Constants
open Game
open Util 

module type BULLET = sig 
	type b 
	val bullet_type: b -> bullet_type
	val bullet_id : b -> int
	val bullet_pos: b -> position
	val bullet_vel: b -> velocity 
	val bullet_accel: b -> acceleration
	val bullet_radius : b -> int 
	val bullet_color : b -> color
end 



module Bullets:BULLET = struct
	type b = bullet
	let bullet_type a= a.b_type
	let bullet_id a= a.b_id
	let bullet_pos a= 
		match a with 
		|Spread -> failwith ""
		|Bubble -> failwith ""
		|Power -> failwith ""
		|Trail -> failwith ""
	let bullet_vel a = 
		match a with 
		|Spread -> cSPREAD_SPEED 
		|Bubble -> cBUBBLE_SPEED 
		|Power -> cPOWER_SPEED
		|Trail -> cTRAIL_SPEED_STEP 
	let bullet_accel a = 
		match a with 
		|Spread -> Spread 
		|Bubble -> Bubble
		|Trail -> Trail 
	let bullet_radius a=
		match a with 
		|Spread -> cSPREAD_RADIUS
		|Bubble -> cBUBBLE_RADIUS 
		|Power -> cPOWER_RADIUS
		|Trail -> cTRAIL_RADIUS 
	let bullet_color a= failwith ""

end 