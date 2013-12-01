open Definitions
open Constants
open Game
open Util 

module type PLAYER = sig 
	type p 
	val player_id : p -> int 
	val player_pos : p -> position
	val player_focused: p -> bool
	val player_radius : p -> int 
	val player_color : p -> color 
end 

module player:PLAYER = struct 
	type p = player_char
	let player_id a = failwith ""
	let player_pos a= failwith ""
	let player_focused a =failwith ""
	let  player_radius a =failwith ""
	let player_color a = failwith ""
end

