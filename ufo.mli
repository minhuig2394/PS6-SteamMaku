(*hit_ufo determines whether there is a collision between a bullet and a ufo*)
val hit_ufo : Definitions.bullet -> Definitions.ufo -> bool

(*set_pos_ufo returns the input ufo with its new position*)
val set_pos_ufo: Definitions.ufo ->Definitions.ufo

(*create_power u bpos rpos takes in a ufo, 
*the position of the red player and the position of 
*the blue player and creates and returns a list of power ups*)

val create_power : Definitions.ufo -> Definitions.position -> 
	Definitions.position -> Definitions.power list 

(*create_ufo creates a ufo: to be called every cUFO SPAWN INTERVAL timesteps*)
val create_ufo : 'a -> Definitions.ufo 

(*update_ufo updates a ufo : to be called at every cUFO MOVE INTERVAL timesteps*)
val update_ufo : Definitions.ufo list -> Definitions.ufo list


