type total_update 
(*create_bullet takes in a bullet_type, a target position, 
* a player color, and acceleration 
* and returns a list of bullets with that bullet_type*)
val create_bullet: Definitions.bullet_type -> Definitions.position -> 
	Definitions.position -> Definitions.color -> Definitions.acceleration -> 
		Definitions.bullet list

(*update_all does the following:
* 1. determines if bullets are out of bounds.  
*	If they are, removes them from the bullet list
* 2. determines if the player has been hit by a bullet
*	Returns a boolean 
* 3. determines if the player has been grazed
*   Returns the number of grazes the player has experienced
* This is all ultimately represented as a record of type total_updates
*)
val update_all: Definitions.player_char -> Definitions.player_char -> 
	bool -> bool -> Definitions.bullet list -> Definitions.ufo list -> 
		Definitions.power list -> total_update

(*delete_all removes all bullets from the gui, given the bullet list*)
val delete_all: Definitions.bullet list -> unit 