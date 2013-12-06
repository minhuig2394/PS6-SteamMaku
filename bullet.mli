type total_update = 
  {mutable rlost:bool; 
   mutable blost:bool; 
    mutable rgraze_pts: int; 
    mutable bgraze_pts: int; 
    mutable bullet_lst: bullet list; 
    mutable ulst: ufo list; 
    mutable powerlst: power list;
    mutable rpower_pts: int;
    mutable bpower_pts: int;
    }

  type update_info = 
    {red: player_char; 
    blue: player_char;
    rinvincible :bool; 
    binvincible:bool; 
    blst: bullet list; 
    ufolst: ufo list; 
    pwrlst: power list}
    
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
val update_all: update_info -> total_update

(*delete_all removes all bullets from the gui, given the bullet list*)
val delete_all: Definitions.bullet list -> unit 