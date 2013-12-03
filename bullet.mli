(*create_bullet takes in a bullet_type and returns a list of bullets with that bullet_type*)
val create_bullet: bullet_type -> int -> position -> color -> acceleration -> bullet list

(*update_blives does the following:
* 1. determines if bullets are out of bounds.  
*	If they are, removes them from the bullet list
*   If not, they are 
* 2. determines if the player has been hit by a bullet
*	Returns a boolean 
* 3. determines if the player has been grazed
*   Returns the number of grazes the player has experienced
* This is all ultimately represented as a tuple in the order of: 
* whether the player is hit, the number of grazes, and finally the list of
* bullets with the new positions 
*)
val update_blives: bullet list -> player_char -> (bool* int* bullet list)