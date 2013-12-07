type total_update = 
  {mutable rlost:bool; 
   mutable blost:bool; 
    mutable rgraze_pts: int; 
    mutable bgraze_pts: int; 
    mutable bullet_lst: Definitions.bullet list;
    mutable delete_bullets: Definitions.bullet list; 
    mutable ulst: Definitions.ufo list; 
    mutable powerlst: Definitions.power list;
    mutable rpower_pts: int;
    mutable bpower_pts: int;
    }

type update_info = 
  { red: Definitions.player_char; 
    blue: Definitions.player_char;
    rinvincible :bool; 
    binvincible:bool; 
    blst: Definitions.bullet list; 
    ufolst: Definitions.ufo list; 
    pwrlst: Definitions.power list}

(*create_bullet takes in a bullet_type, a target position, 
* a player color, and acceleration 
* and returns a list of bullets with that bullet_type*)
val create_bullet: Definitions.bullet_type -> Definitions.position -> 
  Definitions.position -> Definitions.color -> Definitions.acceleration -> 
    Definitions.bullet list

(*update_all does the following:
* 1. Handles all bullet collisions
* 2. Updates positions of all bullets, UFOs, and power ups
* 3. Handles collection of power ups
* This is all ultimately represented as a record of type total_updates
* 4. Handles creation of powerups when UFOs are destroyed
*)
val update_all: update_info -> total_update

(*delete_all removes all bullets from the gui, given the bullet list*)
val delete_all: Definitions.bullet list -> unit 

type update_info = 
  { red: Definitions.player_char; 
    blue: Definitions.player_char;
    rinvincible :bool; 
    binvincible:bool; 
    blst: Definitions.bullet list; 
    ufolst: Definitions.ufo list; 
    pwrlst: Definitions.power list}

(*create_bullet takes in a bullet_type, a target position, 
* a player color, and acceleration 
* and returns a list of bullets with that bullet_type*)
val create_bullet: Definitions.bullet_type -> Definitions.position -> 
  Definitions.position -> Definitions.color -> Definitions.acceleration -> 
    Definitions.bullet list

(*update_all does the following:
* 1. Handles all bullet collisions
* 2. Updates positions of all bullets, UFOs, and power ups
* 3. Handles collection of power ups
* This is all ultimately represented as a record of type total_updates
* 4. Handles creation of powerups when UFOs are destroyed
*)
val update_all: update_info -> total_update

(*delete_all removes all bullets from the gui, given the bullet list*)
val delete_all: Definitions.bullet list -> unit 