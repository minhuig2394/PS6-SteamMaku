(*move takes in a position, direction, and speed (focused or unfocused) 
  and returns a position, which allows us to update the position
  of the player*)
val move_player: Definitions.position -> 
  (Definitions.direction * Definitions.direction) -> bool -> 
    Definitions.position

(*g_result takes in red player's lives, blue player's lives, 
  blue player's score, red player's score, and the timer count
  and returns a game result - Unfinished, Tie, Winner (Red), Winner (Blue)*)
val g_result: int -> int -> int -> int -> float -> Definitions.result

(*shoot takes in a bullet type, the target position, the player position, acceleration,
  player's charge, and the color of the player and returns a tuple of
  the list of bullets shot *)
val shoot: Definitions.bullet_type -> Definitions.position -> 
  Definitions.position -> Definitions.acceleration -> int 
    -> Definitions.color -> (Definitions.bullet list * int)

(*newscore calculates the new score for the player, taking in a boolean for if the player hit the enemy,
  a boolean for if the enemy is currently mercy invincible, number of grazes, 
  number of powers accumulated, the current score*)
val newscore: bool -> bool -> int -> int -> int -> int

(*newinvinc calculates the new invincibility timer based on a boolean of if the player was hit in the past timestep,
  if the player was under mercy invincibility, and the player's current invincibility timer*)
val newinvinc: bool -> bool -> int -> int

(*newpower calculates the amount of power the player currently has by taking in a boolean of whether the player was hit
  in the past timestep, if the player was under mercy invincibility, the player's current power,
  the amount of power the player accumulated in the past timestep*)
val newpower: bool -> bool -> int -> int -> int

(*charge calculates the player's new charge based on the player's current charge, 
  current power, boolean of if the player was hit in the past time interval, 
  if the player was under mercy invincibility, the amount of power accumulated in the past time interval,
  and if the player is under bomb invincibility*)
val charge: int -> int -> bool -> bool -> int -> bool -> int

(*newbombinv determines whether or not a player is under bomb invincibility, taking in
  a boolean of the player's current bomb invincibility status,
  a boolean of if the player was hit in the past time interval,
  a boolean of if the player is under mercy invincibility,
  and the player's current invincibility timer*)
val newbombinv: bool -> bool -> bool -> int -> bool
