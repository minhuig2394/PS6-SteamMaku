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

val newscore: bool -> bool -> int -> int -> int -> int

val newinvinc: bool -> bool -> int -> int

val newpower: bool -> bool -> int -> int -> int

val charge: int -> int -> bool -> bool -> int -> bool -> int

val newbombinv: bool -> bool -> bool -> int -> bool
