(*move takes in a position, direction, and speed (focused or unfocused) 
  and returns a position, which allows us to update the position
  of the player*)

val move_player: Definitions.position -> 
  (Definitions.direction * Definitions.direction) -> bool -> 
    Definitions.position
  
val g_result: int -> int -> int -> int -> float -> Definitions.result

val shoot: Definitions.bullet_type -> Definitions.position -> 
  Definitions.position -> Definitions.acceleration -> int 
    -> Definitions.color -> (Definitions.bullet list * int)
