(*move takes in a position, direction, and speed (focused or unfocused) 
  and returns a position, which allows us to update the position
  of the player*)

  val move: position -> (direction * direction) -> bool -> (float * float)
