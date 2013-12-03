val set_pos: bullet -> unit

(*create_bullet takes in a bullet_type and returns a list of bullets with that bullet_type*)
val create_bullet: bullet_type -> int -> position -> color -> acceleration -> bullet list


val update_bullet: 