open Definitions
open Constants
open Util
open Bullet

let move pos direction speed = 
  let x = vector_of_dirs direction (float_of_int speed) in
  add_v pos x



