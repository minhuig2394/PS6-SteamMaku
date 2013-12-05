open Definitions
open Constants
open Util
open Bullet

let move pos direction foc = 
  if foc = true then
  let x = vector_of_dirs direction (float_of_int cFOCUSED_SPEED) in
  add_v pos x
  else
    let y = vector_of_dirs direction (float_of_int cUNFOCUSED_SPEED) in
    add_v pos y
