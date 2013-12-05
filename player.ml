open Definitions
open Constants
open Util
open Bullet

let move_player pos direction foc =
  let flwidth = float_of_int cBOARD_WIDTH in
  let flheight = float_of_int cBOARD_HEIGHT in
  if foc = true then
    let x = vector_of_dirs direction (float_of_int cFOCUSED_SPEED) in
    let (a, b) = add_v pos x in
    if a < 0. || b < 0. || a > flwidth || a > flheight ||
    b > flheight || b > flwidth then pos
    else (a, b)
  else
    let y = vector_of_dirs direction (float_of_int cUNFOCUSED_SPEED) in
    let (a, b) = add_v pos y in
    if a < 0. || b < 0. || a > flwidth || a > flheight ||
    b > flheight || b > flwidth then pos
    else (a, b)

let g_result redl bluel redsc bluesc t =
  if redl = 0 && bluel = 0 then Tie
  else if redl = 0 && bluel <> 0 then Winner (Blue)
  else if redl <> 0 && bluel = 0 then Winner (Red)
  else if t = 0. then 
    if redsc = bluesc then Tie
    else if redsc > bluesc then Winner (Red)
    else Winner (Blue)
  else Unfinished

let shoot btype tpos plpos acc charge color = 
  if btype = Spread then
    if charge < cSPREAD_COST then ([], 0)
    else (create_bullet btype tpos plpos color acc, cSPREAD_COST)
  else if btype = Trail then
    if charge < cTRAIL_COST then ([], 0)
    else (create_bullet btype tpos plpos color acc, cTRAIL_COST)
  else 
    if charge < cBUBBLE_COST then ([], 0)
    else (create_bullet btype tpos plpos color acc, cBUBBLE_COST)
