open Definitions
open Constants
open Util
open Bullet

let move_player pos direction foc =
  if foc = true then
    let x = vector_of_dirs direction (float_of_int cFOCUSED_SPEED) in
    let (a, b) = add_v pos x in
    let flwidth = float_of_int cBOARD_WIDTH in
    let flheight = float_of_int cBOARD_HEIGHT in
    if a < 0. || b < 0. || a > flwidth || a > flheight ||
    b > flheight || b > flwidth then pos
    else (a, b)
  else
    let y = vector_of_dirs direction (float_of_int cUNFOCUSED_SPEED) in
    let (a, b) = add_v pos y in
    let flwidth = float_of_int cBOARD_WIDTH in
    let flheight = float_of_int cBOARD_HEIGHT in
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

let newlife current hit = 
  if hit = true then current - 1
  else current

let newscore enemyhit grazes powers score =
  let gpoints = grazes * cGRAZE_POINTS in
  let powpoints = powers * cPOWER_POINTS in
  if enemyhit = true then
    gpoints + powpoints + cKILL_POINTS + score
  else gpoints + powpoints + score

let newinvinc hit current =
  if hit = true then cINVINCIBLE_FRAMES
  else current - 1

let newpower curpower hit acc =
  if hit = true then (curpower + acc) / 2 
  else curpower + acc

let charge current cpower hit acc bombinv =
  if bombinv = true then current 
  else
    let pow = newpower cpower hit acc in
    if current = cCHARGE_MAX then cCHARGE_MAX
    else if current + pow + cCHARGE_RATE >= cCHARGE_MAX then cCHARGE_MAX
    else current + cCHARGE_RATE + pow

let newbombinv cbombinv hit currinv = 
  if newinvinc hit currinv = 0 then false 
  else cbombinv
