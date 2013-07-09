let foregrounds = let open Ncurses in [|
  Color.White,	WA.Normal;	(* black	=>  0 *)
  Color.White,	WA.Bold;	(* white	=>  1 *)
  Color.Blue,	WA.Normal;	(* navy		=>  2 *)
  Color.Red,	WA.Normal;	(* red		=>  3 *)
  Color.Red,	WA.Bold;	(* orange	=>  4 *)
  Color.Blue,	WA.Bold;	(* blue		=>  5 *)
  Color.Red,	WA.Normal;	(* darkorange	=>  6 *)
  Color.Green,	WA.Normal;	(* green	=>  7 *)
  Color.Green,	WA.Bold;	(* lightgreen	=>  8 *)
  Color.Black,	WA.Bold;	(* grey		=>  9 *)
  Color.Yellow,	WA.Normal;	(* brown	=> 10 *)
  Color.Yellow,	WA.Bold;	(* gold		=> 11 *)
  Color.Cyan,	WA.Normal;	(* tan		=> 12 *)
  Color.White,	WA.Normal;	(* none		=> 13 *)
|]


let backgrounds = let open Ncurses in [|
  Color.Black,	WA.Normal;	(* black	=>  0 *)
  Color.White,	WA.Bold;	(* white	=>  1 *)
  Color.Blue,	WA.Normal;	(* navy		=>  2 *)
  Color.Red,	WA.Normal;	(* red		=>  3 *)
  Color.Red,	WA.Bold;	(* orange	=>  4 *)
  Color.Blue,	WA.Bold;	(* blue		=>  5 *)
  Color.Red,	WA.Normal;	(* darkorange	=>  6 *)
  Color.Green,	WA.Normal;	(* green	=>  7 *)
  Color.Green,	WA.Bold;	(* lightgreen	=>  8 *)
  Color.Black,	WA.Bold;	(* grey		=>  9 *)
  Color.Yellow,	WA.Normal;	(* brown	=> 10 *)
  Color.Yellow,	WA.Bold;	(* gold		=> 11 *)
  Color.Cyan,	WA.Normal;	(* tan		=> 12 *)
  Color.Black,	WA.Normal;	(* none		=> 13 *)
|]


let fg_color c pre =
  let open Ncurses in
  match c with
  | Color.Black		-> pre.[6] <- '3'; pre.[7] <- '0'
  | Color.Red		-> pre.[6] <- '3'; pre.[7] <- '1'
  | Color.Green		-> pre.[6] <- '3'; pre.[7] <- '2'
  | Color.Yellow	-> pre.[6] <- '3'; pre.[7] <- '3'
  | Color.Blue		-> pre.[6] <- '3'; pre.[7] <- '4'
  | Color.Magenta	-> pre.[6] <- '3'; pre.[7] <- '5'
  | Color.Cyan		-> pre.[6] <- '3'; pre.[7] <- '6'
  | Color.White		-> pre.[6] <- '3'; pre.[7] <- '7'


let bg_color c pre =
  let open Ncurses in
  match c with
  | Color.Black		-> pre.[11] <- '4'; pre.[12] <- '0'
  | Color.Red		-> pre.[11] <- '4'; pre.[12] <- '1'
  | Color.Green		-> pre.[11] <- '4'; pre.[12] <- '2'
  | Color.Yellow	-> pre.[11] <- '4'; pre.[12] <- '3'
  | Color.Blue		-> pre.[11] <- '4'; pre.[12] <- '4'
  | Color.Magenta	-> pre.[11] <- '4'; pre.[12] <- '5'
  | Color.Cyan		-> pre.[11] <- '4'; pre.[12] <- '6'
  | Color.White		-> pre.[11] <- '4'; pre.[12] <- '7'


let attribute a =
  let open Ncurses in
  match a with
  | WA.Normal		-> '0'
  | WA.Bold   		-> '1'
  | _			-> failwith "invalid attribute"


let grapheme darkness g =
  let open Types in
  let { foreground; background; text } = g in
  let foreground, background =
    if darkness < 50 then
      9, 0
    else
      foreground, background
  in

  let fg, fa = foregrounds.(foreground) in
  let bg, ba = backgrounds.(background) in

  let colour_on = "\027[.m\027[..m\027[..m" in
  colour_on.[2] <- attribute fa;
  fg_color fg colour_on;
  bg_color bg colour_on;

  let colour_off = "\027[0m" in

  colour_on ^ (text :> string) ^ colour_off
