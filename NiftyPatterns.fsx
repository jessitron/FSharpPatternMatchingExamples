// Records
[<Measure>]
type g

type NutritionalInfo = { Protein : float<g>; Carbs: float<g>; Fat : float<g>}

let todaysDiet =
  function
   | { Carbs = 0.0<g>} -> "Atkins"
   | { Protein = p; Carbs = c } when p/c > (0.777) -> "Zone"
   | { Fat = f } when f < 2.0<g> -> "Low-fat"
   | _ -> "Seefood"

let italianBread = {Protein= 2.6<g>; Carbs = 15.0<g>; Fat = 1.1<g> }

let rationalization = todaysDiet italianBread


// Discriminated Unions

type TumblrPost =
  | PhotoPost of int * int * string 
  | QuotePost of string * string
  | TextPost of string

let displayElement =
 function
  | PhotoPost (w,h,url) -> sprintf "<img width=%d height=%d href=\"%s\"></img>" w h url
  | QuotePost (said,whoSaidIt) -> sprintf "<h3>%s</h3><p>%s</p>" said whoSaidIt
  | TextPost (text) -> sprintf "<p>%s</p>" text

let e = displayElement (PhotoPost (480, 643, @"http://27.media.tumblr.com/tumblr_lzpsri3lIh1roe4b7o1_500.jpg"))