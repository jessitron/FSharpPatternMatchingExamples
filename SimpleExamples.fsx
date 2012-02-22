// Constants

let nut = "Almond"

let describe nut = 
  match nut with
    | "Almond" -> "I love almonds"
    | "Walnut" -> "Dang these things are hard to open"
    | "Pistachio" -> "Wow, these things grow here?"
    | _ -> "unidentified nut. better not eat it."

describe nut

// function syntax
let opinionate = 
  function
    | "Almond" -> "I love almonds"
    | "Walnut" -> "Dang these things are hard to open"
    | "Pistachio" -> "Wow, these things grow here?"
    | _ -> "unidentified nut. better not eat it."

opinionate "Pistachio" |> printfn "%s" 


// List - simple
let allTheNuts : string list = ["Almond"; "Walnut"; "Almond"]

let exclamation allTheNuts =
   match allTheNuts with
     | [_;_;_] -> "Three nuts! It is my lucky day"
     | [first;second] -> sprintf "%s for me and %s to share" first second
     | [one] -> "One nut, all alone: " + one
     | [] -> "Who stole my nuts?"
     | _ -> "Holy smokes that's a lot of nuts"

printfn "%s !!" (exclamation allTheNuts)


// List - cons pattern

let rec eatTheNuts  =
   function
    | [] -> printfn "All done."
    | head :: tail -> printfn "%s" (opinionate head); eatTheNuts tail

eatTheNuts allTheNuts

// Tuples

[<Literal>]
let FavoriteHidingPlace = "behind the drainpipe"

let bragAboutNutHole (a,b) = 
 match (a,b) with
  | (FavoriteHidingPlace, nut1 :: nut2 :: _) -> printfn "there is a %s and a %s in my favorite spot" nut1 nut2
  | (place, []) -> printfn "there are no nuts %s" place
  | _ -> printfn "Yeah, I got me some nuts but I'm not telling you where."

bragAboutNutHole ("under the porch", [])
bragAboutNutHole ("behind the drainpipe", allTheNuts)
bragAboutNutHole ("under the bush", ["Almond"])

// Pattern matching in function arguments

let giveNutsToSquirrel (squirrel, listOfNuts) =
   printfn "%s is so happy. he says:" squirrel
   eatTheNuts listOfNuts

giveNutsToSquirrel ("Frank", allTheNuts)

// Type Hierarchy
#r "System.Drawing.dll"
open System.Drawing

type Squirrel (color : Color, habitat : string) =
  member this.FurColor = color
  member this.Habitat = habitat
  member this.Breed (s : Squirrel) = new Squirrel(this.FurColor, s.Habitat)

type TreeSquirrel(color : Color) =
  inherit Squirrel (color, "forest")
  member this.Climb (tree : string) = printfn "yee haw"

type GroundSquirrel(color : Color) =
  inherit Squirrel (color, "prairie")
  member this.FavoriteFlower = "sunflower"

type Chipmunk() =
  inherit GroundSquirrel(Color.Brown)
  member this.FavoriteFlower = "daisy"

  // match based on type
let favoriteFood : Squirrel -> string =
  function
   | :? Chipmunk -> "small frogs"
   | :? GroundSquirrel as g -> sprintf "%A seeds" g.FavoriteFlower
   | :? TreeSquirrel as t when t.FurColor = Color.Gray -> "walnuts"
   | _ -> "almonds"

let describeFood (name : string) (s : Squirrel) = 
   printfn "%s is %A and likes to eat %s" name s.FurColor (favoriteFood s)

describeFood "Frank the chipmunk" (new Chipmunk())
describeFood "Shamik the tree squirrel" (new TreeSquirrel(Color.Gray))

