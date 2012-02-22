
// Single-case Active Patterns

[<Measure>]
type cm;

type Bread (name : string, hole : float<cm>) =
  member this.Desc = name;
  member this.Hole = hole;

let (|HoleSize|) (b : Bread) =
   b.Hole;

let holeDescription (bread: Bread) =
  match bread with
    | b when b.Hole > 10.0<cm> -> "big hole"
    | HoleSize 0.0<cm> -> "no hole at all"
    | HoleSize d when d > 5.0<cm> -> "medium hole"
    | _ -> "small hole"

let ciabatta = new Bread("Ciabatta", 5.3<cm>)

let hd = holeDescription ciabatta


// Partial Active Patterns

type Cat = { Name :string ; HeadDiameter : float<cm> ; HasTail : bool; TailDiameter : float<cm> }

let (|TailSize|_|) (c : Cat) =
  match c.HasTail with
   | true -> Some(c.TailDiameter)
   | false -> None

let tailDescription (size : float<cm>) (c :Cat) =
  match c with
   | TailSize t when t > size -> "this tail is too fat"
   | TailSize t when t = size -> "this tail is just right"
   | TailSize t when t < size -> "this tail is kinda skinny"
   | _ -> "No tail. Hopeless."

let garfield =  { Name= "Garfield"; HasTail= true; HeadDiameter= 14.3<cm>; TailDiameter= 3.0<cm> }
tailDescription 1.8<cm> garfield

tailDescription 1.8<cm> { garfield with HasTail = false}


// Multicase Active Patterns


let (|ProperlyBreadedCat|TailBreadedCat|HappyCat|) (c :Cat, b:Bread)=
  match b.Hole with
   | s when s > c.HeadDiameter ->  ProperlyBreadedCat (c.Name,b.Desc)
   | s when s > c.TailDiameter -> TailBreadedCat (c.Name,b.Desc)
   | _ -> HappyCat (c.Name)


let breadTheCat (b:Bread) (c:Cat) =
   match (c,b) with
     | ProperlyBreadedCat (catName, bread) -> sprintf "%s looks dapper in %s" catName bread
     | TailBreadedCat(catName, bread) -> sprintf "At least the %s hides %s's butt" bread catName
     | HappyCat (catName) -> sprintf "Lucky %s" catName
     // TODO: find picture of each type

// now one that looks like it might actually be useful


type ErrorMessage = { FieldName: string ; Message : string ; Severity : int }

let reportError em =
  if (em.Severity > 50) then
    // pop alert box
    printfn "Fatal error, doom"
  else if em.FieldName.Length > 0 then
    // associate error with field
    // move focus to field
    printfn "Error on field %s" em.FieldName
  else 
    // put general error on top of page
    ()

let (|FatalError|FieldError|GeneralError|) em =
  if (em.Severity > 50) then
    FatalError
  else if em.FieldName.Length > 0 then
    FieldError
  else 
    GeneralError

let reportError2 em =
  function
   | FatalError ->
      // pop alert box
      printfn "Fatal error, doom"
   | FieldError ->
      // associate error with field
      // move focus to field
      printfn "Error on field %s" em.FieldName
   | GeneralError ->
      // put general error on top of page
       ()





// Parameterized Active Patterns


let cats = ["Robert"; "Loki"; "Gremlin"]

let (|Contains|) (s : string) (l : string list) =
  List.exists ((=) s) l

let famousCat = "Robert" 

match cats with
 | Contains "Artemis" true -> printfn "Yay! We found our cat!"
 | Contains famousCat true -> printfn "A famous cat is in the room"
 | _ -> printfn "Just some cats" 


let (|CharAt|) (i: int) (s:string) =
   s.[i]

let chooseLine =
   function 
     | CharAt 1 'A' 
     | CharAt 1 'E' -> 1
     | CharAt 0 c when c < 'M' -> 2
     | _ -> 3

chooseLine "Loki"