#r "System.Web.Extensions.dll"

open System.IO
open System.Net
open System.Collections.Generic
open System.Web.Script.Serialization

// How to access the tumblr api
let makeCommand command tumblr = sprintf "http://api.tumblr.com/v2/blog/%s/%s?api_key=Ruv7vxEzH3tZxkJ0buyC3U7wgNvekFRZvmwHOI9Gmp01aFYoau" tumblr command
let makePostsUrl = makeCommand "posts"

let downloadWebpage (url : string) =
        let resp =  WebRequest.Create(url).GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        reader.ReadToEnd()

let parseJSON jsonTxt =
  let jss = new JavaScriptSerializer();
  jss.DeserializeObject(jsonTxt) :?> Dictionary<string, obj>

// active patterns to parse the JSON response!
let (|ResponseCode|) (d : Dictionary<string, obj>) =
   let meta = d.["meta"] :?> Dictionary<string,obj>
   meta.["status"] :?> int

let isADictionary o = typeof<Dictionary<string,obj>> = o.GetType()

let (|ContainsDict|_|) (name : string) (d :Dictionary<string,obj>)=
     match d.TryGetValue(name) with
       | (true, value) when isADictionary(value) -> Some(value :?> Dictionary<string,obj>)
       | (true, v) -> None
       | (false, _) ->None

let (|ContainsArray|_|) (name : string) (d :Dictionary<string,obj>)=
     match d.TryGetValue(name) with
       | (true, value) when typeof<obj array> = value.GetType() -> Some(value :?> obj array)
       | _ ->None

//
// Contact tumblr and parse the response!
//
let getPosts tumblr =
  let response = makePostsUrl tumblr |> downloadWebpage |> parseJSON
  match response with
   | ResponseCode 404 -> printfn "Warning! tumblr not found"; [||]
   | ResponseCode 500 
   | ResponseCode 401 -> failwith "Serious error"
   | ResponseCode 200 &
     ( ContainsDict "response" (ContainsArray "posts" arrayOfPosts))
       -> arrayOfPosts
   | _ -> [||]

let allPosts = getPosts "breadedcats.com"


//
// We can do more processing of the posts if we like.
//

let (|ContainsString|_|) (name : string) (d :Dictionary<string,obj>)=
     match d.TryGetValue(name) with
       | (true, value) when typeof<string> = value.GetType() -> Some(value :?> string)
       | _ ->None

let (|ContainsInt|_|) (name : string) (d :Dictionary<string,obj>)=
     match d.TryGetValue(name) with
       | (true, value) when typeof<int> = value.GetType() -> Some(value :?> int)
       | _ -> None

let (|SingleItemDict|_|) (o: obj array) =
   match o with
    | [|i|] when isADictionary(i) -> Some(i :?> Dictionary<string,obj>)
    | _ -> None

let breadedCatReport (o:obj) =
  let d = o :?> Dictionary<string,obj>
  match d with 
   | ContainsString "type" "photo"
     & ContainsInt "note_count" 0 -> printfn "(not important)"
   | ContainsString "type" "photo" 
     & ContainsArray "photos"
        (SingleItemDict 
        (ContainsDict "original_size" 
          (ContainsString "url" u
            & ContainsInt "width" w
           & ContainsInt "height" h)
         )
        ) -> printfn "breaded cat of size %d x %d at %s" w h u
   | _ -> ()

Array.iter breadedCatReport allPosts


