open System.Text.Json
open System.IO

type Soluton ={
    id: int
    data: string
}

[<EntryPoint>]
let main args =
    let s1 = {id = 0; data ="test0"}
    use sw = new StreamWriter(Path.Combine(Directory.GetCurrentDirectory(), "solutions", "s1.json"))
    JsonSerializer.Serialize s1 |> sw.WriteLine
    0

