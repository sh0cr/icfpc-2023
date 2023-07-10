open System.IO
open System.Text.Json
open System.Net.Http
open System.Net.Http.Json
open System.Net.Http.Headers

let token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiI2NGE1NmNjYjhjNjg1MzEzZDFjNjBkZDIiLCJpYXQiOjE2ODg2NzczNjMsImV4cCI6MTY5ODY3NzM2M30.gbuvWCL0ozLuuXtHX_61sry8PQsP5uTzAFXW99qLkso"
let cdnEdnpoint = "https://cdn.icfpcontest.com/submission"
let apiEndpoint = "https://api.icfpcontest.com/submission"

type SolutionBody = {
    problem_id: int
    contents: string
}

let loadSolution problemId =
    let path = "./grouped_solutions/" + problemId.ToString() + "vol.json"
    use sr = new StreamReader(path)
    let placements = sr.ReadToEnd()
    let body = {problem_id = problemId; contents = placements}
    //let json = JsonSerializer.Serialize(body)
    
    task{
        use client = new HttpClient()
        let! response = 
            let url = apiEndpoint
            client.DefaultRequestHeaders.Authorization <- new AuthenticationHeaderValue("Bearer", token)
            client.PostAsJsonAsync(url, body)
        let! subId = response.Content.ReadAsStringAsync()
        ()
        //printfn "%s" subId
    } |> Async.AwaitTask
    |> Async.RunSynchronously



[| 1;52;53 |]
|> Array.iter (fun i -> loadSolution i)