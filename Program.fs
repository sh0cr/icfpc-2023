open System.Text.Json
open System.IO
open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Stats
open FSharp.Stats.Optimization


type Instrument = int

type Attendee = {
    x: float
    y: float
    tastes: float array
}

type Musician = {
    x: float
    y: float
    instrument: Instrument
}

type Stage = {
    width: float
    height: float
    corner: float array
}

let mutable stage: Stage = {width =0; height=0;corner =[||]}

let d(a:Attendee, m: Musician) =
    (pown (a.x - m.x) 2) + (pown (a.y - m.y) 2)

let dm(m1:Musician, m2: Musician) : float =
    (pown (m1.x - m2.x) 2) + (pown (m1.y - m2.y) 2)

let isBlock(at:Attendee, m1: Musician, m2: Musician) = 
    let a = at.y - m1.y
    let b = at.x - m1.x
    let c = -b * m1.y - a * m1.x
    let dist = abs(a*m2.x + b*m2.y + c) / sqrt(a*a + b*b)
    dist <= 5

let totalImpact(band: Musician array, a: Attendee) : float =
    let mutable impact = 0.

    for m in band do
        let mutable partialImpact = 0.
        partialImpact <- Math.Ceiling((1000000. * a.tastes[m.instrument]) / d(a,m))
        let otherBand = Array.except [|m|] band
        for other in otherBand do
            if isBlock(a, m, other) then partialImpact <- 0
        impact <- impact + partialImpact

    impact

let mutable attendees: Attendee array = [||]
//let mutable stage = (0.,0.,0.)

let score (band: Musician array)=
    let mutable s = 0.
    
    for i = 0 to band.Length-1 do 
        for j = i+1 to band.Length - 1 do
            if dm(band.[i], band.[j]) < 10 then 
                s <- Double.NegativeInfinity

    if Double.IsNegativeInfinity(s) then Double.PositiveInfinity
        else
            for a in attendees do
                s <- s + totalImpact(band, a)
            -s
let mutable instruments : Instrument array = [||]

let scorev(v: vector) =
    let mutable s = 0.
    let bandLocation= v |> Seq.chunkBySize 2
    let band = 
        Seq.map2 (fun (chunk: float array) ins -> {x = chunk.[0]; y=chunk.[1]; instrument = ins}) bandLocation instruments
        |> Seq.toArray
    score band

//#r "nuget: FSharp.Data"

let problem_id = "https://cdn.icfpcontest.com/problems/"

[<Literal>]
let sample = "https://cdn.icfpcontest.com/problems/1.json"
type Problem = JsonProvider<sample>

[<Literal>]
let sample2 = "./problems/1.json"
//type Problem = JsonProvider<sample2>

let loadProblem =
    Problem.Load(problem_id+"1.json")

//let jsonValueToAttendee(v:JsonValue) = {x=v?x.AsFloat(); y = v?y.AsFloat(); tastes = v?tastes.AsArray()|> Array.map (fun vi -> vi.AsFloat()) }


[<EntryPoint>]
let main args =
    let info = loadProblem
    let stageCorner = info.StageBottomLeft
    stage <- {width = info.StageWidth; height = info.StageHeight; corner = info.StageBottomLeft |> Array.map float}
    
    instruments <- info.Musicians
    let xBegin = Array.create instruments.Length (stageCorner.[0] + info.StageWidth/2)
    let yBegin = Array.create instruments.Length (stageCorner.[1] + info.StageHeight/2)
    let band = 
        Array.zip xBegin yBegin 
        |> Array.map (fun (x,y) -> [|x;y|])
        |> Array.concat
        |> Array.map float
        |> vector
        //info.Musicians |> Array.map (fun v -> {x=stage.[0]; y=stage.[1]; instrument=v})
    attendees <- info.Attendees |> Array.map (fun a -> {x=a.X; y = a.Y; tastes = a.Tastes |> Array.map float})
    //score band attendees

    let nmc = NelderMead.NmConfig.defaultInit()
    let optim = NelderMead.minimize nmc band scorev
    let placements = optim.SolutionVector |> Seq.chunkBySize 2

    
    0