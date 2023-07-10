module Program

open System.Text.Json
open System.IO
open System
open FSharp.Data
open FSharp.Stats
open MathNet.Numerics.Optimization
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

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

type MusicianWithId={
    id: int
    x: float
    y: float
    instrument: Instrument
}

type Pillar = {
    center: float array
    radius: float
}

type Stage = {
    width: float
    height: float
    corner: float array
}

let apiProblems = "https://cdn.icfpcontest.com/problems/"

[<Literal>]
let sample = "https://cdn.icfpcontest.com/problems/56.json"
type Problem = JsonProvider<sample>

let loadProblem (i:int) =
    Problem.Load(apiProblems + $"{i}.json")

[<Literal>]
let solutionSample =
    """
{
  "placements": [
    {
      "x": 590.1,
      "y": 10.2
    },
    {
      "x": 1100.1,
      "y": 150.3
    }
  ]
}
"""

type Placement ={
    x: float
    y: float
}

type Solution = {
    placements: Placement array
    volumes: float array
}

type Room = {
    width: int
    height: int
    stageWidth: int
    stageHeight: int
    stageBottomLeft: int*int
    musicians: int array
    attendees: Attendee array
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
    dist < 5

let isPillarBlock (at:Attendee, m: Musician, pil) =
    let a = at.y - m.y
    let b = at.x - m.x
    let c = -b * m.y - a * m.x
    let dist = abs(a*m.x + b*m.y + c) / sqrt(a*a + b*b)
    dist < pil.radius

let inStage(m: Musician)=
    m.x >= stage.corner.[0] + 10. &&
    m.x <= stage.corner.[0] + stage.width - 10. &&
    m.y >= stage.corner.[1] + 10. &&
    m.y <= stage.corner.[1] + stage.height - 10.

let mutable attendees: Attendee array = [||]
let mutable pillars: Pillar array = [||]
let mutable volumes : float array = [||]
let mutable instruments : Instrument array = [||]

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


let musicianImpcat (m: Musician, i: int, otherBand: Musician array) =
    let mutable impact = 0.
    for a in attendees do
        let mutable partialImpact = 0.
        partialImpact <- Math.Ceiling((1000000. * a.tastes[m.instrument]) / d(a,m))
        for other in otherBand do
            if isBlock(a, m, other) then partialImpact <- 0
        for pil in pillars do
            if isPillarBlock(a, m, pil) then partialImpact <- 0
        impact <- impact + partialImpact 
    if impact >= 0 then 
        volumes.[i] <- 10.
        impact*10.
    else
        volumes.[i] <- 0
        0

let score (band: Musician array)=
    let mutable s = 0.

    for i = 0 to band.Length-1 do
        if not (inStage band[i]) then s <- Double.NegativeInfinity
        else
            for j = i+1 to band.Length - 1 do
                if dm(band.[i], band.[j]) < 10 then
                    s <- Double.NegativeInfinity

    if Double.IsNegativeInfinity(s)
        then Double.PositiveInfinity
        else
            for i = 0 to band.Length - 1 do 
                let otherBand =
                    match i with
                    | 0 -> band[1..]
                    | k when k = band.Length-1 -> band[..k-1]
                    | _ -> Array.concat [band[..i-1]; band[i+1..]]
                s <- s + musicianImpcat(band[i], i, otherBand)
            -s


let scorev v =
    let mutable s = 0.
    let bandLocation= v |> Seq.chunkBySize 2
    let band =
        Seq.map2 (fun (chunk: float array) ins -> {x = chunk.[0]; y=chunk.[1]; instrument = ins}) bandLocation instruments
        |> Seq.toArray
    score band

let saveSolution ar problemId =
    let p =
        ar |> Array.chunkBySize 2
        |> Array.map (fun chunk -> {x= chunk.[0]; y = chunk.[1]})
    let solutionJson = JsonSerializer.Serialize({placements = p; volumes = volumes})
    use sw = new StreamWriter("./optimized_solutions/" + problemId.ToString() + "vol.json")
    sw.Write solutionJson

let config problemId =
    let info = loadProblem problemId

    stage <- {width = info.StageWidth; height = info.StageHeight; corner = info.StageBottomLeft |> Array.map float}
    instruments <- info.Musicians
    attendees <- info.Attendees |> Array.map (fun a -> {x=a.X; y = a.Y; tastes = a.Tastes |> Array.map float})
    volumes <- Array.create info.Musicians.Length 1.
    pillars <- info.Pillars |> Array.map (fun jp -> {center = [|jp.Center.[0]; jp.Center.[0]|]; radius = jp.Radius})
    info

let getTrivialSolution problemId  =
    let info = config problemId
    let mutable bnd: (int * int) array = Array.create info.Musicians.Length (0,0)
    let shift = 10
    let mutable (x,y) = info.StageBottomLeft.[0] + 10 , info.StageBottomLeft.[1] + 10
    let mutable row = 1
    for i = 0 to bnd.Length-1 do
        bnd.[i] <- (x,y)
        if x+shift > info.StageBottomLeft.[0] + info.StageWidth - 10
            then
                row <- row + 1
                x <- info.StageBottomLeft.[0] + 10
                y <- info.StageBottomLeft.[1] + (shift * row)
        else
            x <- x + shift  
    bnd

let getOptimizedSolution problemId = 

    let initialGuess =
        getTrivialSolution problemId
        |> Array.map (fun (a,b) -> [| float a; float b|])
        |> Array.concat
        |> DenseVector
    
    let objFunction = ObjectiveFunction.Value scorev
    let minResult = NelderMeadSimplex.Minimum(objFunction, initialGuess, attendees.Length * instruments.Length, 10)
    
    saveSolution (minResult.MinimizingPoint.AsArray()) problemId

let groupMusician problemId = 
    let info = config problemId
    let mutable musicians =  
        Array.mapi (fun i inst -> {id=i; x = float(info.StageBottomLeft.[0] + 10); y=float(info.StageBottomLeft.[1] + 10); instrument = inst}) info.Musicians
        |> Array.sortBy (fun m -> m.instrument)
    let shift = 10
    let mutable (dx,dy) = info.StageBottomLeft.[0] + 10 , info.StageBottomLeft.[1] + 10
    let mutable row = 1
    for i = 0 to musicians.Length-1 do
        musicians.[i] <- {id = musicians.[i].id; x = dx; y = dy; instrument = musicians.[i].instrument}
        if dx+shift > info.StageBottomLeft.[0] + info.StageWidth - 10
            then
                row <- row + 1
                dx <- info.StageBottomLeft.[0] + 10
                dy <- info.StageBottomLeft.[1] + (shift * row)
        else
            dx <- dx + shift

    for i = 0 to musicians.Length - 1 do 
        let otherBand =
            match i with
            | 0 -> musicians[1..]
            | k when k = musicians.Length-1 -> musicians[..k-1]
            | _ -> Array.concat [musicians[..i-1]; musicians[i+1..]]
        let otherBandWithoutId = Array.map (fun (mid: MusicianWithId) -> {x = mid.x; y = mid.y; instrument = mid.instrument}) otherBand
        let mw = musicians.[i]
        musicianImpcat({x = mw.x; y = mw.y; instrument = mw.instrument}, mw.id, otherBandWithoutId) |> ignore
    
    let places: Placement array = 
        Array.sortBy (fun m -> m.id) musicians
        |> Array.map (fun m -> {x= m.x; y = m.y})
    let solutionJson = JsonSerializer.Serialize({placements = places; volumes = volumes})
    use sw = new StreamWriter("./grouped_solutions/" + problemId.ToString() + "vol.json")
    sw.Write solutionJson

    


[<EntryPoint>]
let main args =
    let failed = [| 1;52;53|]

    for pid in failed do
        try groupMusician pid
        with | :? MaximumIterationsException -> printfn $"fail at {pid}"
    
    0