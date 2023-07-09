module Program

open System.Text.Json
open System.IO
open System
open FSharp.Data
open FSharp.Stats
open MathNet.Numerics.Optimization
open MathNet.Numerics.LinearAlgebra

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
let sample = "https://cdn.icfpcontest.com/problems/1.json"
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
    x: int
    y: int
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

let inStage(m: Musician)=
    m.x >= stage.corner.[0] + 10. &&
    m.x <= stage.corner.[0] + stage.width - 10. &&
    m.y >= stage.corner.[1] + 10. &&
    m.y <= stage.corner.[1] + stage.height - 10.

let mutable attendees: Attendee array = [||]
let mutable pillars: Pillar array = [||]
let mutable volumes : float array = [||]

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
        // for pil in pillars do
        //     if isPillarBlock(a, m, pil) then partialImpact <- 0
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
let mutable instruments : Instrument array = [||]

let scorev v =
    let mutable s = 0.
    let bandLocation= v |> Seq.chunkBySize 2
    let band =
        Seq.map2 (fun (chunk: float array) ins -> {x = chunk.[0]; y=chunk.[1]; instrument = ins}) bandLocation instruments
        |> Seq.toArray
    score band

let getTrivialSolution problemId = 
    let info = loadProblem problemId
    let stageCorner = info.StageBottomLeft
    stage <- {width = info.StageWidth; height = info.StageHeight; corner = info.StageBottomLeft |> Array.map float}

    instruments <- info.Musicians

    let mutable bnd = Array.create info.Musicians.Length (0,0)
    let shift = 10
    let mutable (x,y) = stageCorner.[0] + 10 , stageCorner.[1] + 10
    let mutable row = 1
    for i = 0 to bnd.Length-1 do
        bnd.[i] <- (x,y)
        if x+shift > stageCorner.[0] + info.StageWidth - 10
            then
                row <- row + 1
                x <- stageCorner.[0] + 10
                y <- stageCorner.[1] + (shift * row)
        else
            x <- x + shift

    attendees <- info.Attendees |> Array.map (fun a -> {x=a.X; y = a.Y; tastes = a.Tastes |> Array.map float})
    volumes <- Array.create bnd.Length 1.
    
    let p: Placement array = Array.map (fun (a,b) -> {x = a; y = b}) bnd
    let musicians =
        Array.map2 (fun (a,b) ins -> {x = float a; y = float b; instrument = ins}) bnd instruments
    
    let mutable vols = Array.create bnd.Length 10.0
    let mutable scr = musicianImpcat(musicians[0], 0, musicians[1..])
    if scr < 0 then
        vols.[0] <- 0
        scr <- 0
    for i = 1 to musicians.Length - 1 do
        let otherBand = Array.concat  [ musicians[..i-1]; musicians[i+1..] ]
        let mImpact = musicianImpcat(musicians[i],i, otherBand)
        if mImpact < 0 then 
            vols.[i] <- 0.
        else 
            vols.[i] <- 10.0
            scr <- scr + mImpact * 10.

    let solutionJson = JsonSerializer.Serialize({placements = p; volumes = vols})
    use sw = new StreamWriter("./solutions/" + problemId.ToString() + "vol.json")
    sw.Write solutionJson
    use sw = new StreamWriter("./solutions/" + problemId.ToString() + "vol.txt")
    sw.Write scr


let f = ObjectiveFunction.Value scorev
 

type MusicScore() =

    interface IObjectiveFunction with
        member this.CreateNew(): IObjectiveFunction = 
            failwith "Not Implemented"
        member this.EvaluateAt(point: Vector<float>): unit = 
            failwith "Not Implemented"
        member this.Fork(): IObjectiveFunction = 
            failwith "Not Implemented"
        member this.Gradient: Vector<float> = 
            failwith "Not Implemented"
        member this.Hessian: Matrix<float> = 
            failwith "Not Implemented"
        member this.IsGradientSupported: bool = 
            failwith "Not Implemented"
        member this.IsHessianSupported: bool = 
            failwith "Not Implemented"
        member this.Point: Vector<float> = 
            failwith "Not Implemented"
        member this.Value: float = 
            failwith "Not Implemented"


[<EntryPoint>]
let main args =
    
    // let band =
    //     bnd
    //     |> Array.map (fun (x,y) -> [|x;y|])
    //     |> Array.concat
    //     |> Array.map float

    // // let nmc = NelderMead.NmConfig.defaultInit()
    // // let sropCriteria = OptimizationStop.StopCriteria.InitWith(1, 1e-4, 1, 1, Threading.CancellationToken.None)
    // // let optim = NelderMead.minimizeWithStopCriteria nmc band scorev sropCriteria
    //
    // let simplex = Simplex(MaxFunEvaluations=1, Tolerance=1e-4)
    // let solutionVector = simplex.ComputeMin(scorev, band)

    // let p =
    //     band
    //     |> Seq.chunkBySize 2
    //     |> Seq.toArray
    //     |> Array.map (fun chunk -> {x = chunk.[0]; y = chunk.[1]})

    for i = 2 to 55 do
        getTrivialSolution i
    0