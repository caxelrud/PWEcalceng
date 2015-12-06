//PWEcalceng_1.fsx

//RX
#I @"C:\Project(Comp)\Dev_2015\PWEcalceng"
#r @".\lib\System.Reactive.Core.dll"
#r @".\lib\System.Reactive.Interfaces.dll"
#r @".\lib\System.Reactive.Linq.dll"
#r @".\lib\System.Reactive.PlatformServices.dll"
#r @".\lib\System.Reactive.Providers.dll"
#r @".\lib\Microsoft.Reactive.Testing.dll"
#r @".\lib\FSharp.Control.Reactive.dll" //Need to be the last reference
#load "FSRxUtil.fsx"

//FSODE
#load "FSODELib.fsx"

//FSCharting
#r @"System.Windows.Forms.DataVisualization"
#load @".\lib\FSharp.Charting.fsx"

//System
open System
open System.Collections.Generic

//RX
open System.Reactive.Linq
//open System.Reactive
//open System.Reactive.Disposables
//open System.Reactive.Concurrency
open System.Reactive.Subjects
//open Microsoft.Reactive.Testing

//FS-RX
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Builders
open FSharp.Control.Reactive.Observable
open FSRxUtil

//FSCharting
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open FSharp.Charting

//FSODE
open FSODELib

//Example 1:
(* The equation is: 
 dx1/dt = 1 + x1*x1*x2 - 4*x1, 
 dx2/dt = 3*x1 - x1*x1*x2, 
 x1(0) = 1.01, x2(0) = 3.0,
  0 <= t <= 180.0
*)
//Model
let f1(t:float,x:float array,U:float array)=
    let n=x.GetLength(0)
    let dx=Array.zeroCreate n
    dx.[0]<-1.0 + x.[0]*x.[0]*x.[1] - 4.0*x.[0]
    dx.[1]<-3.0*x.[0] - x.[0]*x.[0]*x.[1]
    dx

//Test ODE routine
let t=0.0
let dt=1.0
let h=0.01
let x=[|1.01;3.0|]
let U=[|0.0|]
ODE1.rk4Multi f1 h t dt x U
(* >val it : float * float array = (1.0, [|1.033097709; 2.955539415|]) *)

//Data stream
type odeStream2d={dT:float;h:float;mutable x:float array; mutable U:float array; mutable Ti:float;mutable Tf:float}

//Transformation
let ode2d (r:odeStream2d) (T:int64)=
    r.Ti<-(float T)
    r.Tf<-r.Ti+r.dT
    let r1=(ODE1.rk4Multi f1 r.h r.Ti r.dT r.x r.U)
    r.x<-snd r1 
    r.Tf<-fst r1
    r

//Simulation starting point
//Observable
let od1={dT=0.1;h=0.01;x=[|1.01;3.0|];U=[|0.0|];Ti=0.0;Tf=0.0}

let obs1 = Observable.Interval(TimeSpan.FromSeconds(0.1)).Publish()
obs1.Connect()

let obs2=obs1|> Observable.take (5*60)
obs2 |> Observable.subscribe (fun x ->  printfn "x:%d" x) 

let obs3=obs2 |> Observable.scanInit od1 (fun (od:odeStream2d) (T:int64) -> ode2d od T)

obs3 |> Observable.subscribe (fun r ->  printfn "Ti:%f,Tf:%f,x:%A" r.Ti r.Tf r.x) 

//Incremental 
let obs4 = obs3 |> Observable.map(fun r -> DateTime.Now.ToShortDateString(),r.x.[0])
let obs5 = obs3 |> Observable.map(fun r -> DateTime.Now.ToShortDateString(),r.x.[1])


let P1=LiveChart.FastLineIncremental(obs4,Name="FastLineIncr1",Title="FastLineIncr Chart 1")
                .WithXAxis(Enabled=false).WithYAxis(Enabled=false)
let P2=LiveChart.FastLineIncremental(obs5,Name="FastLineIncr2",Title="FastLineIncr Chart 2")
                .WithXAxis(Enabled=false).WithYAxis(Enabled=false)

//Scroll
let mutable TS1 = 
  [ for x in 0 .. 3*60-1 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),0.0 ]
let mutable TS2 = 
  [ for x in 0 .. 3*60-1 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),0.0 ]

let obs6 = obs3
           |> Observable.map (fun r->
                                TS1<-TS1.Tail @ [DateTime.Now.ToShortDateString(),r.x.[0]] 
                                TS1 
                                 ) 
let obs7 = obs3
           |> Observable.map (fun r->
                                TS2<-TS2.Tail @ [DateTime.Now.ToShortDateString(),r.x.[1]] 
                                TS2 
                                 ) 

let P3=LiveChart.FastLine(obs6,"Scrolling 1","Scrolling 1")
let P4=LiveChart.FastLine(obs7,"Scrolling 2","Scrolling 2")

(*
//Example2: Scroll
let mutable TS1 = 
  [ for x in 0 .. 99 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),sin(float x / 10.0) ]
//Chart.Line(TS1)

let mutable X=99
let obs2 = Observable.Interval(TimeSpan.FromSeconds(1.0))
           |> Observable.map (fun _->
                                X<-X+1
                                TS1<-TS1.Tail @ [DateTime.Now.AddSeconds(float X).ToShortDateString(),sin(float X / 10.0)] 
                                TS1 
                                 ) 
LiveChart.Line(obs2)
*)
