//PWEcalceng_1.fsx

#I @"C:\Project(Comp)\Dev_2015\PWEcalceng"

//RX
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

//==============================================================================================
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

//Example 2:
(*
x=[Ca<0>,T<1>]
U=[F<0>,Caf<1>,Tf<2>,Tj<3>]

dCa/dt=(F/V)*(Caf-Ca)-r
dT/dt=(F/V)*(Tf-T)+(-dH/rhocp)*r-(UA/rhoCp)*(T-Tj)
r=ko*exp(-dE/(R*T))*Ca
*)
//Model
let f2(t:float,x:float array,U:float array)=
    let n=x.GetLength(0)
    let dx=Array.zeroCreate n
    let V=1.0
    let dE=11843.0
    let ko=9703.0*3600.0
    let R=1.987
    let dH=(-5960.0)
    let rhocp=500.0
    let UA=150.0
    let r=ko*exp(-dE/(R*x.[1]))*x.[0]
    dx.[0]<-(U.[0]/V)*(U.[1]-x.[0])-r
    dx.[1]<-(U.[0]/V)*(U.[2]-x.[1])+(-dH/rhocp)*r-(UA/(V*rhocp))*(x.[1]-U.[3])
    dx

//Test ODE routine
let t2=0.0
let dt2=1.0 //need to be compatible with the timer
let h2=0.01
let x_1=[|8.5636;311.1710|] //High Conc.,Low Temp.
let x_2=[|5.51;339.0971|]   //Interm. Conc.,Temp.
let x_3=[|2.3589;368.0629|] //Low Conc.,High Temp.

let x2=[|9.0;300.0|]
let U2=[|1.0;10.0;25.0+273.15;25.0+273.15|]  //[F<0>,Caf<1>,Tf<2>,Tj<3>]

ODE1.rk4Multi f2 h2 t2 dt2 x2 U2
(* val it : float * float array = (1.0, [|9.056988745; 304.765492|]) *)

//Data stream
type odeStream2={dT:float;h:float;mutable x:float array; mutable U:float array; mutable Ti:float;mutable Tf:float}

//Transformation
let odeTransf2 (r:odeStream2) (T:int64)=
    r.Ti<-(float T)
    r.Tf<-r.Ti+r.dT
    let r1=(ODE1.rk4Multi f2 r.h r.Ti r.dT r.x r.U)
    r.x<-snd r1 
    r.Tf<-fst r1
    r

//Simulation starting point--------------------------------------------------------------------------
//1sec(real)->1min(sim)

//Observable
let od2={dT=1.0/60.0;h=0.1/60.0;x=[|9.0;300.0|];U=[|1.0;10.0;25.0+273.15;25.0+273.15|];Ti=0.0;Tf=0.0}

let obs10 = Observable.Interval(TimeSpan.FromSeconds(1.0)).Publish()
obs1.Connect()

let obs12=obs10|> Observable.take (5*60)
//obs2 |> Observable.subscribe (fun x ->  printfn "count:%d" x) 

let obs13=obs12 |> Observable.scanInit od2 (fun (od:odeStream2) (T:int64) -> odeTransf2 od T)

//obs13 |> Observable.subscribe (fun r ->  printfn "Ti:%f,Tf:%f,x:%A" r.Ti r.Tf r.x) 

//Incremental 
let obs14 = obs13 |> Observable.map(fun r -> DateTime.Now.ToShortDateString(),r.x.[0]-8.5)
let obs15 = obs13 |> Observable.map(fun r -> DateTime.Now.ToShortDateString(),r.x.[1]-311.0)
let obs16 = obs13 |> Observable.map(fun r -> DateTime.Now.ToShortDateString(),r.U.[0])


let P11=LiveChart.FastLineIncremental(obs14,Name="Ca_Incr",Title="Ca(conc)")
                .WithXAxis(Enabled=false).WithYAxis(Enabled=false).WithTitle().WithMarkers(Step=1)
let P12=LiveChart.FastLineIncremental(obs15,Name="Temp_Incr",Title="Temp(degK)")
                .WithXAxis(Enabled=false).WithYAxis(Enabled=false).WithTitle().WithMarkers(Step=1)
let P13=LiveChart.FastLineIncremental(obs16,Name="F_Incr",Title="F(l/h)")
                .WithXAxis(Enabled=false).WithYAxis(Enabled=false).WithTitle().WithMarkers(Step=1)

//Scroll
let mutable TS11 = 
  [ for x in 0 .. 3*60-1 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),0.0 ]
let mutable TS12 = 
  [ for x in 0 .. 3*60-1 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),0.0 ]
let mutable TS13 = 
  [ for x in 0 .. 3*60-1 -> 
      DateTime.Now.AddSeconds(float x).ToShortDateString(),0.0 ]

let obs17 = obs13
           |> Observable.map (fun r->
                                TS11<-TS1.Tail @ [DateTime.Now.ToShortDateString(),r.x.[0]-8.5] 
                                TS11 
                                 ) 
let obs18 = obs13
           |> Observable.map (fun r->
                                TS12<-TS12.Tail @ [DateTime.Now.ToShortDateString(),r.x.[1]-311.0] 
                                TS12 
                                 ) 
let obs19 = obs13
           |> Observable.map (fun r->
                                TS13<-TS13.Tail @ [DateTime.Now.ToShortDateString(),r.U.[0]] 
                                TS13 
                                 ) 

let P14=LiveChart.FastLine(obs17,"Ca_Scroll","Ca(conc)")
let P15=LiveChart.FastLine(obs18,"T_Scroll","Temp(degK)")
let P16=LiveChart.FastLine(obs19,"F_Scroll","F(l/h)")

//Simulation end point--------------------------------------------------------------------------

//During Simulation
//Change U at any time since it is a Mutable Element
od2.U.[0]<-2.0;;


//==============================================================================================
//Under Construction
let scrollObs n init obsIn func=
    let mutable TS = 
      [ for x in 0 .. n*60-1 -> 
          DateTime.Now.AddSeconds(float x).ToShortDateString(),init ]
    let obsOut = obsIn
               |> Observable.map (func) 
    obsOut


