namespace FSRxUtil
//RX
#I @"C:\Project(Comp)\Dev_2015\AdaptCtrl_1\ODE\FSODE\lib"
#r @"System.Reactive.Core.dll"
#r @"System.Reactive.Interfaces.dll"
#r @"System.Reactive.Linq.dll"
#r @"System.Reactive.PlatformServices.dll"
#r @"System.Reactive.Providers.dll"
#r @"Microsoft.Reactive.Testing.dll"
#r @"FSharp.Control.Reactive.dll" //Need to be the last reference

//System
open System
open System.Collections.Generic
open System.Threading
//open System.Net

//Rx
open System.Reactive.Linq
//open System.Reactive
//open System.Reactive.Disposables
open System.Reactive.Concurrency
open System.Reactive.Subjects
//open Microsoft.Reactive.Testing

//FS Rx
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Builders
open FSharp.Control.Reactive.Observable


module Dump=
    let name=String.Empty
    let PrintThread (location:string)=
            printfn "Loc:%s Thread:%d" location Thread.CurrentThread.ManagedThreadId
    let f1=fun (i:'T)->PrintThread "OnNext"
                       printfn "%s-->%A" name i
    let f2=fun (ex:Exception)->printfn "%s-->%A" name ex.Message
    let f3=fun ()-> PrintThread "OnCompleted"
                    printfn "%s completed" name
    let Dump1<'T>  (name:string) (source:IObservable<'T>) =
        Observable.subscribeWithCallbacks f1 f2 f3 source              
    let Dump2<'T>  (name:string) (source:IObservable<'T>) =
        source |> (Observable.subscribeOn Scheduler.Default) 
               |>  Observable.subscribeWithCallbacks f1 f2 f3              
    let Dump3<'T>  (name:string) (source:IObservable<'T>) =
        source |> (Observable.subscribeOn Scheduler.Default)   
               |> (Observable.observeOn Scheduler.Default) 
               |>  Observable.subscribeWithCallbacks f1 f2 f3              

