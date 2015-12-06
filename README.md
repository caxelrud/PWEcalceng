# PWEcalceng - Powerfull&Easy Real-time Calculation Engine and Simulator

Real-time Calculation Engine is a powerful application that performs complex calculations with real-time data. 
The application imports or simulate real-time values.
The *Observable-Observer* (PUSH) computation pattern, based on *RX.NET*, is used, in order to obtain the highest 
possible performance in the .NET environment.

The Calculation Engine is non-blocking and, as much as possible, reactive.
No global variables are used. Side effects are minimized.

I like think the problem as *asynchronous* STREAMS of data (types) that are processed by *reactive* OPERATIONS 
that may result in other STREAMS of data, as in an industrial manufacturing facility.   

## Simulation of ODE 
A *Runge-Kutta* routine, for numeric integration, is included for simulation of Ordinary Differential Equation.

## Charts
Data can be visualized on Chart Observers, using *FSharp.Charting* library. 
Examples with *Incremental* and *Scrolling* charts are included.
 

  