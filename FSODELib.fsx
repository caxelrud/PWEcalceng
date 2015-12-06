namespace FSODELib

module ODE1=
    let rk4 (f:float*float array*float array->float array) (t:float) (h:float) (x:float array) (U:float array) =
        /// f: t,y[],U[]
        let n=x.GetLength(0)
        let mutable (xt:float array)=Array.zeroCreate n
        let mutable (k1:float array)=Array.zeroCreate n
        let mutable (k2:float array)=Array.zeroCreate n
        let mutable (k3:float array)=Array.zeroCreate n
        let mutable (k4:float array)=Array.zeroCreate n
        let (xout:float array)=Array.zeroCreate n
        k1<-f(t,x,U) //----------------------->k1 = f init
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+0.5*h*k1.[i]    
        k2<-f(0.5*h+t,xt,U) //--------------->k2 = f (0.5*h+t_, y_+0.5*h*k1)
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+0.5*h*k2.[i]
        k3<-f(0.5*h+t,xt,U) //--------------->k3 = f (0.5*h+t_, y_+0.5*h*k2)
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+h*k3.[i] 
            //dxm.[i]<-dxm.[i]+dxt.[i]
        k4<-f(t+h,xt,U)     //------------------->k4 = f (    h+t_, y_+h*k3)
        for i in 0..(n-1) do
            xout.[i]<-x.[i]+h*(k1.[i]+2.0*k2.[i]+2.0*k3.[i]+k4.[i])/6.0 //t,y = t_+h, y_ + h*(k1+2.0*k2+2.0*k3+k4)/6.0
            //printfn "x[%d]=%f" i xout.[i]
        (t+h,xout)

    let rk4Multi (f:float*float array*float array->float array) (h:float) (t:float) (dt:float) (x:float array) (U:float array) =
        let mutable t_=t
        let mutable h_=h
        let mutable x_=x
        let mutable (r:float*float array)=0.0,null
        if dt>h then
            let N=int (dt/h)
            for k in 0..(N-1) do
                r<-rk4 f t_ h_ x_ U
                t_<-fst r; x_<-snd r
            h_<-dt-(float N)*h_
            if h_ <> 0.0 then
                r<- rk4 f t_ h_ x_ U
                t_<-fst r; x_<-snd r
        else
            h_<-dt
            r<- rk4 f t_ h_ x_ U
            t_<-fst r; x_<-snd r
        (t+dt,x_)

(*
    let rk4 (f:float*float array*float array->float array) (t:float) (h:float) (x:float array) (U:float array) =
        /// f: t,y[],U[]
        let n=x.GetLength(0)
        let mutable (dx:float array)=Array.zeroCreate n
        let mutable (xt:float array)=Array.zeroCreate n
        let mutable (dxt:float array)=Array.zeroCreate n
        let mutable (dxm:float array)=Array.zeroCreate n
        let (xout:float array)=Array.zeroCreate n
        dx<-f(t,x,U) //----------------------->k1 = f init
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+(h/2.0)*dx.[i]    
        dxt<-f(t+h/2.0,xt,U) //--------------->k2 = f (0.5*h+t_, y_+0.5*h*k1)
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+(h/2.0)*dxt.[i]
        dxm<-f(t+h/2.0,xt,U) //--------------->k3 = f (0.5*h+t_, y_+0.5*h*k2)
        for i in 0..(n-1) do
            xt.[i]<-x.[i]+h*dxm.[i] 
            dxm.[i]<-dxm.[i]+dxt.[i]
        dxt<-f(t+h,xt,U) //------------------->k4 = f (    h+t_, y_+h*k3)
        for i in 0..(n-1) do
            xout.[i]<-x.[i]+(h/6.0)*(dx.[i]+dxt.[i]+2.0*dxm.[i]) //t,y = t_+h, y_ + h*(k1+2.0*k2+2.0*k3+k4)/6.0
        xout

module ODE1=
    let SolveEuler (f:float*float->float) (h:float) (init:float*float) = 
        let t0, _ = init
        let t_, y_ = fst init, snd init
        (t_+h, y_ + h*(f init)) 
        
    let SolveRungeKutta (f:float*float->float) (h:float) (init:float*float) = 
        //f:(time,y)->yk+1
        let t0, _ = init
        let t_, y_ = fst init, snd init                    
        let k1 = f init
        let k2 = f (0.5*h+t_, y_+0.5*h*k1)
        let k3 = f (0.5*h+t_, y_+0.5*h*k2)
        let k4 = f (    h+t_, y_+h*k3)
        let t,y = t_+h, y_ + h*(k1+2.0*k2+2.0*k3+k4)/6.0
        (t, y)


*)
