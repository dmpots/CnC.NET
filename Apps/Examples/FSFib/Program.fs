#light




let Fib = {new FibGraph.Ifib with 
    member self.Compute(n, fib, fibSum) =
        if n > 1 then
            fib.Put(n-1)
            fib.Put(n-2)
            fibSum.Put(n)        
        
        CnCRuntime.CnCReturn.Ok 
        
}

let FibSum = {new FibGraph.IfibSum with
    member self.Compute(n, fibs) =
        let x = fibs.Get(n-1)
        let y = fibs.Get(n-2)
        fibs.Put(n, x + y)       
        
        CnCRuntime.CnCReturn.Ok 
}


let Main() =
    let n = 22
    //CnCRuntime.Trace.Enabled <- true
    //System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener()) |> ignore
    let g = FibGraph.FibGraph.Create(Fib, FibSum)        
    g.fibs.Put(0,0)
    g.fibs.Put(1,1)
    g.fib.Put(n)
    
    
    CnCRuntime.Run(g)
    printfn "Fib(%d) = %d" n (g.fibs.Get(n))
    
    
Main()    

