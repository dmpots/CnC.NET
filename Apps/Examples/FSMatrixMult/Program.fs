#light

let nTag = 0

let T2 = CnCRuntime.Tuple2
let T3 = CnCRuntime.Tuple3

let InitStep = {new MMGraph.Iinit with
    member self.Compute(tag, scans, computeTags) = 
        let i,j = tag.Item1, tag.Item2
        //printfn "Init(%d,%d)" i j
        
        let outTag = CnCRuntime.Tuple3(i,j,0)
        scans.Put(outTag, 0.0)
        computeTags.Put(outTag)
        CnCRuntime.CnCReturn.Ok
}

let ComputeStep = { new MMGraph.Icompute with
    member self.Compute(tag, scans, A, B, N, C, computeTags) =
        let i,j,k = tag.Item1, tag.Item2, tag.Item3
        //printfn "Compute(%d,%d,%d)" i j k
        
        let aik = A.Get(CnCRuntime.Tuple2(i,k))
        let bkj = B.Get(CnCRuntime.Tuple2(k,j))
        let cij = scans.Get(tag) + aik * bkj
        let nMax = N.Get(nTag)
        //printfn "Compute(%d,%d,%d) Gets" i j k
        
        if k < (nMax - 1) then
            //printfn "Compute(%d,%d,%d) Scans Put (%d,%d,%d)" i j k i j (k+1)
            scans.Put(CnCRuntime.Tuple3(i,j,k+1), cij)
            computeTags.Put(T3(i,j,k+1))
            
        if(k = (nMax-1)) then
            //printfn "Compute(%d,%d,%d) C Put" i j k
            C.Put(T2(i,j), cij)
        
        //printfn "Compute(%d,%d,%d) Done" i j k    
        CnCRuntime.CnCReturn.Ok
}


let mIter N f =
    for i = 0 to (N-1) do
    for j = 0 to (N-1) do
        f i j


let main() =    
    let N  = try int(Sys.argv.[1]) with _ -> 10
    let Nt = try int(Sys.argv.[2]) with _ -> 8
    CnCRuntime.SetNumberOfThreads(Nt)
    printfn "Graph Run Started"
    
    let graph = MMGraph.MMGraph.Create(InitStep, ComputeStep)
    graph.N.Put(nTag, N)
    mIter N (fun i j -> 
        graph.A.Put(T2(i,j), 1.0) 
        graph.B.Put(T2(i,j), 1.0)
        graph.initTags.Put(T2(i,j))
    )
    
    
    //let tm = new System.Th
    let start = System.DateTime.Now
    
    CnCRuntime.Run(graph)
    let stop = System.DateTime.Now
    printfn "Graph Run Finished %A" (stop - start)
   
    (* 
    mIter N (fun i j ->
        //printfn "C Get (%d,%d)" i j
        printf "%.2f " (graph.C.Get(T2(i,j)))
        if (j = N-1) then printfn ""
    )
    *)
    
    
main()