#light


let K_Compute = {new FSCholeskyGraph.Ik_compute with
    member self.Compute(tag, pC, control_S1) =     
        let p = pC.Get(0)
        [0 .. (p-1)] |> List.iter (fun k -> control_S1.Put(k))
        CnCRuntime.CnCReturn.Ok
}

let KJ_Compute = {new FSCholeskyGraph.Ikj_compute with
    member self.Compute(tag, pC, control_S2) =     
        let p = pC.Get(0)
        let k = tag
        [(k+1) .. (p-1)] |> List.iter (fun j -> control_S2.Put(CnCRuntime.T2(k,j)))
        CnCRuntime.CnCReturn.Ok
}

let KJI_Compute = {new FSCholeskyGraph.Ikji_compute with
    member self.Compute(tag, pC, control_S3) =     
        let p = pC.Get(0)
        let k,j = tag.Item1, tag.Item2
        [(k+1) .. (j)] |> List.iter (fun i -> control_S3.Put(CnCRuntime.T3(k,j,i)))
        CnCRuntime.CnCReturn.Ok
}




let S1_Compute = { new FSCholeskyGraph.IS1_compute with
    member self.Compute(tag, cLkji, bC) =  
        System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, sprintf "S1_Compute %A executing" tag)       
        let b = bC.Get(0)
        let k = tag
        
        let A_block = cLkji.Get(CnCRuntime.T3(k,k,k))
        let L_block = Array2.create b b 0.0
        
        
        let _ = 
            for k_b = 0  to (b-1) do
                if A_block.[k_b,k_b] <= 0.0 then
                    fprintfn stderr "Not a symmetric positive definite (SPD) matrix"
                    exit 1
                L_block.[k_b,k_b] <- sqrt(A_block.[k_b,k_b])
                for j_b = k_b+1 to (b-1) do
                    L_block.[j_b,k_b] <- A_block.[j_b,k_b] / L_block.[k_b,k_b]
      
                for j_bb = k_b+1 to (b-1) do
                for i_b  = j_bb  to (b-1) do
                    A_block.[i_b,j_bb] <- A_block.[i_b,j_bb] - (L_block.[i_b,k_b] * L_block.[j_bb,k_b])
                  
             
        cLkji.Put(CnCRuntime.T3(k,k,k+1), L_block)       
        CnCRuntime.CnCReturn.Ok
}

let S2_Compute = { new FSCholeskyGraph.IS2_compute with
    member self.Compute(tag, cLkji, bC) =
        System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, sprintf "S2_Compute %A executing" tag)
        let b = bC.Get(0)
        let k,j = tag.Item1, tag.Item2
        let A_block = cLkji.Get(CnCRuntime.T3(j,k,k))
        let Li_block = cLkji.Get(CnCRuntime.T3(k,k,k+1))
     
        let Lo_block = Array2.create b b 0.0
        
        let _ =
            for k_b = 0 to (b-1) do
                for i_b = 0 to (b-1) do
                    Lo_block.[i_b,k_b] <-  A_block.[i_b,k_b]/Li_block.[k_b,k_b]
                for j_b = k_b+1 to (b-1) do
                for i_b = 0 to (b-1) do
                   A_block.[i_b,j_b] <- A_block.[i_b,j_b] - (Li_block.[j_b,k_b] * Lo_block.[i_b,k_b])    
 
        cLkji.Put(CnCRuntime.T3(j,k,k+1), Lo_block)
        CnCRuntime.CnCReturn.Ok
}


let S3_Compute = { new FSCholeskyGraph.IS3_compute with
    member self.Compute(tag, cLkji, bC) =
        System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, sprintf "S3_Compute %A executing" tag)       
        let b = bC.Get(0)
        let k,j,i = tag.Item1, tag.Item2, tag.Item3        
        let A_block =cLkji.Get(CnCRuntime.T3(j,i,k))
        
        let L1_block,L2_block = 
            if(i = j) then   // Diagonal tile.
                let L2_block =cLkji.Get(CnCRuntime.T3(j,k,k+1)) // In case of a diagonal tile, i=j, hence both the tiles are the same.
                L2_block,L2_block           
            else   // Non-diagonal tile.
                let L2_block = cLkji.Get(CnCRuntime.T3(i,k,k+1)); // Get the first tile.
                let L1_block = cLkji.Get(CnCRuntime.T3(j,k,k+1)); // Get the second tile.
                L1_block, L2_block    
        
        let _ =
            for j_b = 0 to (b-1) do
            for k_b = 0 to (b-1) do
                let temp = -1.0 * L2_block.[j_b,k_b]
                if i <> j then
                    for i_b = 0 to (b-1) do
                        A_block.[i_b,j_b] <- A_block.[i_b,j_b] + (temp * L1_block.[i_b,k_b])
                else
                    for i_b = j_b to (b-1) do
                        A_block.[i_b,j_b] <- A_block.[i_b,j_b] + (temp * L2_block.[i_b,k_b])
            
       
        cLkji.Put(CnCRuntime.T3(j,i,k+1),A_block) // Write the output at the next time step         
        CnCRuntime.CnCReturn.Ok
}



let Cholesky (A : double[,]) (config : MatrixIO.Configuration) =        
    CnCRuntime.SetNumberOfThreads(config.numberOfThreads)  
    CnCRuntime.Trace.Enabled <- false
    System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener()) |> ignore
    
      
    let p = config.matrixSize / config.blockSize
    let partitions = MatrixIO.splitMatrix A config        
    let graph = FSCholeskyGraph.FSCholeskyGraph.Create(K_Compute, S1_Compute, KJ_Compute, S2_Compute, KJI_Compute, S3_Compute)   
   
    graph.p.Put(0,p)
    graph.b.Put(0,config.blockSize)    
    partitions |> Seq.iter (fun ((i,j),T) -> graph.Lkji.Put(CnCRuntime.T3(i,j,0), T))
    
    // Start Execution
    printfn "Starting Execution "
    let start = System.DateTime.Now
    graph.singleton.Put(1)
    CnCRuntime.Run(graph)
    let stop = System.DateTime.Now
    printfn "Finisehd Execution "
    printfn "The time taken for parallel(%d threads) execution a matrix of size %d x %d : %A " (config.numberOfThreads) (config.matrixSize) (config.matrixSize) (stop-start)
    
    
    
    // End Exection
    if config.verbose then
        MatrixIO.writeMatrixFromCollection config (graph.Lkji)
    
    
    ()