#light

#light

(* 
 * Interface
 *)
type IInputCollection<'tag,'item> =
    abstract Get : 'tag -> 'item
    
type IOutputCollection<'tag,'item> =
    abstract Put : 'tag * 'item -> unit

type IItemCollection<'tag, 'item> =
    inherit IInputCollection<'tag, 'item>
    inherit IOutputCollection<'tag, 'item>

type ITagCollection<'tag> =
    abstract Put : 'tag -> unit 
    
type Graph = 
    abstract outstandingTasks : System.Threading.CountdownEvent
    

type CnCReturn = | Ok = 0  | Error = 1

let taskManager = ref (new System.Threading.Tasks.TaskManager())
let SetNumberOfThreads(nThreads) =  
    //printfn "Current %d" (!taskManager).Policy.IdealProcessors 
    let tm = new System.Threading.Tasks.TaskManager(System.Threading.Tasks.TaskManagerPolicy(1,nThreads))   
    //printfn "New %d" tm.Policy.IdealProcessors 
    let oldTm = !taskManager
    taskManager :=  tm
    //printfn "Disposing..."
    oldTm.Dispose()
    //printfn "Disposed"

let Trace = System.Diagnostics.BooleanSwitch("CnCRuntimeTrace", "Trace of the CnCRutnime")
//let trace = System.Diagnostics.BooleanSwitch()
Trace.Enabled <- false
    
(* 
 * Implementaion 
 *)
let readLock (lock : System.Threading.ReaderWriterLock) f = 
    lock.AcquireReaderLock(System.Threading.Timeout.Infinite)
    try
        f()
    finally
        lock.ReleaseReaderLock()

let writeLock (lock : System.Threading.ReaderWriterLock) f = 
    lock.AcquireWriterLock(System.Threading.Timeout.Infinite)
    try
        f()
        System.Threading.Thread.MemoryBarrier()
    finally
        lock.ReleaseWriterLock()  

type ItemCollection<'tag,'item>(name:string) =    
    let dict = System.Collections.Generic.Dictionary<'tag,'item>()
    let rwlock = System.Threading.ReaderWriterLock()
    let waitLock = System.Threading.SpinLock(true)
    let waitDict = System.Collections.Generic.Dictionary<'tag, System.Threading.ManualResetEventSlim>()    
    
    interface IItemCollection<'tag, 'item> with
        member self.Get(tag) =  
            System.Diagnostics.Debug.WriteLineIf(Trace.Enabled, sprintf "G: %s GetItem(%A)" name tag)                 
            let v = readLock rwlock (fun () -> 
                if dict.ContainsKey(tag) then
                    Some (dict.[tag])
                else
                    waitLock.Enter()
                    let waitObj = 
                        if not(waitDict.ContainsKey(tag)) then
                            waitDict.[tag] <- new System.Threading.ManualResetEventSlim()
                    waitLock.Exit()
                    None                
            )
            match v with
            | Some x -> x
            | None   -> waitDict.[tag].Wait(); (self :> IItemCollection<_,_>).Get(tag)
        
        member self.Put(tag, item) = writeLock rwlock (fun () ->        
            System.Diagnostics.Debug.WriteLineIf(Trace.Enabled, sprintf "P: %s PutItem(%A,%A)" name tag item)                 
            if not(dict.ContainsKey(tag)) || (dict.[tag] <> item) then
                dict.Add(tag,item)
            waitLock.Enter()
            if (waitDict.ContainsKey(tag)) then waitDict.[tag].Set()
            waitLock.Exit()
        )

type PutTagDelegate = System.Func<unit,CnCReturn>

let PutTag(g : Graph, a : System.Func<unit,CnCReturn>) =
    g.outstandingTasks.Increment()
    System.Threading.Tasks.Task.Create((fun _ ->                        
        try
            a.Invoke()  |> ignore          
        finally
            g.outstandingTasks.Decrement() |> ignore                    
    ), !taskManager, System.Threading.Tasks.TaskCreationOptions.None) |> ignore

let Run(g : Graph) = 
    g.outstandingTasks.Decrement() |> ignore
    g.outstandingTasks.Wait()
    
(* convienience functions *)
let Tuple2(x,y) = {Item1=x; Item2=y}
let Tuple3(x,y,z):Microsoft.FSharp.Core.Tuple<'a,'b,'c> = {Item1=x; Item2=y; Item3=z;}
let Tuple4(x,y,z,a):Microsoft.FSharp.Core.Tuple<'a,'b,'c,'d>  = {Item1=x; Item2=y; Item3=z; Item4=a}
let Tuple5(x,y,z,a,b):Microsoft.FSharp.Core.Tuple<'a,'b,'c,'d,'e>  = {Item1=x; Item2=y; Item3=z; Item4=a; Item5=b}

let T2 = Tuple2
let T3 = Tuple3
let T4 = Tuple4
let T5 = Tuple5