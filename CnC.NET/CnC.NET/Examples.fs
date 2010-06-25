#light
//#nowarn "47" // allow self reference in the Graph constructor
(*
open CnCRuntime        
        
(*
 * Example
    <Tag1: int a, int b> :: (Step1: int a, int b); 
    <Tag2: int x> :: (Step2); 
    <Tag2: int y> :: (Step3); 
    [A: int] -> (Step1) -> [B: int], <Tag 2: int x> 
    [B] -> (Step2) 
    (Step3) -> [C]
 *)

(* generated code *)
type IStep1 =
    abstract Compute : tag:(int*int) * A:IInputCollection<(int), int> * B:IOutputCollection<(int),int> * TagCollection2:ITagCollection<(int)> -> CnCRuntime.CnCReturn

type IStep2 = 
    abstract Compute : tag:(int) * A:IInputCollection<(int),int> -> CnCRuntime.CnCReturn

type IStep3 = 
    abstract Compute : tag:(int) * C:IOutputCollection<(int),int> -> CnCRuntime.CnCReturn

type IGraph =
    inherit CnCRuntime.Graph
    abstract A : IItemCollection<(int),int>
    abstract B : IItemCollection<(int),int>
    abstract C : IItemCollection<(int),int>
    
    abstract Tag1 : ITagCollection<(int * int)>
    abstract Tag2 : ITagCollection<(int)>

type Tag1(graph : IGraph, step1:IStep1) =     
    interface ITagCollection<(int*int)> with
        member self.Put(tag) =
            CnCRuntime.PutTag(graph, System.Func<unit,CnCReturn>(fun () -> step1.Compute(tag, graph.A, graph.B, graph.Tag2)))
    
            
            
type Tag2(graph : IGraph, step2:IStep2, step3:IStep3) =
    interface ITagCollection<(int)> with
        member self.Put(tag) =
            CnCRuntime.PutTag(graph, System.Func<unit,CnCReturn>(fun _ -> step2.Compute(tag, graph.B)))
            CnCRuntime.PutTag(graph, System.Func<unit,CnCReturn>(fun _ -> step3.Compute(tag, graph.C)))

type Graph(step1:IStep1, step2:IStep2, step3:IStep3) =
    let ic = new System.Threading.CountdownEvent(1)
    let itemC_A = (CnCRuntime.ItemCollection<(int),int>() :> IItemCollection<(int),int>)
    let itemC_B = (CnCRuntime.ItemCollection<(int),int>() :> IItemCollection<(int),int>)
    let itemC_C = (CnCRuntime.ItemCollection<(int),int>() :> IItemCollection<(int),int>)    
    [<DefaultValue(false)>]
    val mutable tagC_Tag1 : Tag1    
    [<DefaultValue(false)>]
    val mutable tagC_Tag2 : Tag2

    static member Create(step1:IStep1, step2:IStep2, step3:IStep3) =
        let g = Graph(step1, step2, step3)
        let t1 = Tag1(g, step1)
        let t2 = Tag2(g, step2, step3)
        g.tagC_Tag1 <- t1;
        g.tagC_Tag2 <- t2;
        (g :> IGraph)

    interface IGraph with
        member self.A with get() = itemC_A
        member self.B with get() = itemC_B
        member self.C with get() = itemC_C
        member self.Tag1 with get() = (self.tagC_Tag1 :> ITagCollection<_>)
        member self.Tag2 with get() = (self.tagC_Tag2 :> ITagCollection<_>)
        member self.outstandingTasks = ic

(* user written code *)        
let Step1 = { new IStep1 with
    member self.Compute(tag, A, B, tagCollection2) = 
        printfn "STEP 1(%A)" (tag)
        let t = fst(tag)
        let tt = snd(tag)
        let a = A.Get(t)
        printfn "STEP 1(%A) putting %d --> B,Tag2" tag a
        B.Put(tt, a)
        tagCollection2.Put(a)
        printfn "STEP 1(%A) DONE" tag
        CnCRuntime.CnCReturn.Ok
}

let Step2 = { new IStep2 with
    member self.Compute(tag, B) = 
        printfn "STEP 2(%d)" tag
        let a = B.Get(tag) 
        printfn "STEP 2(%d) getting %d <-- A" tag a
        printfn "STEP 2(%d) DONE" tag
        CnCRuntime.CnCReturn.Ok
}

let Step3 = { new IStep3 with
    member self.Compute(tag, C) = 
        printfn "STEP 3(%d)" tag
        C.Put(tag, 1)
        printfn "STEP 3(%d) putting %d --> C" tag 1
        printfn "STEP 3(%d) DONE" tag
        CnCRuntime.CnCReturn.Ok
        
}

let main() =
    //let g = ( Graph(Step1, Step2, Step3) :> IGraph)    
    let g = Graph.Create(Step1, Step2, Step3)
    g.Tag1.Put((1,1))
    g.A.Put(1, 2)
    g.outstandingTasks.Decrement() |> ignore
    g.outstandingTasks.Wait()
    //g
    
let x = new Microsoft.FSharp.Compiler.CodeDom.FSharpCodeProvider()    
*)