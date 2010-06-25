#light
//------------------------------------------------------------------------------
// <autogenerated>
//     This code was generated by a tool.
//     Runtime Version: 2.0.50727.1433
//
//     Changes to this file may cause incorrect behavior and will be lost if 
//     the code is regenerated.
// </autogenerated>
//------------------------------------------------------------------------------

namespace MMGraph
    #nowarn "49" // uppercase argument names
    #nowarn "67" // this type test or downcast will always hold
    #nowarn "66" // this upcast is unnecessary - the types are identical
    #nowarn "58" // possible incorrect indentation..
    #nowarn "57" // do not use create_DelegateEvent
    #nowarn "51" // address-of operator can occur in the code
    
    exception ReturnExceptiona647465194924594b70e407e67503810 of obj
    exception ReturnNoneExceptiona647465194924594b70e407e67503810
    
    type
        Iinit = interface
            
            abstract Compute : Microsoft.FSharp.Core.Tuple<int, int> * CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float> * CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> -> CnCRuntime.CnCReturn
        end
    
    and
        Icompute = interface
            
            abstract Compute : Microsoft.FSharp.Core.Tuple<int, int, int> * CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float> * CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> * CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> * CnCRuntime.IInputCollection<int, int> * CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> * CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> -> CnCRuntime.CnCReturn
        end
    
    and
        IMMGraph = interface
            inherit CnCRuntime.Graph
            abstract A : CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> with get
            
            abstract B : CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> with get
            
            abstract C : CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float> with get
            
            abstract scans : CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float> with get
            
            abstract N : CnCRuntime.IItemCollection<int, int> with get
            
            abstract initTags : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> with get
            
            abstract computeTags : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> with get
            
        end
    
    and
        
        initTags = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_init:Iinit
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IMMGraph
            new(graph:IMMGraph, init:Iinit) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_init <- init
            abstract Put : Microsoft.FSharp.Core.Tuple<int, int> -> unit
            default this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int>) =
                let mutable tag = tag
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_init.Compute(tag, this.m_graph.scans, this.m_graph.computeTags))) |> ignore
            interface CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> with
                member this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int>) =
                    let mutable tag = tag
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_init.Compute(tag, this.m_graph.scans, this.m_graph.computeTags))) |> ignore
            end
        end
    
    and
        
        computeTags = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_compute:Icompute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IMMGraph
            new(graph:IMMGraph, compute:Icompute) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_compute <- compute
            abstract Put : Microsoft.FSharp.Core.Tuple<int, int, int> -> unit
            default this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int, int>) =
                let mutable tag = tag
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_compute.Compute(tag, this.m_graph.scans, this.m_graph.A, this.m_graph.B, this.m_graph.N, this.m_graph.C, this.m_graph.computeTags))) |> ignore
            interface CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> with
                member this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int, int>) =
                    let mutable tag = tag
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_compute.Compute(tag, this.m_graph.scans, this.m_graph.A, this.m_graph.B, this.m_graph.N, this.m_graph.C, this.m_graph.computeTags))) |> ignore
            end
        end
    
    and
        
        MMGraph = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_ic:System.Threading.CountdownEvent
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_A:CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_B:CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_C:CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_scans:CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_N:CnCRuntime.IItemCollection<int, int>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_initTags:CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_computeTags:CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_init:Iinit
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_compute:Icompute
            new() as this =
                {
                }
            interface IMMGraph with
            
                member self.A with get() = self.m_itemC_A
            
                member self.B with get() = self.m_itemC_B
            
                member self.C with get() = self.m_itemC_C
            
                member self.scans with get() = self.m_itemC_scans
            
                member self.N with get() = self.m_itemC_N
            
                member self.initTags with get() = self.m_tagC_initTags
            
                member self.computeTags with get() = self.m_tagC_computeTags
            
                member self.outstandingTasks with get() = self.m_ic
            
            end
            static member Create  (init:Iinit, compute:Icompute) =
                let mutable init = init
                let mutable compute = compute
                let mutable (g:MMGraph) = new MMGraph()
                g.m_stepC_init <- init
                g.m_stepC_compute <- compute
                g.m_itemC_A <- new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>("")
                g.m_itemC_B <- new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>("")
                g.m_itemC_C <- new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, float>("")
                g.m_itemC_scans <- new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float>("")
                g.m_itemC_N <- new CnCRuntime.ItemCollection<int, int>("")
                g.m_tagC_initTags <- new initTags(g, init)
                g.m_tagC_computeTags <- new computeTags(g, compute)
                g.m_ic <- new System.Threading.CountdownEvent(1)
                ((g :> obj) :?> IMMGraph)
        end