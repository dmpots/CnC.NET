#light
//------------------------------------------------------------------------------
// <autogenerated>
//     This code was generated by a tool.
//     Runtime Version: 2.0.50727.3074
//
//     Changes to this file may cause incorrect behavior and will be lost if 
//     the code is regenerated.
// </autogenerated>
//------------------------------------------------------------------------------

namespace FSCholeskyGraph
    #nowarn "49" // uppercase argument names
    #nowarn "67" // this type test or downcast will always hold
    #nowarn "66" // this upcast is unnecessary - the types are identical
    #nowarn "58" // possible incorrect indentation..
    #nowarn "57" // do not use create_DelegateEvent
    #nowarn "51" // address-of operator can occur in the code
    
    exception ReturnException623ac138c4864cb6aa52c037c36cc750 of obj
    exception ReturnNoneException623ac138c4864cb6aa52c037c36cc750
    
    type
        Ik_compute = interface
            
            abstract Compute : int * CnCRuntime.IInputCollection<int, int> * CnCRuntime.ITagCollection<int> -> CnCRuntime.CnCReturn
        end
    
    and
        IS1_compute = interface
            
            abstract Compute : int * CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]> * CnCRuntime.IInputCollection<int, int> -> CnCRuntime.CnCReturn
        end
    
    and
        Ikj_compute = interface
            
            abstract Compute : int * CnCRuntime.IInputCollection<int, int> * CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> -> CnCRuntime.CnCReturn
        end
    
    and
        IS2_compute = interface
            
            abstract Compute : Microsoft.FSharp.Core.Tuple<int, int> * CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]> * CnCRuntime.IInputCollection<int, int> -> CnCRuntime.CnCReturn
        end
    
    and
        Ikji_compute = interface
            
            abstract Compute : Microsoft.FSharp.Core.Tuple<int, int> * CnCRuntime.IInputCollection<int, int> * CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> -> CnCRuntime.CnCReturn
        end
    
    and
        IS3_compute = interface
            
            abstract Compute : Microsoft.FSharp.Core.Tuple<int, int, int> * CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]> * CnCRuntime.IInputCollection<int, int> -> CnCRuntime.CnCReturn
        end
    
    and
        IFSCholeskyGraph = interface
            inherit CnCRuntime.Graph
            abstract Lkji : CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]> with get
            
            abstract p : CnCRuntime.IItemCollection<int, int> with get
            
            abstract b : CnCRuntime.IItemCollection<int, int> with get
            
            abstract singleton : CnCRuntime.ITagCollection<int> with get
            
            abstract control_S1 : CnCRuntime.ITagCollection<int> with get
            
            abstract control_S2 : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> with get
            
            abstract control_S3 : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> with get
            
        end
    
    and
        
        singleton = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_k_compute:Ik_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IFSCholeskyGraph
            new(graph:IFSCholeskyGraph, k_compute:Ik_compute) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_k_compute <- k_compute
            abstract Put : int -> unit
            default this.Put  (tag:int) =
                let mutable tag = tag
                System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: singleton PutTag {0}", tag)) |> ignore
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_k_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S1))) |> ignore
            interface CnCRuntime.ITagCollection<int> with
                member this.Put  (tag:int) =
                    let mutable tag = tag
                    System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: singleton PutTag {0}", tag)) |> ignore
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_k_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S1))) |> ignore
            end
        end
    
    and
        
        control_S1 = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_S1_compute:IS1_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_kj_compute:Ikj_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IFSCholeskyGraph
            new(graph:IFSCholeskyGraph, S1_compute:IS1_compute, kj_compute:Ikj_compute) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_S1_compute <- S1_compute
                    this.m_kj_compute <- kj_compute
            abstract Put : int -> unit
            default this.Put  (tag:int) =
                let mutable tag = tag
                System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S1 PutTag {0}", tag)) |> ignore
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S1_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_kj_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S2))) |> ignore
            interface CnCRuntime.ITagCollection<int> with
                member this.Put  (tag:int) =
                    let mutable tag = tag
                    System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S1 PutTag {0}", tag)) |> ignore
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S1_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_kj_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S2))) |> ignore
            end
        end
    
    and
        
        control_S2 = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_S2_compute:IS2_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_kji_compute:Ikji_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IFSCholeskyGraph
            new(graph:IFSCholeskyGraph, S2_compute:IS2_compute, kji_compute:Ikji_compute) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_S2_compute <- S2_compute
                    this.m_kji_compute <- kji_compute
            abstract Put : Microsoft.FSharp.Core.Tuple<int, int> -> unit
            default this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int>) =
                let mutable tag = tag
                System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S2 PutTag {0}", tag)) |> ignore
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S2_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_kji_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S3))) |> ignore
            interface CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> with
                member this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int>) =
                    let mutable tag = tag
                    System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S2 PutTag {0}", tag)) |> ignore
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S2_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_kji_compute.Compute(tag, this.m_graph.p, this.m_graph.control_S3))) |> ignore
            end
        end
    
    and
        
        control_S3 = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_S3_compute:IS3_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_graph:IFSCholeskyGraph
            new(graph:IFSCholeskyGraph, S3_compute:IS3_compute) as this =
                {
                } then
                    this.m_graph <- graph
                    this.m_S3_compute <- S3_compute
            abstract Put : Microsoft.FSharp.Core.Tuple<int, int, int> -> unit
            default this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int, int>) =
                let mutable tag = tag
                System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S3 PutTag {0}", tag)) |> ignore
                let tag = tag
                CnCRuntime.PutTag(this.m_graph, 
                                                                                                                    System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S3_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
            interface CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> with
                member this.Put  (tag:Microsoft.FSharp.Core.Tuple<int, int, int>) =
                    let mutable tag = tag
                    System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, System.String.Format("PT: control_S3 PutTag {0}", tag)) |> ignore
                    let tag = tag
                    CnCRuntime.PutTag(this.m_graph, 
                                                                                                                        System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> this.m_S3_compute.Compute(tag, this.m_graph.Lkji, this.m_graph.b))) |> ignore
            end
        end
    
    and
        
        FSCholeskyGraph = class
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_ic:System.Threading.CountdownEvent
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_Lkji:CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_p:CnCRuntime.IItemCollection<int, int>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_itemC_b:CnCRuntime.IItemCollection<int, int>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_singleton:CnCRuntime.ITagCollection<int>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_control_S1:CnCRuntime.ITagCollection<int>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_control_S2:CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_tagC_control_S3:CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>>
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_k_compute:Ik_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_S1_compute:IS1_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_kj_compute:Ikj_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_S2_compute:IS2_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_kji_compute:Ikji_compute
            
            [<Microsoft.FSharp.Core.DefaultValueAttribute(false)>]
            val mutable m_stepC_S3_compute:IS3_compute
            new() as this =
                {
                }
            interface IFSCholeskyGraph with
            
                member self.Lkji with get() = self.m_itemC_Lkji
                
                member self.p with get() = self.m_itemC_p
                
                member self.b with get() = self.m_itemC_b
                
                member self.singleton with get() = self.m_tagC_singleton
                
                member self.control_S1 with get() = self.m_tagC_control_S1
                
                member self.control_S2 with get() = self.m_tagC_control_S2
                
                member self.control_S3 with get() = self.m_tagC_control_S3
                
                member self.outstandingTasks with get() = self.m_ic
                
            end
            static member Create  (k_compute:Ik_compute, S1_compute:IS1_compute, kj_compute:Ikj_compute, S2_compute:IS2_compute, kji_compute:Ikji_compute, S3_compute:IS3_compute) =
                let mutable k_compute = k_compute
                let mutable S1_compute = S1_compute
                let mutable kj_compute = kj_compute
                let mutable S2_compute = S2_compute
                let mutable kji_compute = kji_compute
                let mutable S3_compute = S3_compute
                let mutable (g:FSCholeskyGraph) = new FSCholeskyGraph()
                g.m_stepC_k_compute <- k_compute
                g.m_stepC_S1_compute <- S1_compute
                g.m_stepC_kj_compute <- kj_compute
                g.m_stepC_S2_compute <- S2_compute
                g.m_stepC_kji_compute <- kji_compute
                g.m_stepC_S3_compute <- S3_compute
                g.m_itemC_Lkji <- new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, float[,]>("Lkji")
                g.m_itemC_p <- new CnCRuntime.ItemCollection<int, int>("p")
                g.m_itemC_b <- new CnCRuntime.ItemCollection<int, int>("b")
                g.m_tagC_singleton <- new singleton(g, k_compute)
                g.m_tagC_control_S1 <- new control_S1(g, S1_compute, kj_compute)
                g.m_tagC_control_S2 <- new control_S2(g, S2_compute, kji_compute)
                g.m_tagC_control_S3 <- new control_S3(g, S3_compute)
                g.m_ic <- new System.Threading.CountdownEvent(1)
                ((g :> obj) :?> IFSCholeskyGraph)
        end