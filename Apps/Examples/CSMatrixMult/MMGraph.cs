//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.3074
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace MMGraph {
    
    
    public interface Iinit {
        
        CnCRuntime.CnCReturn Compute(Microsoft.FSharp.Core.Tuple<int, int> tag, CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans, CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags);
    }
    
    public interface Icompute {
        
        CnCRuntime.CnCReturn Compute(Microsoft.FSharp.Core.Tuple<int, int, int> tag, CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans, CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> A, CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> B, CnCRuntime.IInputCollection<int, int> N, CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> C, CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags);
    }
    
    public interface IMMGraph : CnCRuntime.Graph {
        
        CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> A {
            get;
        }
        
        CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> B {
            get;
        }
        
        CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> C {
            get;
        }
        
        CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans {
            get;
        }
        
        CnCRuntime.IItemCollection<int, int> N {
            get;
        }
        
        CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> initTags {
            get;
        }
        
        CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags {
            get;
        }
    }
    
    public class initTags : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> {
        
        private Iinit m_init;
        
        private IMMGraph m_graph;
        
        public initTags(IMMGraph graph, Iinit init) {
            m_graph = graph;
            m_init = init;
        }
        
        public virtual void Put(Microsoft.FSharp.Core.Tuple<int, int> tag) {
            System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, string.Format("PT: initTags PutTag {0}", tag));
            CnCRuntime.PutTag(m_graph, (unit => this.m_init.Compute(tag, this.m_graph.scans, this.m_graph.computeTags)));
        }
    }
    
    public class computeTags : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> {
        
        private Icompute m_compute;
        
        private IMMGraph m_graph;
        
        public computeTags(IMMGraph graph, Icompute compute) {
            m_graph = graph;
            m_compute = compute;
        }
        
        public virtual void Put(Microsoft.FSharp.Core.Tuple<int, int, int> tag) {
            System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, string.Format("PT: computeTags PutTag {0}", tag));
            CnCRuntime.PutTag(m_graph, (unit => this.m_compute.Compute(tag, this.m_graph.scans, this.m_graph.A, this.m_graph.B, this.m_graph.N, this.m_graph.C, this.m_graph.computeTags)));
        }
    }
    
    public class MMGraph : IMMGraph {
        
        private System.Threading.CountdownEvent m_ic;
        
        private CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> m_itemC_A;
        
        private CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> m_itemC_B;
        
        private CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> m_itemC_C;
        
        private CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> m_itemC_scans;
        
        private CnCRuntime.IItemCollection<int, int> m_itemC_N;
        
        private CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> m_tagC_initTags;
        
        private CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> m_tagC_computeTags;
        
        private Iinit m_stepC_init;
        
        private Icompute m_stepC_compute;
        
        public virtual CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> A {
            get {
                return this.m_itemC_A;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> B {
            get {
                return this.m_itemC_B;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> C {
            get {
                return this.m_itemC_C;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans {
            get {
                return this.m_itemC_scans;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<int, int> N {
            get {
                return this.m_itemC_N;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> initTags {
            get {
                return this.m_tagC_initTags;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags {
            get {
                return this.m_tagC_computeTags;
            }
        }
        
        public virtual System.Threading.CountdownEvent outstandingTasks {
            get {
                return this.m_ic;
            }
        }
        
        public static IMMGraph Create(Iinit init, Icompute compute) {
            MMGraph g = new MMGraph();
            g.m_stepC_init = init;
            g.m_stepC_compute = compute;
            g.m_itemC_A = new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double>("A");
            g.m_itemC_B = new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double>("B");
            g.m_itemC_C = new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int>, double>("C");
            g.m_itemC_scans = new CnCRuntime.ItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double>("scans");
            g.m_itemC_N = new CnCRuntime.ItemCollection<int, int>("N");
            g.m_tagC_initTags = new initTags(g, init);
            g.m_tagC_computeTags = new computeTags(g, compute);
            g.m_ic = new System.Threading.CountdownEvent(1);
            return g;
        }
    }
}
