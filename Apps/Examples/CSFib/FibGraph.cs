//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.3074
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace FibGraph {
    
    
    public interface Ifib {
        
        CnCRuntime.CnCReturn Compute(int tag, CnCRuntime.ITagCollection<int> fib, CnCRuntime.ITagCollection<int> fibSum);
    }
    
    public interface IfibSum {
        
        CnCRuntime.CnCReturn Compute(int tag, CnCRuntime.IItemCollection<int, int> fibs);
    }
    
    public interface IFibGraph : CnCRuntime.Graph {
        
        CnCRuntime.IItemCollection<int, int> fibs {
            get;
        }
        
        CnCRuntime.ITagCollection<int> fib {
            get;
        }
        
        CnCRuntime.ITagCollection<int> fibSum {
            get;
        }
    }
    
    public class fib : CnCRuntime.ITagCollection<int> {
        
        private Ifib m_fib;
        
        private IFibGraph m_graph;
        
        public fib(IFibGraph graph, Ifib fib) {
            m_graph = graph;
            m_fib = fib;
        }
        
        public virtual void Put(int tag) {
            System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, string.Format("PT: fib PutTag {0}", tag));
            CnCRuntime.PutTag(m_graph, (unit => this.m_fib.Compute(tag, this.m_graph.fib, this.m_graph.fibSum)));
        }
    }
    
    public class fibSum : CnCRuntime.ITagCollection<int> {
        
        private IfibSum m_fibSum;
        
        private IFibGraph m_graph;
        
        public fibSum(IFibGraph graph, IfibSum fibSum) {
            m_graph = graph;
            m_fibSum = fibSum;
        }
        
        public virtual void Put(int tag) {
            System.Diagnostics.Debug.WriteLineIf(CnCRuntime.Trace.Enabled, string.Format("PT: fibSum PutTag {0}", tag));
            CnCRuntime.PutTag(m_graph, (unit => this.m_fibSum.Compute(tag, this.m_graph.fibs)));
        }
    }
    
    public class FibGraph : IFibGraph {
        
        private System.Threading.CountdownEvent m_ic;
        
        private CnCRuntime.IItemCollection<int, int> m_itemC_fibs;
        
        private CnCRuntime.ITagCollection<int> m_tagC_fib;
        
        private CnCRuntime.ITagCollection<int> m_tagC_fibSum;
        
        private Ifib m_stepC_fib;
        
        private IfibSum m_stepC_fibSum;
        
        public virtual CnCRuntime.IItemCollection<int, int> fibs {
            get {
                return this.m_itemC_fibs;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<int> fib {
            get {
                return this.m_tagC_fib;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<int> fibSum {
            get {
                return this.m_tagC_fibSum;
            }
        }
        
        public virtual System.Threading.CountdownEvent outstandingTasks {
            get {
                return this.m_ic;
            }
        }
        
        public static IFibGraph Create(Ifib fib, IfibSum fibSum) {
            FibGraph g = new FibGraph();
            g.m_stepC_fib = fib;
            g.m_stepC_fibSum = fibSum;
            g.m_itemC_fibs = new CnCRuntime.ItemCollection<int, int>("fibs");
            g.m_tagC_fib = new fib(g, fib);
            g.m_tagC_fibSum = new fibSum(g, fibSum);
            g.m_ic = new System.Threading.CountdownEvent(1);
            return g;
        }
    }
}
