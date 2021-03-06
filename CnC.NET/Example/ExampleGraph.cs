//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.1433
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace ExampleGraph {
    
    
    public interface IStep1 {
        
        CnCRuntime.CnCReturn Compute(Microsoft.FSharp.Core.Tuple<int, int> tag, CnCRuntime.IInputCollection<int, int[,]> A, CnCRuntime.IOutputCollection<int, int> B, CnCRuntime.ITagCollection<int> Tag2);
    }
    
    public interface IStep2 {
        
        CnCRuntime.CnCReturn Compute(int tag, CnCRuntime.IInputCollection<int, int> B);
    }
    
    public interface IStep3 {
        
        CnCRuntime.CnCReturn Compute(int tag, CnCRuntime.IOutputCollection<int, int> C);
    }
    
    public interface IStep4 {
        
        CnCRuntime.CnCReturn Compute(object tag, CnCRuntime.IOutputCollection<int, int> C);
    }
    
    public interface IExampleGraph : CnCRuntime.Graph {
        
        CnCRuntime.IItemCollection<int, int> B {
            get;
        }
        
        CnCRuntime.IItemCollection<int, int[,]> A {
            get;
        }
        
        CnCRuntime.IItemCollection<int, int> C {
            get;
        }
        
        CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> Tag1 {
            get;
        }
        
        CnCRuntime.ITagCollection<int> Tag2 {
            get;
        }
    }
    
    public class Tag1 : CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> {
        
        private IStep1 m_Step1;
        
        private IExampleGraph m_graph;
        
        public Tag1(IExampleGraph graph, IStep1 Step1) {
            m_graph = graph;
            m_Step1 = Step1;
        }
        
        public virtual void Put(Microsoft.FSharp.Core.Tuple<int, int> tag) {
            CnCRuntime.PutTag(m_graph, (unit => this.m_Step1.Compute(tag, this.m_graph.A, this.m_graph.B, this.m_graph.Tag2)));
        }
    }
    
    public class Tag2 : CnCRuntime.ITagCollection<int> {
        
        private IStep2 m_Step2;
        
        private IStep3 m_Step3;
        
        private IExampleGraph m_graph;
        
        public Tag2(IExampleGraph graph, IStep2 Step2, IStep3 Step3) {
            m_graph = graph;
            m_Step2 = Step2;
            m_Step3 = Step3;
        }
        
        public virtual void Put(int tag) {
            CnCRuntime.PutTag(m_graph, (unit => this.m_Step2.Compute(tag, this.m_graph.B)));
            CnCRuntime.PutTag(m_graph, (unit => this.m_Step3.Compute(tag, this.m_graph.C)));
        }
    }
    
    public class ExampleGraph : IExampleGraph {
        
        private System.Threading.CountdownEvent m_ic;
        
        private CnCRuntime.IItemCollection<int, int> m_itemC_B;
        
        private CnCRuntime.IItemCollection<int, int[,]> m_itemC_A;
        
        private CnCRuntime.IItemCollection<int, int> m_itemC_C;
        
        private CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> m_tagC_Tag1;
        
        private CnCRuntime.ITagCollection<int> m_tagC_Tag2;
        
        private IStep1 m_stepC_Step1;
        
        private IStep2 m_stepC_Step2;
        
        private IStep3 m_stepC_Step3;
        
        private IStep4 m_stepC_Step4;
        
        public virtual CnCRuntime.IItemCollection<int, int> B {
            get {
                return this.m_itemC_B;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<int, int[,]> A {
            get {
                return this.m_itemC_A;
            }
        }
        
        public virtual CnCRuntime.IItemCollection<int, int> C {
            get {
                return this.m_itemC_C;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int>> Tag1 {
            get {
                return this.m_tagC_Tag1;
            }
        }
        
        public virtual CnCRuntime.ITagCollection<int> Tag2 {
            get {
                return this.m_tagC_Tag2;
            }
        }
        
        public virtual System.Threading.CountdownEvent outstandingTasks {
            get {
                return this.m_ic;
            }
        }
        
        public static IExampleGraph Create(IStep1 Step1, IStep2 Step2, IStep3 Step3, IStep4 Step4) {
            ExampleGraph g = new ExampleGraph();
            g.m_stepC_Step1 = Step1;
            g.m_stepC_Step2 = Step2;
            g.m_stepC_Step3 = Step3;
            g.m_stepC_Step4 = Step4;
            g.m_itemC_B = new CnCRuntime.ItemCollection<int, int>();
            g.m_itemC_A = new CnCRuntime.ItemCollection<int, int[,]>();
            g.m_itemC_C = new CnCRuntime.ItemCollection<int, int>();
            g.m_tagC_Tag1 = new Tag1(g, Step1);
            g.m_tagC_Tag2 = new Tag2(g, Step2, Step3);
            g.m_ic = new System.Threading.CountdownEvent(1);
            return g;
        }
    }
}
