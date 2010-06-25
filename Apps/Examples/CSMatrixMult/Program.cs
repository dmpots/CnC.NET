
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;



namespace CSMatrixMult
{
    

    class InitStep : MMGraph.Iinit
    {
        public CnCRuntime.CnCReturn Compute(Microsoft.FSharp.Core.Tuple<int, int> tag, CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans, CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags)
        {
            int i = tag.Item1;
            int j = tag.Item2;

            Microsoft.FSharp.Core.Tuple<int, int, int> outTag = CnCRuntime.Tuple3(i, j, 0);
            scans.Put(outTag, 0.0);
            computeTags.Put(outTag);
            return CnCRuntime.CnCReturn.Ok;
        }
    }


    class ComputeStep : MMGraph.Icompute
    {
        public CnCRuntime.CnCReturn Compute(Microsoft.FSharp.Core.Tuple<int, int, int> tag, CnCRuntime.IItemCollection<Microsoft.FSharp.Core.Tuple<int, int, int>, double> scans, CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> A, CnCRuntime.IInputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> B, CnCRuntime.IInputCollection<int, int> N, CnCRuntime.IOutputCollection<Microsoft.FSharp.Core.Tuple<int, int>, double> C, CnCRuntime.ITagCollection<Microsoft.FSharp.Core.Tuple<int, int, int>> computeTags)
        {
             int i = tag.Item1;
             int j = tag.Item2;
             int k = tag.Item3;
            //printfn "Compute(%d,%d,%d)" i j k
            
            double aik = A.Get(CnCRuntime.Tuple2(i,k));
            double bkj = B.Get(CnCRuntime.Tuple2(k,j));
            double cij = scans.Get(tag) + aik * bkj;
            int nMax = N.Get(Program.nTag);
            //printfn "Compute(%d,%d,%d) Gets" i j k
            
            if (k < (nMax - 1) ) {
                //printfn "Compute(%d,%d,%d) Scans Put (%d,%d,%d)" i j k i j (k+1)
                scans.Put(CnCRuntime.Tuple3(i,j,k+1), cij);
                computeTags.Put(CnCRuntime.Tuple3(i,j,k+1));
            }    
            if(k == (nMax-1)) {
                //printfn "Compute(%d,%d,%d) C Put" i j k
                C.Put(CnCRuntime.Tuple2(i, j), cij);
            }
            //printfn "Compute(%d,%d,%d) Done" i j k    
            return CnCRuntime.CnCReturn.Ok;
        }
    }


    class Program
    {
        public static int nTag = 0;
        static void Main(string[] args)
        {
            int N = 10;// try int(Sys.argv.[1]) with _ -> 10
            MMGraph.IMMGraph graph = MMGraph.MMGraph.Create(new InitStep(), new ComputeStep());
            graph.N.Put(nTag, N);
            for (int i = 0; i < N; i++)
                for(int j = 0; j < N; j++) {
                    graph.A.Put(CnCRuntime.Tuple2(i, j), 1.0);
                    graph.B.Put(CnCRuntime.Tuple2(i, j), 1.0);
                    graph.initTags.Put(CnCRuntime.Tuple2(i, j));
                }
            
            DateTime start = System.DateTime.Now;
            Console.WriteLine( "Graph Run Started");
            CnCRuntime.Run(graph);
            DateTime stop = System.DateTime.Now;
            Console.WriteLine( "Graph Run Finished {0}",(stop - start));

            for (int i = 0; i < N; i++)
            {
                for (int j = 0; j < N; j++)
                {
                    double v = graph.C.Get(CnCRuntime.Tuple2(i, j));
                    Console.Write("{0} ", v);


                }
                Console.WriteLine("");
            }
        }
    }
}
