using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSFib
{

    class Fib : FibGraph.Ifib
    {
        public CnCRuntime.CnCReturn Compute(int n, CnCRuntime.ITagCollection<int> fib, CnCRuntime.ITagCollection<int> fibSum)
        {
            if (n > 1)
            {
                fib.Put(n - 2);
                fib.Put(n - 1);
                fibSum.Put(n);
            }
            return CnCRuntime.CnCReturn.Ok;
        }
    }

    class FibSum : FibGraph.IfibSum
    {
        public CnCRuntime.CnCReturn Compute(int n, CnCRuntime.IItemCollection<int, int> fibs)
        {
            int x = fibs.Get(n - 1);
            int y = fibs.Get(n - 2);
            fibs.Put(n, x + y);

            return CnCRuntime.CnCReturn.Ok;
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            int N = 20;
            FibGraph.IFibGraph graph = FibGraph.FibGraph.Create(new Fib(), new FibSum());
            graph.fibs.Put(0, 0);
            graph.fibs.Put(1, 1);
            graph.fib.Put(N);

            CnCRuntime.Run(graph);
            Console.WriteLine("Fib({0}) = {1}", N, graph.fibs.Get(N));
        }
    }
}
