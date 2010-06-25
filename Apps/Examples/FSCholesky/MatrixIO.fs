#light

type Configuration = {
    verbose    : bool;
    matrixSize : int;
    blockSize  : int;    
    numberOfThreads : int;
    inputFile  : string;
}

let parseArgs () =
    let verbose    = ref false
    let matrixSize = ref 1
    let blockSize  = ref 2
    let inputFile  = ref ""
    let numberOfThreads = ref System.Environment.ProcessorCount

    let argspec = [
            ("-v",  Arg.Set(verbose), "Print Matrix");
            ("-n",  Arg.Int(fun i -> matrixSize := i), "Matrix Size");
            ("-b",  Arg.Int(fun i -> blockSize  := i), "Block Size");
            ("-t",  Arg.Int(fun i -> numberOfThreads  := i), "Number Of Threads");
        ]
    Arg.parse argspec (fun s -> inputFile := s) "Cholesky"
    let config = {verbose= !verbose; matrixSize= !matrixSize; blockSize= !blockSize; numberOfThreads= !numberOfThreads; inputFile= !inputFile}
    
    // Validations
    if config.matrixSize % config.blockSize <> 0 then
        fprintfn stderr "Error: Block size not a multiple of matrix size"
        Arg.usage argspec "Cholesky"
        exit 1            

    if config.inputFile = "" || not(System.IO.File.Exists(config.inputFile)) then
        fprintfn stderr "Error: File '%s' does not exist" config.inputFile
        Arg.usage argspec "Cholesky"
        exit 1
    config

//let c = {verbose= true; matrixSize= 6; blockSize= 6; numberOfThreads= 1; inputFile= "InputFiles/m6.in"}
let readMatrix (config:Configuration) =     
    let sb = System.Text.StringBuilder()
    let floats = seq {                
        use fs = System.IO.File.OpenText(config.inputFile)                        
        while not(fs.EndOfStream) do
            sb.Remove(0,sb.Length) |> ignore
            while fs.Peek() <> int(' ') && not(fs.EndOfStream) do
                sb.Append(char(fs.Read())) |> ignore                       
            fs.Read() |> ignore //space
            
            yield float(sb.ToString())
    }     
    let e = floats.GetEnumerator()
    Array2.init config.matrixSize config.matrixSize (fun i j -> 
        if not(e.MoveNext()) then failwith "Wrong Matrix Size"
        e.Current
    )
  
let writeMatrix (A:double[,]) =
    let fileName = "OUT.m"
    let fs = System.IO.File.CreateText(fileName)
    A |> Array2.iter(fun f -> fs.Write(string(f) + " "))
    fs.Close()

let writeMatrixFromCollection (config:Configuration) (cLkji : CnCRuntime.IInputCollection<Tuple<int,int,int>, double[,]>) =
    let p  = config.matrixSize / config.blockSize
    let b  = config.blockSize
    let fout = System.IO.File.CreateText("OUT.m")
    
    let _ =    
        for i = 0 to (p-1) do
        for i_b = 0 to (b-1) do
            let k = ref 1
            for j = 0 to i do
                let matOut = cLkji.Get(CnCRuntime.T3(i,j,!k))
                if i <> j then
                    for j_b = 0 to (b-1) do
                        fprintf fout "%f " matOut.[i_b,j_b]
                    done
                else
                    for j_b = 0 to i_b do
                        fprintf fout "%f " matOut.[i_b,j_b]
                    done            
                incr(k)
            done        
         done 
         done
    fout.Close()
     
       
let splitMatrix (A:double[,]) (config:Configuration) =
    let p = config.matrixSize / config.blockSize
    let is = [0 .. (p-1)]
    let js = [0 .. (p-1)]
    let b  = config.blockSize

    is |> Seq.map (fun i ->
    js |> Seq.map (fun j ->        
        ((i,j), Array2.init b b (fun ti tj ->  A.[(i*b+ti),(j*b+tj)]))                    
    ))
    |> Seq.concat
    
        