#light

open System.IO
open System.Text
open CnCAst

type Configuration = {
    graphName  : string;
    sourceType : CodeGen.SourceLanguage;
    debug      : bool;
    inputFile  : string;
}

let parseArgs () =
    let graphName  = ref "CnCGraph"
    let debugOut   = ref false
    let sourceType = ref CodeGen.FSharp
    let inputFile  = ref ""

    let argspec = [
            ("--name", Arg.String(fun s -> graphName := s), "Name of the graph class to generate");
            ("--debug", Arg.Set(debugOut), "Print debugging Output");
            ("--lang",  Arg.String(function | "fs" -> sourceType := CodeGen.FSharp | _ -> sourceType := CodeGen.CSharp),"Source language to generate");
        ]
    Arg.parse argspec (fun s -> inputFile := s) "CnC.NET Parser"
    
    //Checks
    if !inputFile = "" || not(System.IO.File.Exists(!inputFile)) then
        fprintfn stderr "Error: Input file '%s' does not exist'" !inputFile
        Arg.usage argspec "CnC.NET.exe [options] graphFile"
        exit 1
    
    {graphName = !graphName; sourceType = !sourceType; debug = !debugOut; inputFile = !inputFile}





let parseInput inputFile = 
    use inputReader = File.OpenText(inputFile) in
    let lexbuf = Lexing.from_text_reader Encoding.UTF8 inputReader   
    try
        Parser.start Lexer.token lexbuf          
    with e ->
       let pos = lexbuf.EndPos
       fprintfn stderr "Error near line %d, character %d\n" (pos.Line+1) pos.Column
       exit 1
       
let main() =
    let config = parseArgs()
    let ast = parseInput (config.inputFile)
    if config.debug then fprintfn stderr "%A" ast
    let graph = CnCIR.Conversion.ConvertAstToIR ast config.graphName
    if config.debug then fprintfn stderr "%A" graph
    CodeGen.generateCodeForGraph graph config.sourceType

main()