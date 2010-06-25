#light



let main() =
    let config = MatrixIO.parseArgs()
    let A = MatrixIO.readMatrix config
    //printfn "%A" (MatrixIO.splitMatrix A config)
    
    Cholesky.Cholesky A config
    
    
main()    