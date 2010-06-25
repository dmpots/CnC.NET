#light

open System.CodeDom
open System.CodeDom.Compiler
//open CnCIR

type SourceLanguage = FSharp | CSharp
type AccessType = Input | Output | InputOutput

let inputCollectionType = "CnCRuntime.IInputCollection"
let outputCollectionType = "CnCRuntime.IOutputCollection"
let itemCollectionType = "CnCRuntime.IItemCollection"
let tagCollectionType = "CnCRuntime.ITagCollection"
let graphInterfaceType (g : CnCIR.Graph) = "I"+ g.Name

let writeCodeToFile outFileBaseName (provider : CodeDomProvider) compileUnit =    
    let outFileName = outFileBaseName + "." + provider.FileExtension
    printfn "WRITING TO %s" outFileName
    use tw = new IndentedTextWriter(new System.IO.StreamWriter(outFileName, false), "    ")    
    provider.GenerateCodeFromCompileUnit(compileUnit, tw, CodeGeneratorOptions())
    tw.Close()

let codeTypeReferenceForItemCollection (collection : CnCIR.DataCollection) (direction : AccessType) =
    let baseType = match direction with | Input -> inputCollectionType | Output -> outputCollectionType | InputOutput -> itemCollectionType
    let genericTypes = [|CodeTypeReference(collection.Tag.TupleType); CodeTypeReference(collection.Type) |]
    CodeTypeReference(baseType, genericTypes)

let codeTypeReferenceForRuntimeItemCollection (collection : CnCIR.DataCollection)  =
    let baseType = "CnCRuntime.ItemCollection"
    let genericTypes = [|CodeTypeReference(collection.Tag.TupleType); CodeTypeReference(collection.Type) |]
    CodeTypeReference(baseType, genericTypes)


let codeTypeReferenceForTagCollection (collection : CnCIR.ControlCollection) =            
    CodeTypeReference(tagCollectionType, [|CodeTypeReference(collection.Tag.TupleType)|])
    
let codeTypeReferenceForStepCollection (collection : CnCIR.StepCollection) =
    CodeTypeReference("I"+collection.Name)

let codeTypeReferenceForGraph    = CodeTypeReference("CnCRuntime.Graph")
let codeTypeReferenceForRuntime  = CodeTypeReference("CnCRuntime")
let codeTypeReferenceForGraphInterface (g : CnCIR.Graph)    = CodeTypeReference(graphInterfaceType g)
let codeTypeReferenceForGraphInstance  (g : CnCIR.Graph)    = CodeTypeReference(g.Name)
let codeTypeReferenceForCountdown = CodeTypeReference(typeof<System.Threading.CountdownEvent>)

let processStepParametersTypeReferences (step : CnCIR.StepCollection) (action : string -> CodeTypeReference -> unit) =
    let separateReadWrites (step :  CnCIR.StepCollection) =
        let listUnionDifference list1 list2 = let union,difference = List.partition (fun a -> List.mem a list2) list1 in union,difference
        let inBoth = fst(listUnionDifference step.Inputs step.DataOutputs)
        if inBoth = [] then
            step.Inputs,step.DataOutputs,[]
        else
            let inputOnly =  snd (listUnionDifference step.Inputs inBoth)
            let outputOnly = (snd(listUnionDifference step.DataOutputs inBoth))
            (inputOnly,outputOnly, inBoth) 
    
    let reads,writes,both = separateReadWrites step
    both    |> List.iter (fun input -> action input.Name (codeTypeReferenceForItemCollection input InputOutput))
    reads   |> List.iter (fun input -> action input.Name (codeTypeReferenceForItemCollection input Input))       
    writes  |> List.iter (fun input -> action input.Name (codeTypeReferenceForItemCollection input Output))                  
    step.ControlOutputs |> List.iter (fun input -> action input.Name (codeTypeReferenceForTagCollection input))       
    ()

let generateStepClasses (graph : CnCIR.Graph) =     
    let generateStepClass (step : CnCIR.StepCollection) =
        let klass = CodeTypeDeclaration(Name="I"+step.Name, IsInterface=true)      
        let computeMethod = CodeMemberMethod(Name="Compute",ReturnType=CodeTypeReference("CnCRuntime.CnCReturn"))
        computeMethod.Attributes <- MemberAttributes.Abstract ||| MemberAttributes.Public
        klass.Members.Add(computeMethod) |> ignore
        
        // Tag Parameter
        let parameters = CodeParameterDeclarationExpressionCollection()
        parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference(step.Tag.TupleType), "tag")) |> ignore
       
        // Collection Parameters 
        processStepParametersTypeReferences step (fun name typeRef -> parameters.Add(CodeParameterDeclarationExpression(Type=typeRef, Name=name)) |> ignore)        
        computeMethod.Parameters.AddRange(parameters)        
        klass
        
    graph.Steps |> List.map generateStepClass |> List.to_array

        
let generateGraphClassInterface (graph : CnCIR.Graph) =
    let addPropertyMember (klass : CodeTypeDeclaration) (name : string) (codeType : CodeTypeReference) =        
        let p = CodeMemberProperty(Name=name, Type=codeType, HasGet=true, HasSet=false)                
        klass.Members.Add(p) |> ignore        

    let klass = CodeTypeDeclaration(Name=(graphInterfaceType graph), IsInterface=true)
    klass.BaseTypes.Add(codeTypeReferenceForGraph) |> ignore
    graph.Data |> List.iter (fun d -> addPropertyMember klass d.Name (codeTypeReferenceForItemCollection d InputOutput))
    graph.Tags |> List.iter (fun d -> addPropertyMember klass d.Name (codeTypeReferenceForTagCollection d))
    
    klass
    
let prescribedSteps (graph : CnCIR.Graph) (tagCollection : CnCIR.ControlCollection) =
        graph.Steps |> List.filter (fun s -> s.Control = tagCollection)
            
let generateTagClasses (graph :CnCIR.Graph) sourceType =               
    let codeSnippetForStep (step : CnCIR.StepCollection) =
        let stepComputeInvocation (step :CnCIR.StepCollection) =            
            let paramList = System.Collections.Generic.List<string>()            
            processStepParametersTypeReferences step (fun name typeRef -> paramList.Add("this.m_graph."+name))            
            let paramString = paramList.ToArray() |> Array.fold_left (fun s param -> s + ", " + param) "tag"
            "this.m_"+step.Name+".Compute("+paramString+")"

        let computeStep = stepComputeInvocation step
        match sourceType with
        | CSharp -> CodeSnippetExpression("(unit => "+computeStep+")") :> CodeExpression
        | FSharp -> CodeSnippetExpression("System.Func<unit,CnCRuntime.CnCReturn>(fun _ -> "+computeStep+")") :> CodeExpression

    let generateTagClass (tagCollection : CnCIR.ControlCollection) =
        let steps = (prescribedSteps graph tagCollection)
        let klass = CodeTypeDeclaration(Name=tagCollection.Name)
        klass.BaseTypes.Add(codeTypeReferenceForTagCollection tagCollection) |> ignore
        
        // Fields
        steps |> List.iter (fun s -> klass.Members.Add( CodeMemberField(Name="m_"+s.Name, Type=codeTypeReferenceForStepCollection s)) |> ignore)
        klass.Members.Add(CodeMemberField(Name="m_graph", Type=(codeTypeReferenceForGraphInterface graph))) |> ignore
                
        // Constructor
        let ctor = CodeConstructor()
        ctor.Attributes <- MemberAttributes.Public
        ctor.Parameters.Add(CodeParameterDeclarationExpression(Name="graph", Type=(codeTypeReferenceForGraphInterface graph))) |> ignore
        ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(FieldName="m_graph"), CodeVariableReferenceExpression("graph"))) |> ignore
        steps |> List.iter (fun s -> ctor.Parameters.Add(CodeParameterDeclarationExpression(Name=s.Name, Type=(codeTypeReferenceForStepCollection s))) |> ignore )
        steps |> List.iter (fun s -> ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(FieldName="m_"+s.Name), CodeVariableReferenceExpression(s.Name))) |> ignore)        
        klass.Members.Add(ctor) |> ignore
                      
        // Put() Method
        let putMethod = CodeMemberMethod(Name="Put")        
        let tagParam  = CodeParameterDeclarationExpression(Name="tag",Type=CodeTypeReference(tagCollection.Tag.TupleType))
        tagParam.Direction <- FieldDirection.In        
        putMethod.Parameters.Add(tagParam) |> ignore
        putMethod.Attributes <- MemberAttributes.Public
        putMethod.ImplementationTypes.Add(codeTypeReferenceForTagCollection tagCollection) |> ignore
        
            // Debug stmt        
        let cond       = CodePropertyReferenceExpression(CodePropertyReferenceExpression(CodeTypeReferenceExpression(codeTypeReferenceForRuntime), "Trace"), "Enabled")
        let formatMref = CodeMethodReferenceExpression(TargetObject=CodeTypeReferenceExpression(typeof<System.String>),MethodName="Format")         
        let formatMInv = CodeMethodInvokeExpression(formatMref,[|(CodePrimitiveExpression("PT: "+tagCollection.Name+" PutTag {0}") :> CodeExpression); (CodeTypeReferenceExpression("tag") :> CodeExpression) |])
        let mref = CodeMethodReferenceExpression(TargetObject=CodeTypeReferenceExpression(typeof<System.Diagnostics.Debug>),MethodName="WriteLineIf")         
        let mInv = CodeMethodInvokeExpression(mref,[|(cond :> CodeExpression); (formatMInv :> CodeExpression) |])
        putMethod.Statements.Add(mInv) |> ignore
        
        if sourceType = FSharp then // F# hack to remove the capture of tag as a mutable variable       
            putMethod.Statements.Add(CodeSnippetStatement("let tag = tag")) |> ignore
        steps |> List.iter (fun step ->
            let mref = CodeMethodReferenceExpression(TargetObject=CodeTypeReferenceExpression(codeTypeReferenceForRuntime),MethodName="PutTag")
            let mInv = CodeMethodInvokeExpression(mref,[|(CodeFieldReferenceExpression(FieldName="m_graph") :> CodeExpression); codeSnippetForStep step |])
            putMethod.Statements.Add(mInv) |> ignore
        )
 
        klass.Members.Add(putMethod) |> ignore
        klass
    
    graph.Tags |> List.map generateTagClass |> List.to_array


let generateGraphClass (graph : CnCIR.Graph) sourceType =
    let fieldNameForItem (item : CnCIR.DataCollection)   = "m_itemC_"+item.Name
    let fieldNameForTag  (tag : CnCIR.ControlCollection) = "m_tagC_"+tag.Name
    let fieldNameForStep (step : CnCIR.StepCollection)   = "m_stepC_"+step.Name
    let countdownFieldName = "m_ic"
    let countdownPropertyName = "outstandingTasks"
    
    
    // Class Type Declaration
    let klass = CodeTypeDeclaration(Name=graph.Name)
    klass.BaseTypes.Add(codeTypeReferenceForGraphInterface graph) |> ignore
    
    // Field Declarations
    klass.Members.Add(CodeMemberField(Name=countdownFieldName, Type=CodeTypeReference(typeof<System.Threading.CountdownEvent>))) |> ignore    
    graph.Data |> List.iter (fun itemC ->
        klass.Members.Add(CodeMemberField(Name=fieldNameForItem itemC, Type=codeTypeReferenceForItemCollection itemC InputOutput)) |> ignore
    )
    graph.Tags |> List.iter (fun tagC ->
        klass.Members.Add(CodeMemberField(Name=fieldNameForTag tagC, Type=codeTypeReferenceForTagCollection tagC)) |> ignore
    )
    graph.Steps |> List.iter (fun stepC ->
        klass.Members.Add(CodeMemberField(Name=fieldNameForStep stepC, Type=codeTypeReferenceForStepCollection stepC)) |> ignore
    )
    
    // Static Factory Method
    let createMethod = CodeMemberMethod(Name="Create", ReturnType=codeTypeReferenceForGraphInterface graph)
    createMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static
    graph.Steps |> List.iter (fun s -> createMethod.Parameters.Add(CodeParameterDeclarationExpression(Name=s.Name, Type=(codeTypeReferenceForStepCollection s))) |> ignore )
        // Graph lloction
    let localGraphVarName = "g"                
    let graphVarDecl = CodeVariableDeclarationStatement(Name=localGraphVarName, Type=codeTypeReferenceForGraphInstance graph)
    graphVarDecl.InitExpression <- CodeObjectCreateExpression(CreateType=codeTypeReferenceForGraphInstance graph)
    createMethod.Statements.Add(graphVarDecl) |> ignore
        // Step Assignments
    graph.Steps |> List.iter (fun s -> 
        let target = CodeFieldReferenceExpression(TargetObject=CodeVariableReferenceExpression(localGraphVarName), FieldName=fieldNameForStep s)
        let source = CodeVariableReferenceExpression(s.Name)
        let assign = CodeAssignStatement(Left=target, Right=source)
        createMethod.Statements.Add(assign) |> ignore        
    )
        // Item Collection Assignments
    graph.Data |> List.iter (fun d -> 
        let target = CodeFieldReferenceExpression(TargetObject=CodeVariableReferenceExpression(localGraphVarName), FieldName=fieldNameForItem d)
        let source = CodeObjectCreateExpression(codeTypeReferenceForRuntimeItemCollection d, [|(CodePrimitiveExpression(d.Name) :> CodeExpression)|])
        let assign = CodeAssignStatement(Left=target, Right=source)
        createMethod.Statements.Add(assign) |> ignore        
    )
        // Tag Collection Assignments
    graph.Tags |> List.iter (fun t -> 
        let target = CodeFieldReferenceExpression(TargetObject=CodeVariableReferenceExpression(localGraphVarName), FieldName=fieldNameForTag t)
        let ctorParams = System.Collections.Generic.List<CodeExpression>()
        ctorParams.Add((CodeVariableReferenceExpression(localGraphVarName) :> CodeExpression))
        let steps = prescribedSteps graph t
        steps |> List.iter (fun s -> ctorParams.Add(CodeVariableReferenceExpression(s.Name)))        
        let source = CodeObjectCreateExpression(t.Name, ctorParams.ToArray())
        let assign = CodeAssignStatement(Left=target, Right=source)                
        createMethod.Statements.Add(assign) |> ignore        
    )
        // Countdown Assignment
    let generateCountdownAssignment() =
        let target = CodeFieldReferenceExpression(TargetObject=CodeVariableReferenceExpression(localGraphVarName), FieldName=countdownFieldName)
        let source = CodeObjectCreateExpression(codeTypeReferenceForCountdown,[|(CodePrimitiveExpression(1) :> CodeExpression)|])
        let assign = CodeAssignStatement(Left=target, Right=source)
        createMethod.Statements.Add(assign) |> ignore 
    generateCountdownAssignment()   
    
        // Return Statement
    createMethod.Statements.Add(CodeMethodReturnStatement(CodeVariableReferenceExpression(localGraphVarName))) |> ignore        
    
    // Interface Implementation        
    if sourceType = FSharp then
        klass.Members.Add(CodeSnippetTypeMember("interface I"+graph.Name+ " with")) |> ignore
        graph.Data |> List.iter (fun d ->
            klass.Members.Add(CodeSnippetTypeMember("member self."+d.Name+" with get() = self."+fieldNameForItem d)) |> ignore                        
        )
        graph.Tags |> List.iter (fun t ->
            klass.Members.Add(CodeSnippetTypeMember("member self."+t.Name+" with get() = self."+fieldNameForTag t)) |> ignore                        
        )
        klass.Members.Add(CodeSnippetTypeMember("member self."+countdownPropertyName+" with get() = self."+countdownFieldName)) |> ignore                        
        
        klass.Members.Add(CodeSnippetTypeMember("end")) |> ignore
    else
        let addMembers propNameFun fieldTypeFun fieldNameFun mem =
            let p = CodeMemberProperty(Name=(propNameFun mem), Type=fieldTypeFun mem, HasGet=true, HasSet=false)
            p.GetStatements.Add(CodeMethodReturnStatement(CodeFieldReferenceExpression(TargetObject=CodeThisReferenceExpression(), FieldName=fieldNameFun mem))) |> ignore            
            p.ImplementationTypes.Add(codeTypeReferenceForGraphInterface graph) |> ignore
            p.Attributes <- MemberAttributes.Public 
            klass.Members.Add(p) |> ignore
            // Collections and Tags
        graph.Data |> List.iter (addMembers (fun d -> d.Name) (fun d -> codeTypeReferenceForItemCollection d InputOutput) (fun d -> fieldNameForItem d))
        graph.Tags |> List.iter (addMembers (fun d -> d.Name) (fun d -> codeTypeReferenceForTagCollection  d            ) (fun d -> fieldNameForTag d))
            // Countdown 
        let p = CodeMemberProperty(Name=countdownPropertyName, Type=codeTypeReferenceForCountdown, HasGet=true, HasSet=false)
        p.GetStatements.Add(CodeMethodReturnStatement(CodeFieldReferenceExpression(TargetObject=CodeThisReferenceExpression(), FieldName=countdownFieldName))) |> ignore            
        p.ImplementationTypes.Add(codeTypeReferenceForGraphInterface graph) |> ignore
        p.Attributes <- MemberAttributes.Public 
        klass.Members.Add(p) |> ignore

    klass.Members.Add(createMethod) |> ignore
    klass

(*
let generateDebuggingClass (graph : CnCIR.Graph) =
    let fieldNameForTrace = "m_Trace"
    let codeTypeReferenceForTraceField = CodeTypeReference(typeof<System.Diagnostics.BooleanSwitch>)
    
    // Class Type Declaration
    let klass = CodeTypeDeclaration(Name="Debug")        
    
    // Trace Field
    let f = CodeMemberField(Name=fieldNameForTrace, Type=codeTypeReferenceForTraceField, Attributes=MemberAttributes.Static)
    f.InitExpression <- CodeObjectCreateExpression(codeTypeReferenceForTraceField,[|(CodePrimitiveExpression(graph.Name+"Trace") :> CodeExpression); (CodePrimitiveExpression("Trace Graph Execution") :> CodeExpression)|])
    klass.Members.Add(f) |> ignore
    
    
    //let ctor = CodeConstructor()
    //ctor.Attributes <- MemberAttributes.Public
    //ctor.Parameters.Add(CodeParameterDeclarationExpression(Name="graph", Type=(codeTypeReferenceForGraphInterface graph))) |> ignore
    //ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(FieldName="m_graph"), CodeVariableReferenceExpression("graph"))) |> ignore
    //steps |> List.iter (fun s -> ctor.Parameters.Add(CodeParameterDeclarationExpression(Name=s.Name, Type=(codeTypeReferenceForStepCollection s))) |> ignore )
    //steps |> List.iter (fun s -> ctor.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(FieldName="m_"+s.Name), CodeVariableReferenceExpression(s.Name))) |> ignore)        
    //klass.Members.Add(ctor) |> ignore

    
    // Trace Property
    let p = CodeMemberProperty(Name="Trace", Type=codeTypeReferenceForTraceField, HasGet=true, HasSet=false, Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
        // Get
    p.GetStatements.Add(CodeMethodReturnStatement(CodeFieldReferenceExpression(TargetObject=CodeThisReferenceExpression(), FieldName=fieldNameForTrace))) |> ignore            
    
    (*
        // Set
    let target = CodeFieldReferenceExpression(TargetObject=CodeThisReferenceExpression(), FieldName=fieldNameForTrace)
    let source = CodeVariableReferenceExpression("value")
    let assign = CodeAssignStatement(Left=target, Right=source)         
    p.SetStatements.Add(assign) |> ignore
    *)
    
    klass.Members.Add(p) |> ignore
    klass
*)    
let generateCodeForGraph (graph : CnCIR.Graph) sourceLang =
    let compileUnit = CodeCompileUnit()
    let graphNamespace = CodeNamespace(graph.Name)
    compileUnit.Namespaces.Add(graphNamespace) |> ignore
    
    // Add debugging class
    //graphNamespace.Types.Add(generateDebuggingClass graph) |> ignore
    
    // Add step declarations
    graphNamespace.Types.AddRange((generateStepClasses graph))    
    
    // Add Graph class interface declaration
    graphNamespace.Types.Add(generateGraphClassInterface graph) |> ignore
    
    // Add Tag class implementations 
    graphNamespace.Types.AddRange((generateTagClasses graph sourceLang))
    
    // Add Graph class implementation
    graphNamespace.Types.Add((generateGraphClass graph sourceLang)) |> ignore
    
    let codeProvider = 
        match sourceLang with
        | FSharp -> new Microsoft.FSharp.Compiler.CodeDom.FSharpCodeProvider() :> CodeDomProvider
        | CSharp -> new  Microsoft.CSharp.CSharpCodeProvider() :> CodeDomProvider
        
    writeCodeToFile graph.Name codeProvider compileUnit
    
    
let main() =
    generateCodeForGraph CnCIR.Example.graph CSharp
    generateCodeForGraph CnCIR.Example.graph FSharp
        
let generateAllSourceForGraph g =  
    generateCodeForGraph g CSharp 
    generateCodeForGraph g FSharp
       