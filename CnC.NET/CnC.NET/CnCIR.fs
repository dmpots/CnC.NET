#light


type Name = string
type DataType = string 
type TagComponentType = System.Type
type TagType = System.Type

type TagDescription = {
    Arity : int; 
    Types : TagComponentType list;
    TupleType : TagType;
}

type DataCollection = {
    Name : Name;
    Type : DataType;
    Tag  : TagDescription;
}
and ControlCollection = {
    Name : Name;
    Tag  : TagDescription;    
}
and StepCollection = {
    Name    : Name;
    Tag     : TagDescription;
    Control : ControlCollection;
    Inputs  : DataCollection list;
    DataOutputs : DataCollection list; 
    ControlOutputs : ControlCollection list;  
}
and StepOutput = 
    | Data    of DataCollection 
    | Control of ControlCollection

type Graph  = {
    Name  : Name;
    Steps : StepCollection list;
    Tags  : ControlCollection list;
    Data  : DataCollection list;
}

module Example =
    let types1 = [System.Type.GetType("System.Int32"); System.Type.GetType("System.Int32")]
    let types2 = [System.Type.GetType("System.Int32");]
    let td1:TagDescription = {Arity=2; Types = types1; TupleType=(Reflection.FSharpType.MakeTupleType (Array.of_list types1))}
    let td2:TagDescription = {Arity=1; Types = types2; TupleType=types2.Head}
    let a:DataCollection  = {Name="A"; Type="System.Int32"; Tag=td1;}
    let b:DataCollection  = {Name="B"; Type="System.Int32"; Tag=td2;}
    let c:DataCollection  = {Name="C"; Type="System.Int32"; Tag=td2;}
    let tag1:ControlCollection = {Name="Tag1"; Tag=td1;}
    let tag2:ControlCollection = {Name="Tag2"; Tag=td2;}
    let step1:StepCollection = {Name="Step1"; Tag=td1; Control=tag1; Inputs=[a]; DataOutputs=[b;a];  ControlOutputs=[tag2];}
    let step2:StepCollection = {Name="Step2"; Tag=td2; Control=tag2; Inputs=[b]; DataOutputs=[]; ControlOutputs=[]}
    let step3:StepCollection = {Name="Step3"; Tag=td2; Control=tag2; Inputs=[];  DataOutputs=[c]; ControlOutputs=[]}
    
    let graph = {
        Name  = "ExampleGraph";
        Steps = [step1;step2;step3];
        Tags  = [tag1;tag2;];
        Data  = [a;b;c];
    }

module Conversion =                                              
    type CollectionId   = string
    let cid (s : string) = (s : CollectionId)

    (* name filtering *)
    let outputInstanceName = (function | CnCAst.ItemInst ii -> ii.Name | CnCAst.TagInst ti -> ti.Name)
    let inputInstanceName  (ii : CnCAst.ItemInstance) = ii.Name    
    let filterToUniqueNames nameFun olist  =
        let exists (name:string) lst = 
            lst |> List.exists (fun n -> (nameFun n) = name)            
        List.fold_left (fun accum elem ->             
            if exists (nameFun elem) accum then accum else elem::accum
        ) [] olist |> List.rev
    let filterOutputInstanceToUniqueNames = filterToUniqueNames  outputInstanceName
    let filterInputInstanceToUniqueNames  = filterToUniqueNames  inputInstanceName        

    (* tuple types *)        
    let getTupleTypeForTypes = function    
    | []  -> failwithf "tag tuple must contain at least one component type"
    | [t] -> t        
    | types   -> Reflection.FSharpType.MakeTupleType (Array.of_list types)
     
    (* conversion function *)
    let ConvertAstToIR (ast : CnCAst.Graph) (graphName:string) = 
        let itemSymtab = System.Collections.Generic.Dictionary<CollectionId, DataCollection>()
        let tagsSymtab = System.Collections.Generic.Dictionary<CollectionId, ControlCollection>()
        let stepSymtab = System.Collections.Generic.Dictionary<CollectionId, StepCollection>()
        
        let convertTagDescription (td : CnCAst.TagDescription) = 
            let getSystemTypeForTagComponent = function
                | "int" -> typeof<int>
                | "int64" -> typeof<int64>
                | "string" -> typeof<string>
                | s -> failwithf "unsupported tag component type %s" s
            let types = td |> List.map (fun t ->
                match t with
                | CnCAst.BaseTypeComponent bt -> getSystemTypeForTagComponent bt.ComponentType                    
                | CnCAst.FunctionTypeComponent _ -> failwith "function type components not yet supported"
            )
            {Arity=types.Length; Types=types; TupleType=getTupleTypeForTypes types}
        
        let convertItemCollection (ic : CnCAst.ItemInstance) =
            let expandedType = 
                match ic.Type with
                | "int"    -> (typeof<int>).FullName
                | "int[]"  -> (typeof<int[]>).FullName
                | "int[,]"  -> (typeof<int[,]>).FullName
                | "int[,,]"  -> (typeof<int[,,]>).FullName
                
                | "int64"    -> (typeof<int64>).FullName
                | "int64[]"  -> (typeof<int64[]>).FullName
                | "int64[,]"  -> (typeof<int64[,]>).FullName
                | "int64[,,]"  -> (typeof<int64[,,]>).FullName
                
                | "float32"    -> (typeof<float32>).FullName
                | "float32[]"  -> (typeof<float32[]>).FullName
                | "float32[,]"  -> (typeof<float32[,]>).FullName
                | "float32[,,]"  -> (typeof<float32[,,]>).FullName                
                
                | "float"    -> (typeof<float>).FullName
                | "float[]"  -> (typeof<float[]>).FullName
                | "float[,]"  -> (typeof<float[,]>).FullName
                | "float[,,]"  -> (typeof<float[,,]>).FullName
                
                | "double"    -> (typeof<double>).FullName
                | "double[]"  -> (typeof<double[]>).FullName
                | "double[,]"  -> (typeof<double[,]>).FullName
                | "double[,,]"  -> (typeof<double[,,]>).FullName
                
                                                                             
                | "string" -> (typeof<string>).FullName            
                | _       -> ic.Type
            {DataCollection.Name = ic.Name; DataCollection.Type=expandedType; DataCollection.Tag=convertTagDescription ic.Tag;}
        
        let findItemCollectionOrInitialize (ic : CnCAst.ItemInstance) =
            let itemId = cid ic.Name
            if itemSymtab.ContainsKey(itemId) then
                itemSymtab.[itemId]
            else
                let dataC = convertItemCollection ic
                itemSymtab.Add(itemId, dataC)
                dataC  
        
        let findTagCollectionOrInitialize (tc : CnCAst.TagInstance) =
            let tagId =  cid tc.Name  
            if tagsSymtab.ContainsKey(tagId) then
                tagsSymtab.[tagId]
            else
                let tagDescription = convertTagDescription tc.Tag
                let tag = {ControlCollection.Name=tc.Name; ControlCollection.Tag=tagDescription;}
                tagsSymtab.Add(tagId, tag)
                tag
        
        let dummyTag = {ControlCollection.Name="DUMMY"; ControlCollection.Tag=({TagDescription.Arity=0; TagDescription.Types=[]; TagDescription.TupleType=(typeof<obj>)})}
        let findStepCollectionOrInitialize (sc : CnCAst.UserStepInstance) =
            let stepId = cid sc.Name
            let stepC = 
                if stepSymtab.ContainsKey(stepId) then
                    stepSymtab.[stepId]
                else 
                    {StepCollection.Name=sc.Name; StepCollection.Tag=dummyTag.Tag; StepCollection.Control=dummyTag; StepCollection.Inputs=[]; StepCollection.DataOutputs=[]; StepCollection.ControlOutputs=[]}                                                                           
            stepC   
            
        let processGraphNode node =
            match node with
            | CnCAst.Declaration d ->
                match d with
                | CnCAst.ItemDecl id -> findItemCollectionOrInitialize id |> ignore
                | CnCAst.TagDecl  td -> findTagCollectionOrInitialize  td |> ignore
                | CnCAst.StepDecl sd -> 
                    match sd with
                    | CnCAst.Env -> ()
                    | CnCAst.UserDefinedStep sd ->  findStepCollectionOrInitialize sd |> ignore
                
                
            | CnCAst.Relation    r -> 
                match r with
                | CnCAst.StepPrescription (tagInstance, prescriptionList) ->                 
                    // Add tag to symtab if needed                          
                    let cc = findTagCollectionOrInitialize tagInstance

                    // Add steps to symtab
                    prescriptionList |> List.iter (fun step -> 
                        match step with
                        | CnCAst.UserDefinedStep uds -> 
                            let stepC = findStepCollectionOrInitialize uds                                                          
                            stepSymtab.[cid uds.Name] <- {stepC with Tag=cc.Tag; Control=cc}
                        | CnCAst.Env -> ()
                    )
                        
                        
                | CnCAst.StepExecution (inputs,stepType,outputs) -> 
                    match stepType with
                    | CnCAst.Env -> ()
                    | CnCAst.UserDefinedStep step ->                         
                        let itemOutputs,controlOutputs = outputs |> List.partition (function | CnCAst.ItemInst _ -> true | _ -> false)
                        let dataOutputC = itemOutputs |> filterOutputInstanceToUniqueNames |> List.map (function  
                            | CnCAst.ItemInst i -> findItemCollectionOrInitialize i                            
                            |  _ -> failwith "internal logic error"
                        )  
                        let tagOutputC = controlOutputs |> filterOutputInstanceToUniqueNames |> List.map (function                               
                            | CnCAst.TagInst t -> findTagCollectionOrInitialize t                         
                            |  _ -> failwith "internal logic error"
                        )
                        let inputC = inputs |> filterInputInstanceToUniqueNames |> List.map findItemCollectionOrInitialize
                        let stepC = findStepCollectionOrInitialize step
                        stepSymtab.[cid stepC.Name] <- {stepC with Inputs=inputC @ stepC.Inputs; DataOutputs=dataOutputC @ stepC.DataOutputs; ControlOutputs=tagOutputC @ stepC.ControlOutputs}
                                            
        
        ast |> List.iter processGraphNode 
          
        { Name  = graphName;
          Steps = Seq.to_list (seq {for v in stepSymtab -> v.Value});
          Tags  = Seq.to_list (seq {for v in tagsSymtab -> v.Value});
          Data  = Seq.to_list (seq {for v in itemSymtab -> v.Value});
        }
        
