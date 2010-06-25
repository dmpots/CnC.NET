#light

type Graph = Statement list

(* Statement *)
and Statement = 
    | Declaration of Declaration
    | Relation    of Relation

(* Declaration *)    
and Declaration = 
    | ItemDecl of ItemDeclaration
    | TagDecl  of TagDeclaration
    | StepDecl of StepDeclaration
    
(* Relation *)
and Relation =
    | StepExecution    of StepExecution
    | StepPrescription of StepPrescription

(* Step Execution *)
and StepExecution = (ItemInstance list * StepInstance * OutputInstance list)
    
and OutputInstance =
    | ItemInst of ItemInstance
    | TagInst  of TagInstance

(* Step Prescription *)
and StepPrescription = 
    TagInstance * (StepInstance list)

(* Tag Instances *)
and TagInstance  = {
    Name : string;
    Tag  : TagDescription;
}
and TagDescription = TagComponent list
and TagComponent = 
    | BaseTypeComponent     of BaseTypeTagComponent
    | FunctionTypeComponent of FunctionTypeTagComponent        
and BaseTypeTagComponent = {
    ComponentType : Type;
    ComponentName : string;    
}
and FunctionTypeTagComponent = {
    FunctionName : string;
    ReturnType : Type;
    ParameterList: string list;   
}

(* Item Instance *)
and ItemInstance = {
    Name : string;
    Type : Type;
    Tag : TagDescription;
}

(* Step Instances *)        
and StepInstance = 
    | UserDefinedStep of UserStepInstance
    | Env
and UserStepInstance = {
    Name : string;
    Tag  : TagDescription;
}

(* Declarations *)
and ItemDeclaration = ItemInstance
and TagDeclaration  = TagInstance
and StepDeclaration = StepInstance
and Type = string

(*
Graph Grammar 

graph:
statements

statements:
	terminated_declaration
	terminated_relation
	statements terminated_declaration
	statements terminated_relation

terminated_declaration:
	declaration ;

declaration:
	item_declaration
	tag_declaration
 	step_declaration

terminated_relation:
	relation ;

relation:
	step_execution
	step_prescription
						
step_execution:
	instance_list -> step_instance 
	instance_list -> step_instance -> instance_list
	instance_list <- step_instance 
	instance_list <- step_instance <- instance_list
	step_instance <- instance_list
	step_instance -> instance_list

step_prescription:
	tag_instance :: step_instance_list
	step_instance_list :: tag_instance
						
instance_list:
	item_instance
	tag_instance
	instance_list , item_instance
	instance_list , tag_instance

tag_instance:
	< T_NAME tag_description >
	< T_NAME >

tag_description:
	: tag_component_list

tag_component_list:
	tag_component
	tag_component_list , tag_component
					
tag_component:
	T_NAME T_NAME
	T_TYPE T_NAME
	T_NAME
	T_NAME T_NAME ( param_list )					
	T_TYPE T_NAME ( param_list )					
	T_NAME ( param_list )			

param_list:
	param
	param_list , param
					
param:
	T_NAME

item_instance:
	[ item_definition tag_description ]
	[ item_definition ]

item_definition:
	T_NAME T_NAME
	T_TYPE T_NAME
	T_NAME
						
step_instance_list:
	step_instance
	step_instance_list , step_instance

step_instance:
	( T_NAME )
	( T_NAME tag_description )
	T_ENV

item_declaration:
	item_instance attribute_list
	item_instance

tag_declaration:
	tag_instance attribute_list
	tag_instance

step_declaration:
	step_instance attribute_list
	step_instance

attribute_list:
	attribute
	attribute_list , attribute

attribute:
	T_NAME = T_NAME
	T_NAME = T_NUMBER
	T_NAME = T_QUOTEDVAL
*)





