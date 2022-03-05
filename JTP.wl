(* ::Package:: *)

(* ::Chapter:: *)
(*JerryI Transfer Protocol*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["JTP`"]


(* ::Section:: *)
(*Clear names*)


ClearAll["`*"]


(* ::Section:: *)
(*Public names*)


JTPServer::usage = 
"JTPServer[]
JTPServer[opts]"


JTPServerStart::usage = 
"JTPServerStart[server]"


JTPServerStop::usage = 
"JTPServerStop[server]"


JTPClientSend::usage = 
"JTPClientSend[address, expr]"


JTPClientRead::usage = 
"JTPClientRead[address]"


JTPClientEvaluate::usage = 
"JTPClientEvaluate[address, expr]"


(* ::Section:: *)
(*Begin private*)


(*Begin["`Private`"]*)


(* ::Section:: *)
(*Serialization*)


SetAttributes[serialize, HoldFirst]


serialize[expr_] := 
Module[{data, length}, 
    data = BinarySerialize[Hold[expr]]; 
    length = ExportByteArray[Length[data], "UnsignedInteger32"]; 
    Join[length, data]
]


getLength[data_ByteArray] := 
First[ImportByteArray[data[[1 ;; 4]], "UnsignedInteger32"]]


deserialize[buffer_DataStructure?(DataStructureQ[#, "RingBuffer"]&), length_Integer] := 
Module[{data = buffer["PopBack"]}, 
	While[Length[data] < length, data = Join[data, buffer["PopBack"]]]; 
	BinaryDeserialize[data]
]


(* ::Section:: *)
(*Evaluation*)


evaluate[kernel_LinkObject, Hold[expr_]] := 
Module[{$expr = expr}, 
    With[{$def = Language`ExtendedFullDefinition[$expr]}, 
        If[LinkReadyQ[kernel], 
            LinkWrite[kernel, Unevaluated[Language`ExtendedFullDefinition[] = $def; expr]], 
            Missing[StringTemplate["Kernel [``] not ready"][kernel]]
        ]
    ]
]


evaluate[func: _Symbol | _Function, Hold[expr_]] := 
func[expr]


result[kernel_LinkObject] := 
If[LinkReadyQ[kernel], 
    LinkRead[kernel][[1]], 
    Missing[StringTemplate["Kernel [``] not ready"][kernel]]
]


selectKernel[{Evaluate}] := 
Evaluate


selectKernel[kernels: {__LinkObject}] := 
RandomChoice[Select[kernels, LinkReadyQ]]


createAsyncKernels[n_Integer?Positive] := 
Table[LinkLaunch["mathkernel -mathlink"], {n}]


(* ::Section:: *)
(*Logging*)


writeLog[log_DataStructure?(DataStructureQ[#, "Queue"]&), message_String, args___] := 
Block[{$message = StringTemplate[message][args]}, 
    log["Push", $message]; 
    Print[$message]; 
    Return[$message]
]


(* ::Section:: *)
(*Connection*)


openFreeSocket[host_String, port_Integer] := 
Block[{$port = port, $socket = SocketOpen[{host, port}, "TCP"]}, 
    While[FailureQ[$socket], 
        $socket = SocketOpen[{host, $port++}, "TCP"]
    ]; 
    Return[<|"port" -> $port, "socket"  -> $socket|>]
]


openFreeSocket[assoc_?AssociationQ] := 
openFreeSocket[#host, #port]& @ assoc


(* ::Section:: *)
(*Handler*)


SetAttributes[handler, HoldFirst]


handler[server_Symbol?AssociationQ][assoc_?AssociationQ] := 
Module[{set, get, uuid = assoc["SourceSocket"][[1]], data = assoc["DataByteArray"]}, 
	set = Function[{key, value}, server["buffer", uuid, key] = value]; 
	get = Function[key, server["buffer", uuid, key]]; 
	If[Not[KeyExistsQ[server["buffer"], uuid]], 
		server["buffer", uuid] = <|
			"data" -> CreateDataStructure["RingBuffer", 256], 
			"status" -> "Empty", 
			"length" -> 0, 
			"currentLength" -> 0, 
			"result" -> Null
		|>; 
		writeLog[server["log"], "Received "]
	]; 
	Which[
		get["status"] == "Empty",
			set["length", getLength[data]]; 
			set["currentLength", Length[data[[4 ;; ]]]]; 
			get["data"]["PopBack", data[[4 ;; 1]]]; 
			set["status", "Filling"]; , 
			
		get["status"] == "Filling", 
			set["currentLength", get["currentLength"] + Length[data]]; 
			get["data"]["PopBack", data]; 
	]; 
	Which[get["length"] == get["currentLength"],  
		set["result", evalaute[selectKernel[server["kernels"]], get["data"]]]; 
		set["status", "Ready"]; 
		get["data"]["DropAll"]; 
		set["length", 0]; 
		set["currentLength", 0]; 
		
	]; 
]


(* ::Section:: *)
(*Server*)


SetAttributes[JTPServer, HoldFirst]


Options[JTPServer] = {
    "host" -> "localhost", 
    "port" -> 8000, 
    "kernels" -> {Evaluate}
}


JTPServer[opts___?OptionQ] := With[{server = Unique["JTP`Objects`Server$"]}, 
    server = <|
		"host" -> OptionValue[JTPServer, Flatten[{opts}], "host"], 
		"port" -> OptionValue[JTPServer, Flatten[{opts}], "port"], 
		"kernels" -> OptionValue[JTPServer, Flatten[{opts}], "kernels"], 
		"socket" -> Automatic, 
		"handler" -> handler[server], 
		"listener" -> Automatic, 
		"status" -> "Not started", 
		"buffer" -> <||>, 
		"log" -> CreateDataStructure["DynamicArray"], 
		"self" -> JTPServer[server]
	|>; 
	server[["log"]]["Append", StringTemplate["[<*Now*>] JTPServer created"][]]; 
	Return[JTPServer[server]]
]


JTPServer /: 
MakeBoxes[obj: JTPServer[server_Symbol?AssociationQ], form_] := (
	BoxForm`ArrangeSummaryBox[
		JTPServer, 
		obj, 
		Null, 
		{
			{BoxForm`SummaryItem[{"port: ", server[["port"]]}], SpanFromLeft}, 
			{BoxForm`SummaryItem[{"host: ", server[["host"]]}], SpanFromLeft}, 
			{BoxForm`SummaryItem[{"status: ", server[["status"]]}], SpanFromLeft}
		}, {
			{BoxForm`SummaryItem[{"kernels: ", server[["kernels"]]}], SpanFromLeft}, 
			{BoxForm`SummaryItem[{"self: ", server[["self"]]}] /. JTPServer -> Defer, SpanFromLeft}
		}, 
		form
	]
)


JTPServer /: 
JTPServerStart[JTPServer[server_Symbol?AssociationQ]] := (
	server[[{"port", "socket"}]] = Values[openFreeSocket[server]]; 
	server["listener"] = SocketListen[server["socket"], server["handler"]]; 
	JTPServer[server]
)


JTPServer[server_Symbol?AssociationQ][keys__String] := 
server[keys]


JTPServer[server_Symbol?AssociationQ][keys_Symbol] := 
server[ToString[keys]]


JTPServer[server_Symbol?AssociationQ][key_Symbol] := 
server[ToString[key]]


JTPServer /: 
Set[name_Symbol, server_JTPServer] := (
	name /: Set[name[key: _String | _Symbol], value_] := With[{$server = server}, $server[key] = value];
	Block[{JTPServer}, SetAttributes[JTPServer, HoldFirst]; name = server]
)


JTPServer /: 
Set[JTPServer[symbol_Symbol?AssociationQ][key_String], value_] := 
symbol[[key]] = value


JTPServer /: 
Set[JTPServer[symbol_Symbol?AssociationQ][key_Symbol], value_] := 
symbol[[ToString[key]]] = value


(* ::Section:: *)
(*Client*)


SetAttributes[JTPClientEvaluate, HoldRest]


JTPClientEvaluate[{host_String, port_Integer}, expr_] := 
Module[{socket = SocketConnect[{host, port}, "TCP"]}, 
	Write[socket, serialize[expr]]; 
	SocketReadMessage[socket]; 
]


(* ::Section:: *)
(*End private*)


(*End[]*) (*`Private`*)


(* ::Section:: *)
(*End package*)


EndPackage[] (*JTP`*)
