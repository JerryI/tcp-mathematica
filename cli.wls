If[!FileExistsQ["default_conn"],
    $ip=InputString["Enter the address (ip:port): "];
    Put[$ip, "default_conn"];
,
    $ip = Get["default_conn"];
];

Print["starting zmq-like client..."];

$cli = SocketConnect[$ip,"TCP"];

stream = <|"raw"->CreateDataStructure["RingBuffer", 150], "length"->0, "residue"->{}, "promise"->Eval|>;

$listen = SocketListen[$cli, Function[x,  
    
        StringTemplate["received: `` bytes"][x["DataByteArray"] // Length]//PrintDebug;
        stream["raw"]["PushBack", x["DataByteArray"]];

        (*handling the data*)

        Module[{data = {}, result},

            If[stream["length"] == 0,
                PrintDebug["checking the incomming message"];
                (*check the residue*)
                If[Length[stream["residue"]] > 0,
                    data = stream["residue"];
                    PrintDebug["found residue..."];
                    stream["residue"] = {};
                ];

                (*check if it contains 4 bytes and extract the length*)
                While[True,
                    If[Length[data] >= 4,
                        PrintDebug["length extracted..."];
                        stream["length"] = ImportByteArray[data[[1 ;; 4]], "UnsignedInteger32"][[1]];
                        data = Drop[data, 4];
                        Break[];
                    ,
                        PrintDebug["add more bytes..."];
                        If[stream["raw"]["EmptyQ"], Break[]];
                        If[Length[data] == 0,
                            data = stream["raw"]["PopBack"];
                        ,
                            data = Join[data, stream["raw"]["PopBack"]];
                        ];
                        
                    ];
                ];

                (*only of the length is known*)
                If[stream["length"] != 0,
                    StringTemplate["expect `` bytes"][stream["length"]]//PrintDebug;
                    
                    While[True,
                        If[Length[data] >= stream["length"], Break[]];
                        If[stream["raw"]["EmptyQ"],
                            (*return back to the residue*)
                            stream["residue"] = data;
                            Break[];
                        ,
                            PrintDebug["add more bytes for the payload..."];
                            If[Length[data] == 0, 
                                data = stream["raw"]["PopBack"]
                            ,
                                data = Join[data, stream["raw"]["PopBack"]]
                            ];
                        ];
                    ];

                    If[Length[data] >= stream["length"],
                        (*harvest the data*)
                        PrintDebug["success!"];
                        result = Take[data, stream["length"]];
                        stream["residue"] = Drop[data, stream["length"]];
                        stream["length"] = 0;    

                        result = result//BinaryDeserialize;   
                        stream["promise"][result, $cli//First];  
                        (*back to default*)
                        stream["promise"] = Eval;               
                    ,
                        (*return back to the residue*)
                        PrintDebug["not enough bytes..."];
                        stream["residue"] = data;                        
                    ];
                    
                ,
                    PrintDebug["not enough bytes to extract length..."];
                    (*return back to the residue*)
                    stream["residue"] = data;
                ];



            ,
                PrintDebug["continue transfering"];

                (*check the residue*)
                If[Length[stream["residue"]] > 0,
                    data = stream["residue"];
                    PrintDebug["found residue..."];
                    stream["residue"] = {};
                ];

                While[True,
                    If[Length[data] >= stream["length"], Break[]];
                    If[stream["raw"]["EmptyQ"],
                        (*return back to the residue*)
                        stream["residue"] = data;
                        Break[];
                    ,
                        PrintDebug["add more bytes for the payload..."];
                        If[Length[data] == 0, 
                            data = stream["raw"]["PopBack"]
                        ,
                            data = Join[data, stream["raw"]["PopBack"]]
                        ];
                    ];
                ]   
                If[Length[data] >= stream["length"],
                    (*harvest the data*)
                    PrintDebug["success!"];
                    result = Take[data, stream["length"]];
                    stream["residue"] = Drop[data, stream["length"]];
                    stream["length"] = 0;    

                    result = result//BinaryDeserialize;   
                    stream["promise"][result, $cli//First];  
                    (*back to default*)
                    stream["promise"] = Eval;               
                ,
                    (*return back to the residue*)
                    PrintDebug["not enough bytes..."];
                    stream["residue"] = data;                        
                ];                                


            ];
        ];


     ]
]; 

EvalAndReply[message_, src_]:=
    (*virtual env for evaluation*)
    Block[{socket = src}, 
        With[{data = message//ReleaseHold},
            PrintDebug[data//ToString];
            SendAsync[data, src];
            data
        ]
    ];


Eval[message_, src_]:=
    (*virtual env for evaluation*)
    Block[{socket = src}, 
        With[{data = message//ReleaseHold},
            PrintDebug[data//ToString];
            data
        ]
    ];

SendAsync[message_, src_, OptionsPattern[]]:=With[{socket = SocketObject[src], data = message//Hold//BinarySerialize},
    BinaryWrite[socket, ExportByteArray[Length[data], "UnsignedInteger32"]];
    stream["promise"] = OptionValue[Promise];
    BinaryWrite[socket, data];
];

SetAttributes[SendAsync, HoldFirst];
Options[SendAsync] = {Promise -> Eval};
