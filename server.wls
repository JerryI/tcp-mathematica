$port=256;
$ip="127.0.0.1";

Print["starting zmq-like server..."];

$server=SocketOpen[{$ip,ToString[$port]},"TCP"];
While[FailureQ[$server],
    $port++;
    $server=SocketOpen[{$ip,ToString[$port]},"TCP"];
];

streams = <||>;

$listen = SocketListen[$server, Function[x,  
    
        StringTemplate["received: `` bytes"][x["DataByteArray"] // Length]//Print;
        
        If[KeyExistsQ[streams, x["SourceSocket"]//First],
            streams[x["SourceSocket"]//First]["raw"]["PushBack", x["DataByteArray"]];
        ,
            StringTemplate["new client `` "][""]//Print;
            streams[x["SourceSocket"]//First] = <|"raw"->CreateDataStructure["RingBuffer", 150], "length"->0, "residue"->{}, "promise"->EvalAndReply|>;
            streams[x["SourceSocket"]//First]["raw"]["PushBack", x["DataByteArray"]];
        ];

        (*handling the data*)

        Module[{data = {}, result},

            If[streams[#]["length"] == 0,
                Print["checking the incomming message"];
                (*check the residue*)
                If[Length[streams[#]["residue"]] > 0,
                    data = streams[#]["residue"];
                    Print["found residue..."];
                    streams[#]["residue"] = {};
                ];

                (*check if it contains 4 bytes and extract the length*)
                While[True,
                    If[Length[data] >= 4,
                        Print["length extracted..."];
                        streams[#]["length"] = ImportByteArray[data[[1 ;; 4]], "UnsignedInteger32"][[1]];
                        data = Drop[data, 4];
                        Break[];
                    ,
                        Print["add more bytes..."];
                        If[streams[#]["raw"]["EmptyQ"], Break[]];
                        If[Length[data] == 0,
                            data = streams[#]["raw"]["PopBack"];
                        ,
                            data = Join[data, streams[#]["raw"]["PopBack"]];
                        ];
                        
                    ];
                ];

                (*only of the length is known*)
                If[streams[#]["length"] != 0,
                    StringTemplate["expect `` bytes"][streams[#]["length"]]//Print;
                    
                    While[True,
                        If[Length[data] >= streams[#]["length"], Break[]];
                        If[streams[#]["raw"]["EmptyQ"],
                            (*return back to the residue*)
                            streams[#]["residue"] = data;
                            Break[];
                        ,
                            Print["add more bytes for the payload..."];
                            If[Length[data] == 0, 
                                data = streams[#]["raw"]["PopBack"]
                            ,
                                data = Join[data, streams[#]["raw"]["PopBack"]]
                            ];
                        ];
                    ];

                    If[Length[data] >= streams[#]["length"],
                        (*harvest the data*)
                        Print["success!"];
                        result = Take[data, streams[#]["length"]];
                        streams[#]["residue"] = Drop[data, streams[#]["length"]];
                        streams[#]["length"] = 0;    

                        result = result//BinaryDeserialize//ReleaseHold;                    
                        Print[result//ToString];
                        Reply[#, result];
                    ,
                        (*return back to the residue*)
                        Print["not enough bytes..."];
                        streams[#]["residue"] = data;                        
                    ];
                    
                ,
                    Print["not enough bytes to extract length..."];
                    (*return back to the residue*)
                    streams[#]["residue"] = data;
                ];



            ,
                Print["continue transfering"];

                (*check the residue*)
                If[Length[streams[#]["residue"]] > 0,
                    data = streams[#]["residue"];
                    Print["found residue..."];
                    streams[#]["residue"] = {};
                ];

                While[True,
                    If[Length[data] >= streams[#]["length"], Break[]];
                    If[streams[#]["raw"]["EmptyQ"],
                        (*return back to the residue*)
                        streams[#]["residue"] = data;
                        Break[];
                    ,
                        Print["add more bytes for the payload..."];
                        If[Length[data] == 0, 
                            data = streams[#]["raw"]["PopBack"]
                        ,
                            data = Join[data, streams[#]["raw"]["PopBack"]]
                        ];
                    ];
                ]   
                If[Length[data] >= streams[#]["length"],
                    (*harvest the data*)
                    Print["success!"];
                    result = Take[data, streams[#]["length"]];
                    streams[#]["residue"] = Drop[data, streams[#]["length"]];
                    streams[#]["length"] = 0;    

                    result = result//BinaryDeserialize;   
                    streams[#]["promise"][result, #];  
                    (*back to default*)
                    streams[#]["promise"] = EvalAndReply;               
                ,
                    (*return back to the residue*)
                    Print["not enough bytes..."];
                    streams[#]["residue"] = data;                        
                ];                                


            ];
        ]&/@Keys[streams];


     ]
]; 

EvalAndReply[message_, src_]:=
    (*virtual env for evaluation*)
    Block[{socket = src}, 
        With[{data = message//ReleaseHold},
            Print[data//ToString];
            SendAsync[data, src];
        ];
    ];


Eval[message_, src_]:=
    (*virtual env for evaluation*)
    Block[{socket = src}, 
        With[{data = message//ReleaseHold},
            Print[data//ToString];
        ];
    ];

SendAsync[message_, src_, OptionsPattern[]]:=With[{socket = SocketObject[src], data = message//Hold//BinarySerialize},
    BinaryWrite[socket, ExportByteArray[Length[data], "UnsignedInteger32"]];
    streams[src]["promise"] = OptionValue[Promise];
    BinaryWrite[socket, data];
];

SetAttributes[SendAsync, HoldFirst];
Options[SendAsync] = {Promise -> EvalAndReply};
            

Print["listen at: "<>ToString[$port]];
