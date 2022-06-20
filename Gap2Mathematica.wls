Gap2Mathematica[filename_] := Module[{dataset},

    dataset = Dataset@ToExpression@StringReplace[
        StringDelete[Import[filename], 
        "rec"
        ],
        {"(\n" -> "<|\n", 
        " )" -> " |>", 
        ":=" -> "->", 
        "[" -> "{", 
        "]" -> "}", 
        "E(" -> "Ef[", 
        ")" -> "]"}
    ];

    Export[StringReplace[filename, "txt" -> "m"], dataset];

]