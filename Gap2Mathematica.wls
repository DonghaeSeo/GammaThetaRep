Ef[num_] := Exp[2 Pi I / num];

GAP2Mathematica[filename_] := Module[{dataset},

    dataset = ToExpression@StringReplace[
        StringDelete[Import[filename], 
        "rec"
        ],
        {"(\n" -> "<|\n", 
        " )" -> " |>", 
        ":=" -> "->", 
        "[" -> "{", 
        "]" -> "}", 
        "E(" -> "Ef[",
        "i" -> "I", 
        ")" -> "]",
        "S" -> "\"S\"",
        "T" -> "\"T\"",
        "degree" -> "\"degree\""}
    ];

    Export[StringReplace[filename, "txt" -> "m"], dataset];

]