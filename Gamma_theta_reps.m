Ef[num_] := Exp[2 Pi I / num];



GAP2Mathematica[filename_] := Module[{dataset},

    dataset = ToExpression@StringReplace[
        StringDelete[Import[filename], 
        "rec"
        ],
        {
            "(\n" -> "<|\n", 
            " )" -> " |>", 
            ":=" -> "->", 
            "[" -> "{", 
            "]" -> "}", 
            "E(" -> "Ef[",
            "i" -> "I", 
            ")" -> "]",
            "S" -> "\"S\"",
            "T" -> "\"T\"",
            "level" -> "\"level\"",
            "degree" -> "\"degree\"",
            "name" -> "\"name\""
        }
    ];

    Export[StringReplace[filename, "txt" -> "m"], dataset];

];



IrrepQ[s_, tsqr_] := Module[{ns, ntsqr, sol, dim},

    dim = Length[s];

    ns = N[s];
    ntsqr = N[tsqr];

    sol = Array[x, {dim, dim}];
    sol = sol/.First@Solve[sol.s == s.sol];
    sol = sol/.First@Solve[sol.tsqr == tsqr.sol];

    If[
        DiagonalMatrixQ@Chop[sol] && (Length@Variables[sol] == 1),
        Return[True],
        Return[False]
    ]

];



T2Tsqr[reps_] := Union@First@Last@Reap@Do[
    Sow[
        <|
            "S" -> reps[[i]]["S"],
            "T^2" -> MatrixPower[reps[[i]]["T"], 2],
            "degree" -> reps[[i]]["degree"]
        |>
    ],
    {i, Length[reps]}
];