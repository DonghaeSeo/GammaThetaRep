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
            "s" -> "\"s\"",
            "t" -> "\"t\"",
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



T2Tsqr[reps_] := First@Last@Reap@Do[
    Sow[
        <|
            "s" -> reps[[i]]["s"],
            "t^2" -> MatrixPower[reps[[i]]["t"], 2],
            "degree" -> reps[[i]]["degree"]
        |>
    ],
    {i, Length[reps]}
];



SL2T2Tsqr[reps_] := First@Last@Reap@Do[
    Sow[
        <|
            "s" -> reps[[i]]["S"],
            "t^2" -> MatrixPower[reps[[i]]["T"], 2],
            "degree" -> reps[[i]]["degree"]
        |>
    ],
    {i, Length[reps]}
];



InducedRep[rep_] := Module[{S, T, id, V},
    id = IdentityMatrix[rep["degree"]];

    S = ArrayFlatten[{
        {rep["s"], 0, 0},
        {0, 0, MatrixPower[rep["s"], 2]},
        {0, id, 0}
    }];
    T = ArrayFlatten[{
        {0, rep["t^2"], 0},
        {id, 0, 0},
        {0, 0, Inverse[rep["s"].rep["t^2"]]}
    }];

    (* V = Transpose@Eigenvectors[T];
    <|
        "S" -> Inverse[V].S.V,
        "T" -> Inverse[V].T.V,
        "degree" -> rep["degree"]
    |> *)

    <|
        "S" -> S,
        "T" -> T,
        "degree" -> rep["degree"]
    |>
];



(* InducedRep[rep_] := Module[{Ulist, tinvlist, id},
    tinvlist = (#.Inverse[DiagonalMatrix[
        Sqrt/@Diagonal[rep["T^2"]]
    ]]&)/@DiagonalMatrix/@Tuples[
        {1, -1}, 
        rep["degree"]
    ];
    id = IdentityMatrix[rep["degree"]];
    Ulist = ArrayFlatten[{
        {-id, id, 0},
        {#, #, 0},
        {0, 0, id}
    }]&/@tinvlist;
] *)





EquivalentQ[sl2z_, ind_] := Module[{tol, dim, diag, d, P, V, spec, U, Usol, cond},
    dim = 3ind["degree"];
    V = Chop@Transpose@Eigenvectors@N[ind["T"]];
    tol = 10;
    spec = Mod[Rationalize[Chop@Arg@Diagonal[Inverse[V].ind["T"].V] / (2 Pi)], 1];
    diag = Mod[Rationalize[Chop@Arg@Diagonal@N[sl2z["T"]] / (2 Pi)], 1];
    (* Print[Sort@Tally[diag], Sort@Tally[spec]]; *)
    If[Sort@Tally[diag] != Sort@Tally[spec], Return[False]];
    P = IdentityMatrix[dim][[#]]&/@Permute[
        Range[dim], FindPermutation[spec, diag]
    ];
    d = DiagonalMatrix[Array[x, {dim}]]/.x[1]->1;
    U = Chop[d.P.Inverse[V]];
    cond = Reduce[U.N[ind["S"]] == N[sl2z["S"]].U];
    If[cond == False, Return[False]];
    Usol = Chop[U/.First@Solve[cond]];
    Usol = Usol/.(#->1&/@Variables[Usol]);
    (* Print[Usol]; *)
    If[
        Chop@Det[Usol] == 0,
        Return[False],
        Return[True]
    ]
];



OrderST[rep_] := Module[{diag, P},
    diag = Diagonal[rep["t^2"]];
    If[OrderedQ[diag], Return[rep]];
    P = IdentityMatrix[rep["degree"]][[#]]&/@Permute[
        Range[rep["degree"]],
        FindPermutation[diag, Sort[diag]]
    ];
    <|
        "s" -> P.rep["s"].Inverse[P],
        "t^2" -> P.rep["t^2"].Inverse[P],
        "degree" -> rep["degree"]
    |>
]