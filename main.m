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
            "name" -> "\"name\"",
            "Tsqr" -> "\"T^2\"",
            "conductor" -> "\"conductor\""
        }
    ];

    Export[StringReplace[filename, "txt" -> "m"], dataset];

];



IrrepQ[s_, tsqr_] := Module[{ns, ntsqr, sol, dim},

    dim = Length[s];

    ns = N[s];
    ntsqr = N[tsqr];

    sol = Array[x, {dim, dim}];
    sol = sol/.First@Solve[sol.ns == ns.sol];
    sol = sol/.First@Solve[sol.ntsqr == ntsqr.sol];

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
            "degree" -> reps[[i]]["degree"],
            "level" -> reps[[i]]["level"]
        |>
    ],
    {i, Length[reps]}
];



SL2T2Tsqr[reps_] := First@Last@Reap@Do[
    If[
        EvenQ[reps[[i]]["level"]],
        Sow[
            <|
                "s" -> Conjugate[reps[[i]]["S"]],
                "t^2" -> MatrixPower[reps[[i]]["T"], 2],
                "degree" -> reps[[i]]["degree"],
                "level" -> reps[[i]]["level"]
            |>
        ],
        Sow[
            <|
                "s" -> Conjugate[reps[[i]]["S"]],
                "t^2" -> MatrixPower[reps[[i]]["T"], 2],
                "degree" -> reps[[i]]["degree"],
                "level" -> 2reps[[i]]["level"]
            |>
        ]
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
        "degree" -> 3rep["degree"]
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



(* OrderST[rep_] := Module[{diag, P},
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
] *)



(* OrderingFun[rep_] := Module[{P},
    P = IdentityMatrix[rep["degree"]][[#]]&/@Ordering[rep["t^2"]];
    <|
        "s" -> Inverse[P].rep["s"].P,
        "t^2" -> Sort[rep["t^2"]],
        "d" -> rep["degree"],
        "n" -> rep["level"]
    |>
] *)


OrderingFun[rep_] := Module[{P, spin},
    spin = Rationalize[Arg@N@Diagonal[rep["t^2"]]/(2 Pi)];
    P = IdentityMatrix[rep["degree"]][[#]]&/@Ordering[spin];
    <|
        "s" -> TrigReduce@ExpToTrig[P.rep["s"].Inverse[P]],
        "t^2" -> Sort[spin],
        "d" -> rep["degree"],
        "n" -> rep["level"]
    |>
]


OrderingFun2[rep_] := Module[{P, spin},
    spin = rep["t^2"];
    P = IdentityMatrix[rep["d"]][[#]]&/@Ordering[spin];
    <|
        "s" -> TrigReduce@ExpToTrig[P.rep["s"].Inverse[P]],
        "t^2" -> Sort[spin],
        "d" -> rep["d"],
        "n" -> rep["n"]
    |>
]



Coprime[n_] := Select[Range[n], CoprimeQ[n, #]&];



InverseMultGrp[a_, n_] := Module[{abar},
    abar = Select[Range[n], Mod[a #, n] == 1&];
    If[Length[abar] != 1, Return[False]];
    First[abar]
];



(* HRep[s_, t_, a_, n_] := Module[{abar},
    abar = InverseMultGrp[a, n];
    s.s.Inverse@MatrixPower[
        t, a-1
    ].s.MatrixPower[
        t.t.s, abar-1
    ].Inverse@MatrixPower[
        t, a-1
    ].s
]; *)


HRep[s_, t2_, a_, n_] := Module[{abar},
    abar = InverseMultGrp[a, n];
    s.s.Inverse@MatrixPower[
        t2, (a-1)/2
    ].s.MatrixPower[
        t2.s, abar-1
    ].Inverse@MatrixPower[
        t2, (a-1)/2
    ].s
];



HRepList[s_, t2_, n_] := Module[{alist, abarlist, Hlist},
    alist = Coprime[n];
    abarlist = InverseMultGrp[#, n]&/@alist;
    Hlist = Rationalize@Chop@HRep[N[s], N[t2], #, n]&/@alist;
    <|
        "a" -> alist[[#]],
        "H" -> Hlist[[#]]
    |>&/@Range@Length[alist]
]



CongruenceQ[s_, t2_, n_] := Module[{list, len, t},
    (* t = DiagonalMatrix[Exp[2I Pi #]&/@(t2 / 2)]; *)
    (* list = HRepList[s, t, n]; *)
    list = HRepList[s, t2, n];
    len = Length[list];
    If[Reduce@Flatten@Table[
        list[[i]]["H"].list[[j]]["H"] == Select[
            list, #["a"] == Mod[
                list[[i]]["a"] list[[j]]["a"],
                n
            ]&
        ][[1]]["H"],
        {i, len}, {j, len}
    ] == False, Return[False]];
    If[Reduce@Table[
        s.list[[i]]["H"] == Rationalize@Chop@HRep[
            (* N[s], N[t], InverseMultGrp[list[[i]]["a"], n], n *)
            N[s], N[t2], InverseMultGrp[list[[i]]["a"], n], n
        ].s,
        {i, len}
    ] == False, Return[False]];
    If[Reduce@Table[
        Rationalize@Chop[s.s.MatrixPower[
            (* t, list[[i]]["a"]^2 - list[[i]]["a"] *)
            t2, (list[[i]]["a"]^2 - list[[i]]["a"])/2
        ].s.Inverse@MatrixPower[
            (* t, InverseMultGrp[list[[i]]["a"], n] - 1 *)
            t2, (InverseMultGrp[list[[i]]["a"], n] - 1)/2
        ].s.MatrixPower[
            (* t.t.s, list[[i]]["a"] - 1 *)
            t2.s, list[[i]]["a"] - 1
        ]] == list[[i]]["H"],
        {i, len}
    ] == False, Return[False]];
    True
    
];


OrthogonalMat[angle_] := {{Cos[angle], -Sin[angle]}, {Sin[angle], Cos[angle]}};


PermutationMat[perm_] := IdentityMatrix[Length[perm]][[#]]&/@perm;