(* P.S.Inverse[P] *)
P[dim_, perm_] := IdentityMatrix[dim][[#]]&/@(Permute[Range[dim], perm]);

TOrderedQ[diag_] := Module[{dim = Length[diag]},
    (* Union[diag[[1;;dim/3]] + diag[[dim/3+1;;2dim/3]]] == {0} && *)
    diag[[1;;dim/3]] == -diag[[dim/3+1;;2dim/3]] &&
    OrderedQ[diag[[2dim/3+1;;dim]]]
];

IndexPermutation[mat_, perm_] := Module[{dim = Length[mat]},
    P[dim, perm].mat.Inverse[P[dim, perm]]
    (* Inverse[P[dim, perm]].mat.P[dim, perm] *)
    (* mat *)
];

ReducibleQ[mat_] := Module[{dim = Length[mat]},
    (Union@Flatten[{
        mat[[dim/3+1;;2dim/3, 1;;2dim/3]],
        mat[[1;;dim/3, dim/3+1;;2dim/3]]
    }] == {0}) || (Union@Flatten[{
        mat[[1;;2dim/3, 1;;dim/3]],
        mat[[dim/3+1;;2dim/3, 1;;dim/3]]
    }] == {0})
];

Reduction[rep_] := Module[{dim, t, U, id},
    dim = Length[rep[[1]]];
    t = DiagonalMatrix[rep[[2, dim/3+1;;2dim/3]]];
    id = IdentityMatrix[dim/3];
    (* U = ArrayFlatten[{
        {Inverse[t], 0, 0},
        {0, Inverse[t], 0},
        {0, 0, id}
    }].ArrayFlatten[{
        {-t, t, 0},
        {id, id, 0},
        {0, 0, id}
    }]; *)
    U = ArrayFlatten[{
        {id, id, 0},
        {-Inverse[t], Inverse[t], 0},
        {0, 0, id}
    }];
    {U.rep[[1]].Inverse[U], t}
];


Restriction[reps_] := Module[{dim, len, diag, permuted, candidates, result},

    dim = Length[reps[[1]]["S"]];
    len = Length[reps];

    result = Last@Monitor[Reap@Do[
        diag = N@Diagonal[reps[[i]]["T"]];
        (* diag = Diagonal[reps[[i]]["T"]]; *)
        permuted = Select[Permutations[diag], TOrderedQ];
        candidates = Reduction[#]&/@Transpose[{
            IndexPermutation[Inverse@N[reps[[i]]["S"]], #]&/@(
            (* IndexPermutation[Inverse[reps[[i]]["S"]], #]&/@( *)
                FindPermutation[diag, #]&/@permuted
            ),
            permuted
        }];
        (* Print[Table[MatrixForm@Chop[candidates[[j, 1]]],{j,Length[candidates]}]]; *)
        (* Print[Select[candidates, ReducibleQ[#[[1]]]&]]; *)
        Sow[<|
            "S" -> #[[1]][[1;;dim/3, 1;;dim/3]],
            "T^2" -> MatrixPower[#[[2]], 2]
        |>]&/@Select[Chop[candidates], ReducibleQ[#[[1]]]&],
        {i, len}
    ], ProgressIndicator[(i-1) / len]];

    If[Length[result] == 0, Return[False]];

    First[result]
]