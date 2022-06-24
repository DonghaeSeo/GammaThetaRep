Reduction[dim_, S_, T_, Ttuples_] := Module[{
    id, Utuples, Stuples, result
},

    id = IdentityMatrix[dim/3];

    Utuples = (ArrayFlatten[{
        {Inverse[#], 0, 0},
        {0, Inverse[#], 0},
        {0, 0, id}
    }].ArrayFlatten[{
        {-#, #, 0},
        {id, id, 0},
        {0, 0, id}
    }])&/@DiagonalMatrix/@(Part[#, dim/3+1;;2dim/3]&/@Ttuples);

    Stuples = (#.S.Transpose[#])&/@(
        IdentityMatrix[dim][[#]]&/@(
            PermutationList[#, dim]&/@(
                FindPermutation[T, #]&/@Ttuples
            )
        )
    );

    

    (* Stuples = Table[
        Utuples[[i]].Stuples[[i]].Inverse[Utuples[[i]]],
        {i, Length[Stuples]}
    ];

    result = Select[
        Transpose[{Stuples, Ttuples}],
        ((Union@Flatten[{
            #[[dim/3+1;;2dim/3, 1;;dim/3]],
            #[[dim/3+1;;2dim/3, dim/3+1;;2dim/3]],
            #[[1;;dim/3, dim/3+1;;2dim/3]]
        }] == {0})&)@First[#]&
    ];

    If[Length[result] == 0, Return[False]];

    Table[
        {
            result[[i, 1, 1;;dim/3, 1;;dim/3]],
            result[[i, 2, dim/3+1;;2dim/3]]
        },
        {i, Length[result]}
    ] *)

]

RRR[Sblocks_, Tblocks_] := Module[{
    dim, Ttuples
},

    dim = Total[Length/@Sblocks];

    Ttuples = Select[
        Flatten/@Tuples[Permutations/@Diagonal/@Tblocks],
        (
            Union[#[[1;;dim/3]] + #[[dim/3+1;;2dim/3]]] == {0} &&
            OrderedQ[#[[2dim/3+1;;-1]]]
        )&
    ];

    Reduction[
        dim, #, Flatten@Diagonal/@Tblocks, Ttuples
    ]&@ArrayFlatten[
        {Sblocks[[1]], 0},
        {0, Sblocks[[2]]}
    ]

]

RRI[S_, T_] := Module[{
    dim, Ttuples
},
    
    dim = Length[S];

    Ttuples = Select[
        Permutations@Diagonal[T],
        (
            Union[#[[1;;dim/3]] + #[[dim/3+1;;2dim/3]]] == {0} &&
            OrderedQ[#[[2dim/3+1;;-1]]]
        )&
    ];

    Reduction[dim, S, Diagonal[T], Ttuples]

]