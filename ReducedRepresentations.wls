ReducedRepresentations[Sblocks_, Tblocks_] := Module[{
    dim, id, Stuples, Ttuples, Utuples, result
},
    
    dim = Sum@Length/@Sblocks;

    Ttuples = Select[
        Flatten/@Tuples[Permutations/@Diagonal/@Tblocks],
        (
            Union[#[[1;;dim/3]] + #[[dim/3+1;;2dim/3]]] == {0} &&
            OrderedQ[#[[2dim/3+1;;-1]]]
        )&
    ];

    id = IdentityMatrix[dim];

    Utuples = ArrayFlatten[{
        {Inverse[#], 0, 0},
        {0, Inverse[#], 0},
        {0, 0, id}
    }].ArrayFlatten[{
        {-#, #, 0},
        {id, id, 0},
        {0, 0, id}
    }]&/@DiagonalMatrix/@Part[#, dim/3+1;;2dim/3]&/@Ttuples;

    Stuples = (#.ArrayFlatten[
        {Sblocks[[1]], 0},
        {0, Sblocks[[2]]}
    ].#)&/@Utuples;

    result = Select[
        Transpose[Stuples, Ttuples],
        (Union@Flatten[
            #[[dim/3+1;;2dim/3, 1;;dim/3]],
            #[[dim/3+1;;2dim/3, dim/3+1;;2dim/3]],
            #[[1;;dim/3, dim/3+1;;2dim/3]]
        ] == 0)&@First
    ];

    If[Length[result] == 0, Return[False]];

    result

]