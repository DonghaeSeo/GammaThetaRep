LoadPackage("SL2Reps");
SetInfoLevel(InfoSL2Reps, 0);

Read("Restrict.g");

STPermutations := function(S, T)

    local dim, diag, permlist, permutation, p, i, j, m, ran, result;

    diag := DiagonalOfMat(T);
    dim := Length(diag);
    permlist := PermutationsList(diag);

    # m := NullMat(dim, dim);
    ran := [1..dim];

    result := [];

    for permutation in permlist do
        p := Permuted(ran, PermListList(permutation, diag));
        m := NullMat(dim, dim);
        for i in ran do
            for j in ran do
                m[i][j] := S[p[i]][p[j]];
            od;
        od;
        # Display(m);
        Append(result, [rec(S := m, T := DiagonalMat(permutation))]);
        # Append(result, [m]);
    od;

    return result;

end;

RepsByReducibles := function(d1, d2)

    local irr1, irr2, irr1p, irr2p, rep1, rep2, dim, diag, result, Scand, Tcand, tmp;

    dim := d1 + d2;

    if dim mod 3 <> 0 then
        return false;
    fi;

    irr1 := SL2IrrepsOfDegree(d1);
    irr2 := SL2IrrepsOfDegree(d2);

    irr1p := [];
    irr2p := [];

    for rep1 in irr1 do
        Append(irr1p, STPermutations(rep1.S, rep1.T));
    od;

    for rep2 in irr2 do
        Append(irr2p, STPermutations(rep2.S, rep2.T));
    od;

    result := [];

    for rep1 in irr1p do
        for rep2 in irr2p do
            diag := Concatenation(
                DiagonalOfMat(rep1.T),
                DiagonalOfMat(rep2.T)
            );
            if Set(diag{[1..dim/3]} + diag{[dim/3+1..2*dim/3]}) <> [0] then
                continue;
            elif IsSortedList(diag{[2*dim/3+1..dim]}) = false then
                continue;
            else
                # Scand := DirectSumMat(rep1.S, rep2.S);
                # Tcand := DiagonalMat(diag);
                # tmp := Restrict(Scand, Tcand);
                # if tmp <> false then
                #     Append(result, [tmp]);
                # fi;
                tmp := Restrict(
                    DirectSumMat(rep1.S, rep2.S),
                    DiagonalMat(diag)
                );
                if tmp = false then
                    continue;
                fi;
                Append(result, [tmp]);
            fi;
        od;
    od;

    return result;

end;

# RepsByIrreps := function(d)

#     local reps, rep, test, list, tmp, result;

#     if d mod 3 <> 0 then
#         return false;
#     fi;

#     result := [];
#     reps := SL2IrrepsOfDegree(d);

#     for rep in reps do
#         list := IndexPerm(rep.S, rep.T);
#         if list = false then
#             continue;
#         fi;
#         for test in list do
#             tmp := Restrict(test.S, test.T);
#             if tmp = false then
#                 continue;
#             fi;
#             Append(result, [tmp]);
#         od;
#     od;    

#     return Set(result);

# end;

# RepsByReducibles := function(d1, d2)

#     local irr1, irr2, result, irr1p, irr2p, irr, i, j, Scand, Tcand, tmp;

#     if (d1 + d2) mod 3 <> 0 then
#         return false;
#     fi;

#     irr1 := SL2IrrepsOfDegree(d1);
#     irr2 := SL2IrrepsOfDegree(d2);
#     result := [];

#     irr1p := [];
#     irr2p := [];

#     for irr in irr1 do
#         tmp := IndexPerm(irr.S, irr.T);
#         if tmp = false then
#             continue;
#         fi;
#         Append(irr1p, tmp);
#     od;

#     for irr in irr2 do
#         tmp := IndexPerm(irr.S, irr.T);
#         if tmp = false then
#             continue;
#         fi;
#         Append(irr2p, tmp);
#     od;

#     for i in irr1p do
#         for j in irr2p do
#             Scand := DirectSumMat(i.S, j.S);
#             Tcand := DirectSumMat(i.T, j.T);
#             tmp := Restrict(Scand, Tcand);
#             if tmp = false then
#                 continue;
#             fi;
#             Append(
#                 result, [tmp]
#             );
#         od;
#     od;

#     return Set(result);

# end;