LoadPackage("SL2Reps");
SetInfoLevel(InfoSL2Reps,0);


PermuteST := function(S, T)
    local Dim, dim, diag, permlist, perm, P, result;

    Dim := Length(T);
    dim := Dim/3;
    diag := DiagonalOfMat(T);

    permlist := Filtered(
        PermutationsList(diag),
        g -> Set(
            g{[1..dim]} + g{[dim+1..2*dim]}
        ) = [0]
        and IsSortedList(g{[2*dim+1..Dim]})
        # and IsSortedList(g{[1..dim]})
    );

    if permlist = [] then
        return false;
    fi;

    result := [];

    for perm in permlist do
        P := PermutationMat(PermListList(perm, diag), Dim);
        Append(result, [rec(
            S := P * S * Inverse(P),
            T := DiagonalMat(perm),
            degree := Dim
        )]);
    od;

    return result;

end;


ReduceST := function(S, T)
    local Dim, dim, s, t, t_inv, id, U, S_conj;

    Dim := Length(T);
    dim := Dim/3;
    t := T{[dim+1..2*dim]}{[dim+1..2*dim]};
    t_inv := Inverse(t);
    id := IdentityMat(dim);

    U := BlockMatrix([
        [1,1,-id],
        [1,2,id],
        [2,1,t_inv],
        [2,2,t_inv],
        [3,3,id]
    ],3,3);

    S_conj := U * S * Inverse(U);

    if (S_conj{[dim+1..2*dim]}{[1..dim]} = NullMat(dim, dim)
    and S_conj{[dim+1..2*dim]}{[dim+1..2*dim]} = NullMat(dim, dim)
    and S_conj{[1..dim]}{[dim+1..2*dim]} = NullMat(dim, dim)) then
        s := S_conj{[1..dim]}{[1..dim]};
    elif (S_conj{[dim+1..2*dim]}{[1..dim]} = NullMat(dim, dim)
    and S_conj{[1..dim]}{[1..dim]} = NullMat(dim, dim)
    and S_conj{[1..dim]}{[dim+1..2*dim]} = NullMat(dim, dim)) then
        s := S_conj{[dim+1..2*dim]}{[dim+1..2*dim]};
    else 
        return false;
    fi;

    return rec(
        s := s, 
        t := t,
        degree := dim
    );

end;


SaveAsTXT := function(data, filename)
    local f;

    f := OutputTextFile(filename, false);
    SetPrintFormattingStatus(f, false);
    PrintTo(f, List(data));
    CloseStream(f);

    return true;

end;


Restrict := function(reps)
    local result, rep, permlist, perm, rdc;

    result := [];

    for rep in reps do
        permlist := PermuteST(rep.S, rep.T);
        if permlist = false then
            continue;
        else
            for perm in permlist do
                rdc := ReduceST(ComplexConjugate(perm.S), perm.T);
                if rdc = false then
                    continue;
                else
                    Append(result, [rec(
                        S := Inverse(rep.S),
                        T := rep.T,
                        s := rdc.s,
                        t := rdc.t,
                        degree := rdc.degree
                    )]);
                fi;
            od;
        fi;
    od;

    return result;

end;


DirectSumReducibleReps := function(dim1, dim2, lev)
    local reps1, reps2, len, i, j, result;

    result := [];

    if dim1 = dim2 then
        # reps1 := Filtered(
        #     SL2IrrepsOfDegree(dim1),
        #     g -> g.level = lev
        # );

        reps1 := SL2IrrepsOfDegree(dim1);

        len := Length(reps1);

        for i in [1..len] do
            for j in [i..len] do
                Append(
                    result,
                    [rec(
                        S := DirectSumMat(reps1[i].S, reps1[j].S),
                        T := DirectSumMat(reps1[i].T, reps1[j].T),
                        degree := dim1 + dim2
                    )]
                );
            od;
        od;
    else 
        reps1 := Filtered(
            SL2IrrepsOfDegree(dim1),
            g -> g.level = lev
        );
        reps2 := Filtered(
            SL2IrrepsOfDegree(dim2),
            g -> g.level = lev
        );

        for i in reps1 do
            for j in reps2 do
                Append(
                    result,
                    [rec(
                        S := DirectSumMat(i.S, j.S),
                        T := DirectSumMat(i.T, j.T),
                        degree := dim1 + dim2,
                        level := lev
                    )]
                );
            od;
        od;
    fi;

    return result;

end;


LevelsOfIrreps := function(reps)
    local rep, result;

    result := [];

    for rep in reps do
        Append(result, [rep.level]);
    od;

    return Set(result);

end;
