LoadPackage("SL2Reps");
SetInfoLevel(InfoSL2Reps,0);


IsReducible := function(rep)
    local diag;

    diag := DiagonalOfMat(rep.T^2);

    return not IsDuplicateFreeList(diag);

end;


LoadSL2ZReps := function(degree)
    local reps, rep;

    reps := SL2IrrepsOfDegree(degree);

    for rep in reps do
        rep.S := ComplexConjugate(rep.S);
        Unbind(rep.name);
    od;

    return reps;

end;


RotateLeft := function(list)
    local result, len;

    len := Length(list);

    if len = 1 then
        return list;
    fi;

    result := list{[2..len]};
    Append(result, [list[1]]);

    return result;

end;


SortDiagonals := function(diag)
    local pos, neg, others, a, len, dim, idx, idx2, result, pos_set, tuples, tmp;

    pos := [];
    neg := [];
    others := [];

    for a in diag do
        if a in neg then
            continue;
        elif -a in diag then
            Append(pos, [a]);
            Append(neg, [-a]);
        else
            Append(others, [a]);
        fi;
    od;

    len := Length(pos);
    dim := Length(diag) / 3;

    if len < dim then
        return false;
    fi;

    pos_set := [];

    tuples := Tuples([1,-1], len);

    for idx in [1..Length(tuples)] do
        tmp := [];
        for idx2 in [1..len] do
            Append(tmp, [pos[idx2] * tuples[idx][idx2]]);
        od;
        Append(pos_set, [tmp]);
    od;

    result := [];

    for pos in pos_set do
        neg := -pos;
        for idx in [1..len] do
            tmp := [];
            pos := RotateLeft(pos);
            neg := RotateLeft(neg);
            Append(tmp, pos{[1..dim]});
            Append(tmp, neg{[1..dim]});
            if len > dim then
                Append(tmp, pos{[dim+1..len]});
                Append(tmp, neg{[dim+1..len]});
            fi;
            Append(tmp, others);
            Append(result, [tmp]);
        od;
    od;

    return result;

end;


CheckSMat := function(S)
    local dim, nullmat;

    dim := Length(S)/3;
    nullmat := NullMat(dim, dim);

    if (S{[dim+1..2*dim]}{[1..dim]} = nullmat
    and S{[dim+1..2*dim]}{[dim+1..2*dim]} = nullmat
    and S{[1..dim]}{[dim+1..2*dim]} = nullmat) then
        return S{[1..dim]}{[1..dim]};
    else
        return false;
    fi;

end;


main := function(degree)
    local reps, reducible, rep, diag, diag_pmd, dim, P, tmp, testset, 
    t, t_inv, id, U1, U2, result, S, s;

    dim := degree / 3;

    reps := LoadSL2ZReps(degree);
    Print("SL2Z: ", Length(reps), "\n");
    reducible := Filtered(LoadSL2ZReps(degree), g -> IsReducible(g));
    Print("Reducible: ", Length(reducible), "\n");

    testset := [];

    for rep in reducible do
        diag := DiagonalOfMat(rep.T);
        diag_pmd := SortDiagonals(diag);

        if diag_pmd = false then
            continue;
        fi;

        for tmp in diag_pmd do
            P := PermutationMat(PermListList(tmp, diag), degree);
            Append(
                testset,
                [rec(
                    S := P * rep.S * Inverse(P),
                    T := DiagonalMat(tmp),
                    degree := degree,
                    level := rep.level
                )]
            );
        od;
    od;

    id := IdentityMat(dim);

    result := [];

    for rep in testset do
        t := rep.T{[dim+1..2*dim]}{[dim+1..2*dim]};
        t_inv := Inverse(t);
        U1 := BlockMatrix([
            [1,1,-id],
            [1,2,id],
            [2,1,t_inv],
            [2,2,t_inv],
            [3,3,id]
        ],3,3);
        U2 := BlockMatrix([
            [1,1,id],
            [1,2,id],
            [2,1,-t_inv],
            [2,2,t_inv],
            [3,3,id]
        ],3,3);

        s := CheckSMat(U1 * rep.S * Inverse(U1));
        if s = false then
            s := CheckSMat(U2 * rep.S * Inverse(U2));
            if s = false then
                continue;
            fi;
        fi;

        Append(
            result,
            [rec(
                S := s,
                T_square := t^2,
                degree := dim,
                level := rep.level
            )]
        );    
    od;

    Print("result: ", Length(result), "\n");

    return result;

end;


SaveData := function(data, filename)
    local f;

    f := OutputTextFile(filename, false);
    SetPrintFormattingStatus(f, false);
    PrintTo(f, List(data));
    CloseStream(f);

    return true;

end;


# Restrict := function(reps)
#     local result, rep, permlist, perm, rdc;

#     result := [];

#     for rep in reps do
#         permlist := PermuteST(rep.S, rep.T, rep.level);
#         if permlist = false then
#             continue;
#         else
#             for perm in permlist do
#                 rdc := ReduceST(perm.S, perm.T, perm.level);
#                 if rdc = false then
#                     continue;
#                 else
#                     Append(result, [rec(
#                         # S := Inverse(rep.S),
#                         # T := rep.T,
#                         s := rdc.s,
#                         t := rdc.t,
#                         degree := rdc.degree,
#                         level := rdc.level
#                     )]);
#                 fi;
#             od;
#         fi;
#     od;

#     return result;

# end;





# LevelsOfIrreps := function(reps)
#     local rep, result;

#     result := [];

#     for rep in reps do
#         Append(result, [rep.level]);
#     od;

#     return Set(result);

# end;



# SL2IrrepsReduction := function(degree)
#     local rep, result, tmp, diag;

#     result := [];

#     for rep in SL2IrrepsOfDegree(degree) do
#         tmp := rec(
#             S := ComplexConjugate(rep.S),
#             Tsqr := (rep.T)^2,
#             conductor := Conductor(DefaultField(Flat(rep.S))),
#             level := rep.level
#         );
#         diag := DiagonalOfMat(tmp.Tsqr);
#         if Length(Set(diag)) = Length(diag) then
#             continue;
#         fi;
#         Append(result, [tmp]);
#     od;

#     return result;

# end;






# PermuteST := function(S, T, n)
#     local Dim, dim, diag, permlist, perm, P, result;

#     Dim := Length(T);
#     dim := Dim/3;
#     diag := DiagonalOfMat(T);

#     permlist := Filtered(
#         PermutationsList(diag),
#         g -> Set(
#             g{[1..dim]} + g{[dim+1..2*dim]}
#         ) = [0]
#         and IsSortedList(g{[2*dim+1..Dim]})
#         # and IsSortedList(g{[1..dim]})
#     );

#     if permlist = [] then
#         return false;
#     fi;

#     result := [];

#     for perm in permlist do
#         P := PermutationMat(PermListList(perm, diag), Dim);
#         Append(result, [rec(
#             S := P * S * Inverse(P),
#             T := DiagonalMat(perm),
#             degree := Dim,
#             level := n
#         )]);
#     od;

#     return result;

# end;


# ReduceST := function(S, T, n)
#     local Dim, dim, s, t, t_inv, id, U, S_conj;

#     Dim := Length(T);
#     dim := Dim/3;
#     t := T{[dim+1..2*dim]}{[dim+1..2*dim]};
#     t_inv := Inverse(t);
#     id := IdentityMat(dim);

#     U := BlockMatrix([
#         [1,1,-id],
#         [1,2,id],
#         [2,1,t_inv],
#         [2,2,t_inv],
#         [3,3,id]
#     ],3,3);

#     S_conj := U * S * Inverse(U);

#     if (S_conj{[dim+1..2*dim]}{[1..dim]} = NullMat(dim, dim)
#     and S_conj{[dim+1..2*dim]}{[dim+1..2*dim]} = NullMat(dim, dim)
#     and S_conj{[1..dim]}{[dim+1..2*dim]} = NullMat(dim, dim)) then
#         s := S_conj{[1..dim]}{[1..dim]};
#     elif (S_conj{[dim+1..2*dim]}{[1..dim]} = NullMat(dim, dim)
#     and S_conj{[1..dim]}{[1..dim]} = NullMat(dim, dim)
#     and S_conj{[1..dim]}{[dim+1..2*dim]} = NullMat(dim, dim)) then
#         s := S_conj{[dim+1..2*dim]}{[dim+1..2*dim]};
#     else 
#         return false;
#     fi;

#     return rec(
#         s := s, 
#         t := t,
#         degree := dim,
#         level := n
#     );

# end;



# DirectSumReducibleReps := function(dim1, dim2, lev)
#     local reps1, reps2, len, i, j, result;

#     result := [];

#     if dim1 = dim2 then
#         # reps1 := Filtered(
#         #     SL2IrrepsOfDegree(dim1),
#         #     g -> g.level = lev
#         # );

#         reps1 := SL2IrrepsOfDegree(dim1);

#         len := Length(reps1);

#         for i in [1..len] do
#             for j in [i..len] do
#                 Append(
#                     result,
#                     [rec(
#                         S := DirectSumMat(reps1[i].S, reps1[j].S),
#                         T := DirectSumMat(reps1[i].T, reps1[j].T),
#                         degree := dim1 + dim2
#                     )]
#                 );
#             od;
#         od;
#     else 
#         reps1 := Filtered(
#             SL2IrrepsOfDegree(dim1),
#             g -> g.level = lev
#         );
#         reps2 := Filtered(
#             SL2IrrepsOfDegree(dim2),
#             g -> g.level = lev
#         );

#         for i in reps1 do
#             for j in reps2 do
#                 Append(
#                     result,
#                     [rec(
#                         S := DirectSumMat(i.S, j.S),
#                         T := DirectSumMat(i.T, j.T),
#                         degree := dim1 + dim2,
#                         level := lev
#                     )]
#                 );
#             od;
#         od;
#     fi;

#     return result;

# end;