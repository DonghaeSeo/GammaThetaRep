LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");
Read("CentralChar.g");
Read("IsFusionRing.g");
Read("IsPrimitiveRep.g");

SortAndRestrict := function(testset)
    local sorted, restricted, result, i, j;

    result := [];

    for i in [1..Length(testset)] do
        sorted := SortFun(testset[i].S, testset[i].T);

        if sorted = false then continue;
        else
            for j in [1..Length(sorted)] do
                restricted := Restrict(sorted[j].S, sorted[j].T);
                if restricted = false then continue;
                else Append(result, [restricted]);
                fi;
            od;
        fi;
    od;

    return result;

end;

GammaThetaRepsByIrreps := function(degree) 
    local testset;

    if degree mod 3 <> 0 then return false; fi;

    testset := SL2IrrepsOfDegree(degree);

    return Set(SortAndRestrict(testset));

end;

GammaThetaRepsByReducibles := function()
    local irreps1, irreps2, testset, i, j;

    irreps1 := SL2IrrepsOfDegree(3);
    irreps2 := SL2IrrepsOfDegree(3);
    testset := [];

    for i in irreps1 do
        for j in irreps2 do
            Append(testset,
                [rec(
                    S := DirectSumMat(i.S, j.S),
                    T := DirectSumMat(i.T, j.T),
                    degree := Length(i.S) + Length(j.S)
                )]
            );
        od;
    od;

    return Set(SortAndRestrict(testset));

end;

#####################
# store

storeResults := function(reps, filename)
    local f;

    f := OutputTextFile(filename, false);
    SetPrintFormattingStatus(f, false);
    PrintTo(f, List(reps));
    CloseStream(f);
end;
