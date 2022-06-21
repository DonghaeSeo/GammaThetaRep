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

    return SortAndRestrict(testset);

end;

GammaThetaRepsByReducibles := function()
    local irreps, testset, i, j;

    irreps := SL2IrrepsOfDegree(3);
    testset := [];

    for i in irreps do
        for j in irreps do
            Append(testset,
                [rec(
                    S := BlockMatrix([[1,1,i.S],[2,2,j.S]],2,2),
                    T := BlockMatrix([[1,1,i.T],[2,2,j.T]],2,2),
                    degree := Length(i.S) + Length(j.S)
                )]
            );
        od;
    od;

    return SortAndRestrict(testset);

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
