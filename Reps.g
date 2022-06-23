LoadPackage("SL2Reps");
SetInfoLevel(InfoSL2Reps,0);

Reps := function(dim, name)
    local reps, result, tmp;

    reps := SL2IrrepsOfDegree(dim);
    result := [];

    for tmp in reps do
        Append(result, [rec(S := tmp.S, T := tmp.T)]);
    od;

    f := OutputTextFile(name, false);
    SetPrintFormattingStatus(f, false);
    PrintTo(f, List(result));
    CloseStream(f);
end;