LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");

##############
# arguments
dim := 6;
level := 32;
##############

result := [];

testset := Filtered(SL2IrrepsOfDegree(dim),g->g.level=level);

for i in [1..Length(testset)] do
    sorted_testset := SortFun(testset[i].S, testset[i].T);
    if sorted_testset = false then
        continue;
    else
        len := Length(sorted_testset);

        for j in [1..len] do
            res := Restrict(sorted_testset[j].S, sorted_testset[j].T, level);
            if res = false then continue;
            else Append(result, [res]);
            fi;
        od;
    fi;
od;

Print(Set(result));





