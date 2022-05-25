LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");

##############
# arguments
dim := 6;
level := 8;
##############

testset := Filtered(SL2IrrepsOfDegree(dim),g->g.level=level);

for i in [1..Length(testset)] do
    sorted_testset := SortFun(testset[i].S, testset[i].T);
    if sorted_testset = false then
        Print("false\n");
    else
        len := Length(sorted_testset);

        for j in [1..len] do
            Print(Restrict(sorted_testset[j].S,sorted_testset[j].T),"\n");
        od;
    fi;
od;







