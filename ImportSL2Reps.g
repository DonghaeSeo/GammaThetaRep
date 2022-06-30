LoadPackage("SL2Reps");

tmp := SL2IrrepsOfDegree(9);

lev6 := Filtered(tmp, g->g.level = 6);
lev12 := Filtered(tmp, g->g.level = 12);
lev24 := Filtered(tmp, g->g.level = 24);
lev48 := Filtered(tmp, g->g.level = 48);

result := [];
reps := [];

Append(reps, lev6);
Append(reps, lev12);
Append(reps, lev24);
Append(reps, lev48);

for rep in reps do
    Append(result, [rec(
        S := rep.S,
        T := rep.T,
        level := rep.level
    )]);
od;
