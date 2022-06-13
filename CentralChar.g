CentralChar := function(reps)
local i, T00, S00, cc, result;

result := [];

for i in [1..Length(reps)] do
    S00 := reps[i].S[1][1];
    T00 := reps[i].T[1][1];
    Append(result, [rec(
        S := reps[i].S / S00,
        T := (reps[i].T / T00)^2,
        degree := reps[i].degree
    )]);
od;

return Set(result);

end;


