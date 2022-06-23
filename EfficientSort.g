EfficientSort := function(S, T, n1, n2)
    local dim, diag, d1, d2, p1, p2, perm, i, j, tmp, filtered_perm, permed_T, len, loop_index, r, m, p, result;

    dim := n1 + n2;

    d1 := DiagonalOfMat(T{[1..n1]}{[1..n1]});
    d2 := DiagonalOfMat(T{[n1+1..n1+n2]}{[n1+1..n1+n2]});
    diag := DiagonalMat(T);

    p1 := PermutationsList(d1);
    p2 := PermutationsList(d2);

    perm := [];
    for i in p1 do
        for j in p2 do
            tmp := [];
            Append(tmp, [i]);
            Append(tmp, [j]);
            Append(perm, [[tmp]]);
        od;
    od;

    filtered_perm := Filtered( perm, g -> Set( g{[1..dim/3]} + g{[dim/3+1..2*dim/3]} ) = [0] 
       and IsSortedList( g{[2*dim/3+1..dim]} ) );
    permed_T := [];

    if filtered_perm = [] then
       return false;
    fi;

    len := Length( filtered_perm );

    for loop_index in [1..len] do
       Append( permed_T, [DiagonalMat( filtered_perm[loop_index] )] );
    od;

    r := [1..dim];
    m := NullMat( dim, dim );

    result := [];

    for loop_index in [1..len] do
       p := Permuted( r, PermListList( filtered_perm[loop_index], diag ) );
       for i in [1..dim] do
          for j in [1..dim] do
             m[i][j] := S[p[i]][p[j]];
          od;
       od;
       Append( result, [rec( S := m, T := permed_T[loop_index] )] );
    od;

    return Set( result );

end;