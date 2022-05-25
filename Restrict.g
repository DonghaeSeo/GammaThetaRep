Restrict := function( T, S )
local dim, t, id, U, USU, s;

dim := Length( T );
t := T{[dim/3+1..2*dim/3]}{[dim/3+1..2*dim/3]};
id := IdentityMat( dim/3 );
U := BlockMatrix([[1,1,-t],[1,2,t],[2,1,id],[2,2,id],[3,3,id]],3,3);
USU := U * S * Inverse( U );

if USU{[dim/3+1..2*dim/3]}{[1..dim/3]} = NullMat( dim/3, dim/3 )
and USU{[dim/3+1..2*dim/3]}{[dim/3+1..2*dim/3]} = NullMat( dim/3, dim/3 )
and USU{[1..dim/3]}{[dim/3+1..2*dim/3]} = NullMat( dim/3, dim/3 ) then
    s := USU{[1..dim/3]}{[1..dim/3]};
else 
    return false;
fi;

return rec( S := s, T := t );

end;