-module(first).
-export([double/1 ,area/3, is_zero/1, xor_t/2, fact/1]).
% Multiplies two numbers.
mult(X, Y) -> X * Y.

% Accessible from outside module.
double(X) -> mult(2, X).

area(A, B, C) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

% Atoms constant Values which have a name start with lower case letter with underscore or @ if not use 'somthing here'
% and or -> check both left and right
% andalso / orelse -> short circuit if false andalso then skip left
% tuples {monday, teusay, 15 } -> first field is usually to describe the tuple like a record
% list [monday, teusday], [{x, y z}, {Big, {small, eat}}] [[123, 12412, 45645]]

%fun(X, Y) -> X*Y .

%variables upper case letters

%{rectangle, Pt1, W, H} = {rectangle, {1,2}, 5, 7}

is_zero(0) -> false;
is_zero(_) -> true.

xor_t(true, false) -> true;
xor_t(false, true) -> true;
xor_t(_, _) -> false.

fact(0) -> 1;
fact(N) when is_integer(N), N > 0 -> fact(N-1) *N. 



