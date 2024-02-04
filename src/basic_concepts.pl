
:- module(basic_concepts, [is_3/1, is_3_or_5/1, fizzbuzz/1, factorial/2, hailstone/2]).
:- use_module(library(clpfd)).


% A Prolog program is a set of predicates. Predicates define relations
% between their arguments. Each predicate has a name, and zero or more
% arguments.

% The name of a predicate is an 'atom' that is to say it is
% a singular data item. There is also a built-in predicate `atom` which
% can tell you what is and is not atomic. As an exercise, give it a go
% on various arguments! Does it yield the results you expect?
%
% A predicate Pred with N args is written Pred/N. This is called a
% predicate indicator. N is called the arity of the predicate. You can
% see an example in the module exports using this predicate which simply
% asserts that its only argument X is equal to 3

is_3(X) :- X =:= 3.

% A predicate is defined by a collection of clauses. A clause is either
% a rule or a fact. Here is an example of a predicate with multiple
% clauses.
%

is_3_or_5(X) :- X =:= 3.
is_3_or_5(X) :- X =:= 5.

% You may have seen something a bit similar in Haskell or Erlang (no
% surprise there, since Erlang itself emerged from Prolog). Each rule
% here offers an alternative solution. In essence, it works like the
% logical Or. is_3_or_5 holds if either X is 3, or X is 5. Give it a
% whirl!
%

% Each rule can take multiple goal clauses, too, separated by commas.
% Here's a simple example you can try.

fizzbuzz(X) :-
    X mod 3 =:= 0,
    X mod 5 =:= 0.

% Rules can also be recursive. Also you can use constants as arguments!
% Try executing the following: factorial(X, Y). Remember that you can
% use semi-colons to generate new solutions!

% Base case
factorial(1, 1).

% Recursive case
factorial(X, Y) :-
    X #> 0,
    X1 #= X-1,
    factorial(X1, Y1),
    Y #= Y1*X.


%   * Example: The Collatz Conjecture *

% Let's see another example. There exists a hailstone sequence which
% takes integer N0 and generates N1, N2, and so on by the following
% rules. If N is even, divide by 2. If N is odd, multiply by 3 and add
% 1, obtaining 3N + 1.
%
% The Collatz conjecture states that the integer 1 appears in this
% sequence for all positive integers.

% We can model the Hailstone sequence as follows:
%

hailstone(N, N).

hailstone(N0, N) :-
    N0 #= 2 * N1,
    hailstone(N1, N).

hailstone(N0, N) :-
    N0 #= 2 * _ + 1,
    N1 #= 3*N0 + 1,
    hailstone(N1, N).

% Let's start from the bottom. The last two rules clearly correspond to
% the odd-case and even-cases. It works because #= is an integer
% constraint -- which means our interpreter already knows it's looking
% only for integers. Also as an aside, _ is necessary if we don't want
% to use the result, or the compiler will complain. Change it to an
% upper-case letter and see what I mean.
%
% But there are a couple factors that may cause confusion here. Why do
% we have the first rule, and why are there two arguments when our
% sequence only requires knowledge about one value? This is because we
% actually want to return our values. Using two arguments means that we
% can input a start integer and generate new values.

% Try giving this a whirl with hailstone(X, Y) and watch the
% backtracking. Our query has been modelled as a relation between
% successive states!

% As a final exercise to the reader, try to write a more efficient
% implementation of hailstone -- hint: Use `if_/3`.
%
% I'll see you in data_structures.pl!

