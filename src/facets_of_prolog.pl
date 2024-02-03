:- module(facets_of_prolog, [list_length/2, call_example/1]).
% This is how you use a module. In this case we are using clpfd to make reasoning about integers easier. https://www.swi-prolog.org/man/clpfd.html without it, reasoning about integers becomes a lot harder. We'll get into reasoning over finite domains more later!
:- use_module(library(clpfd)).

%   * Prolog is a very simple language. *

% All data is represented by Prolog
% terms. There is a single building block -- a clause (or horn clause).
% The clause is of the form
%
% Head :- Body.
%
% Which says that if Body holds then Head holds. The operator `:-`
% represents a leftwards arrow in classical logic (Head if and only if
% Body).
%
% One may also see the following
%
% Head.
%
% Which is shorthand to say that Head always holds (the body is empty).



%   * Prolog is a declarative language*
%
% The above may be unintuitive to those who are new to declarative
% programming. In Prolog, we express what holds about solutions we want
% to find, and the compiler utilises unification and search to give us
% the conditions under which what we have expressed is true. This is the
% opposite of procedural, functional, and imperative languages, where
% one expresses how the program should actually reach a solution.
%
% Here is an example:
%

% The length of the empty list [] is 0
list_length([], 0).

% N is the length of the list [_|Ls] (this is an example of
% destructuring-- Ls is the list [_|Ls] without its first value) if
list_length([_|Ls], N) :-
    /* N is greater than 0 */
    N #> 0,
    /* N is equal to N0 + 1 (We are defining an intermediate value N0) */
    N #= N0 + 1,
    /* The length of Ls is N0 */
    list_length(Ls, N0).

% Complete the following exercises: In your running `swipl` command
% line, run `[src/facets_of_prolog].` This compiles (or 'consults') the
% current file. Now run the following predicates:

% What happens if we call list_length([1,2,3], N). ?
% What happens if we call list_length(Ls, 3). ?
% What happens if we call list_length(Ls, L). ?
%
% Press enter after each time you get a return value.

% Now let's try something else... Run the final exercise again, but this
% time instead of hitting enter immediately, input a semi-colon ; and
% then hit enter. You will see a really cool feature of Prolog: its
% non-determinism. Prolog will report multiple answers back! It does
% this by utilising backtracking-- it will return to a previous point in
% the compiled program and try different values to see if it can get
% multiple solutions to a problem, and then returns them back! Wow!!
%
%   * Prolog is a logic programming language *
%
% As we've discussed, the basic building blocks of the Prolog language
% are Horn Clauses https://en.wikipedia.org/wiki/Horn_clause
% Execution in Prolog is a special case of resolution https://en.wikipedia.org/wiki/Resolution_(logic)
% Since it uses resolution, adding constraints can never add new
% possible solutions, it can only reduce the set of solutions. Adding a
% clause can at most extend it, likewise. This property is called
% monotonicity https://en.wikipedia.org/wiki/Monotonicity_of_entailment
% and allows us to have powerful declarative debugging techniques (we'll
% get to this in a later chapter).
%
%
%   * Prolog is a homoiconic language *
%
% For those who are familiar with Lisp, you will understand this concept
% already. Prolog programs are also valid Prolog terms-- this is to say,
% everything is just data. This means that you can create Prolog
% programs that analyse, transform, and interpret other prolog programs.
%
% As an example, consult this file and then call `call_example(X).` You
% should receive X = 3 back.
%
% This predicate says 'call the predicate plus, with the arguments 1, 2,
% and X. In essence, you could rewrite this predicate as call_example :-
% plus(1, 2, X).
%
call_example(X) :- call(plus, 1, 2, X).

% As an exercise, try to find more examples that leverage Prolog's
% homoiconicity!


%   * Prolog is a very dynamic language *
%
% Prolog programs can be created, called, and modified at runtime. This
% lets you implement higher-order predicates and the implementation of
% dynamic techniques. This means it's a great language for writing
% extensible programs! There is a famous example of the meta-interpreter
% in Prolog, which can take as little as two lines to write!
% https://www.metalevel.at/acomip/

%   * Prolog is a very versatile language *
%
% Since Prolog is relational, Prolog can be used for all kinds of
% use-cases, and can be used to build huge applications, as we'll see.

% Please refer to the next chapter, which can be found at
% `logical_foundations.pl`

