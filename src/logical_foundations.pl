
:- module(logical_foundations, []).

% Logic, being primarily concerned with language, describes the rules of
% relations between:
%
% Syntax: The formalised structure of language
% Semantics: The rules for interpreting linguistic structures
% Inferences: The information which is derived on the basis of carrying
% forward premises
%
% Prolog is based on classical first-order logic. Programs are executed
% in a manner similar to theorem provers. When Prolog answers a query,
% it binds variables to arguments and narrow down the set of possible
% results by means of proof-by-contradiction.
%
% The exact method differs and isn't set, and the execution strategy can
% be changed, the default strategy being known as SLDNF resolution,
% under which a search tree is implicitly defined, where every node
% contains a goal clause, and the root node is the original goal, and
% the resolver attempts to resolve every leaf node down to an empty goal
% clause (think the empty body example a la facets_of_prolog.pl -- AKA
% it is simply stated to be true as an axiom).
%
% Later, when we discuss meta-interpreters, we will show how Prolog can
% be used to transformed into a complete theorem prover.
%
% Please continue to basic_concepts.pl




