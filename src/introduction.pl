
% Welcome to Pce Emacs! We won't be going over a lot of the details of
% editing in Pce but I strongly suggest learning the basics of Emacs in
% order to continue with our tutorial and taking some time to read up on
% using (and customising!) Pce in the meantime. For now you can use
% Control-h Control-b to see basic help and keybindings. Here's a good
% resource for using Pce you may want to utilise
% https://www.metalevel.at/pceprolog/
%
%  * Introduction *
% Prolog is a declarative programming language rooted in classical
% logic. It supports search and unification as built-in features. A
% Prolog program consists of predicates, and each predicate defines a
% relation between its arguments.
%
%  Here is an example of a predicate-- `module`.

:- module(introduction, []).

% module is a predicate which takes an atom (the name of the module),
% and the exported predicates and defines... a module! We will get more
% into modules later, but for now, the structure of a predicate is all
% you need to know.
%
% The next section is found at `facets_of_prolog.pl`. Open this file in
% your editor either from the editor or using a similar method as
% before.
