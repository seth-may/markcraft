%% Prolog: Expert system for programming language recommendation
%% Consult and query: ?- recommend(Lang), explain(Lang).

:- dynamic user_need/1, user_exp/1.

%% --- Knowledge base: language properties ---
language(python).    language(rust).      language(go).
language(swift).     language(typescript). language(haskell).
language(elixir).    language(kotlin).    language(julia).
language(zig).

paradigm(python, multi).     paradigm(rust, systems).
paradigm(go, concurrent).    paradigm(swift, multi).
paradigm(typescript, multi). paradigm(haskell, functional).
paradigm(elixir, functional).paradigm(kotlin, multi).
paradigm(julia, scientific). paradigm(zig, systems).

typing(python, dynamic).   typing(rust, static).
typing(go, static).        typing(swift, static).
typing(typescript, static).typing(haskell, static).
typing(elixir, dynamic).   typing(kotlin, static).
typing(julia, dynamic).    typing(zig, static).

good_for(python, [web, ai, scripting, data]).
good_for(rust, [systems, performance, safety, wasm]).
good_for(go, [backend, devops, microservices, cli]).
good_for(swift, [ios, macos, mobile, server]).
good_for(typescript, [web, frontend, fullstack, node]).
good_for(haskell, [compilers, math, research, dsl]).
good_for(elixir, [realtime, distributed, web, telecom]).
good_for(kotlin, [android, backend, multiplatform]).
good_for(julia, [scientific, math, data, hpc]).
good_for(zig, [systems, embedded, performance, gamedev]).

learning_curve(python, easy).   learning_curve(rust, hard).
learning_curve(go, easy).       learning_curve(swift, medium).
learning_curve(typescript, easy). learning_curve(haskell, hard).
learning_curve(elixir, medium). learning_curve(kotlin, medium).
learning_curve(julia, medium).  learning_curve(zig, hard).

%% --- Inference rules ---
matches_domain(Lang, Domain) :-
    good_for(Lang, Domains),
    member(Domain, Domains).

suitable(Lang) :-
    language(Lang),
    user_need(Domain),
    matches_domain(Lang, Domain).

%% Prefer easy languages for beginners
prefer_easy(Lang) :-
    user_exp(beginner),
    learning_curve(Lang, easy).
prefer_easy(Lang) :-
    user_exp(intermediate),
    learning_curve(Lang, Curve),
    member(Curve, [easy, medium]).
prefer_easy(_Lang) :-
    user_exp(expert).

%% Score a language (higher = better match)
score(Lang, Score) :-
    suitable(Lang),
    prefer_easy(Lang),
    findall(D, (user_need(D), matches_domain(Lang, D)), Matches),
    length(Matches, Score),
    Score > 0.

%% Main recommendation
recommend(Lang) :-
    findall(S-L, score(L, S), Pairs),
    sort(0, @>=, Pairs, Sorted),
    member(_-Lang, Sorted).

explain(Lang) :-
    language(Lang),
    paradigm(Lang, P),
    typing(Lang, T),
    learning_curve(Lang, LC),
    good_for(Lang, Domains),
    format("~n=== ~w ===~n", [Lang]),
    format("  Paradigm: ~w~n  Typing: ~w~n", [P, T]),
    format("  Difficulty: ~w~n  Domains: ~w~n", [LC, Domains]).

%% --- Interactive session ---
ask_needs :-
    retractall(user_need(_)),
    retractall(user_exp(_)),
    write('Experience (beginner/intermediate/expert)? '),
    read(Exp), assert(user_exp(Exp)),
    write('Domain (web/ai/systems/mobile/data/devops)? '),
    read(Dom), assert(user_need(Dom)).

run :-
    ask_needs,
    write('--- Recommendations ---'), nl,
    forall(recommend(L), explain(L)).

%% Quick test: assert needs, then query
:- assert(user_need(web)),
   assert(user_need(ai)),
   assert(user_exp(intermediate)).
