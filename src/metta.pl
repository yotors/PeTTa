%%%%%%%%%% Dependencies %%%%%%%%%%
library(X, Path) :- library_path(Base), atomic_list_concat([Base, '/', X], Path).
library(X, Y, Path) :- library_path(Base), atomic_list_concat([Base, '/../', X, '/', Y], Path).
:- prolog_load_context(directory, Source),
   directory_file_path(Source, '..', Parent),
   directory_file_path(Parent, 'lib', LibPath),
   asserta(library_path(LibPath)).
:- autoload(library(uuid)).
:- use_module(library(random)).
:- use_module(library(janus)).
:- use_module(library(error)).
:- use_module(library(listing)).
:- use_module(library(aggregate)).
:- use_module(library(thread)).
:- use_module(library(lists)).
:- use_module(library(yall), except([(/)/3])).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- current_prolog_flag(argv, Argv),
   ( member(mork, Argv) -> ensure_loaded([parser, translator, filereader, '../mork_ffi/morkspaces', spaces])
                         ; ensure_loaded([parser, translator, filereader, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Representation conversion: %%%
id(X, X).
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'!='(A,B,R) :- (A==B -> R=false ; R=true).
'='(A,B,R) :-  (A=B -> R=true ; R=false).
'=?'(A,B,R) :- (\+ \+ A=B -> R=true ; R=false).
'=alpha'(A,B,R) :- (A =@= B -> R=true ; R=false).
'=@='(A,B,R) :- (A =@= B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A >= B -> R=true ; R=false).
min(A,B,R)  :- R is min(A,B).
max(A,B,R)  :- R is max(A,B).
exp(Arg,R) :- R is exp(Arg).
:- use_module(library(clpfd)).
'#+'(A, B, R) :- R #= A + B.
'#-'(A, B, R) :- R #= A - B.
'#*'(A, B, R) :- R #= A * B.
'#div'(A, B, R) :- R #= A div B.
'#//'(A, B, R) :- R #= A // B.
'#mod'(A, B, R) :- R #= A mod B.
'#min'(A, B, R) :- R #= min(A,B).
'#max'(A, B, R) :- R #= max(A,B).
'#<'(A, B, true)  :- A #< B, !.
'#<'(_, _, false).
'#>'(A, B, true)  :- A #> B, !.
'#>'(_, _, false).
'#='(A, B, true)  :- A #= B, !.
'#='(_, _, false).
'#\\='(A, B, true)  :- A #\= B, !.
'#\\='(_, _, false).
'pow-math'(A, B, Out) :- Out is A ** B.
'sqrt-math'(A, Out)   :- Out is sqrt(A).
'abs-math'(A, Out)    :- Out is abs(A).
'log-math'(Base, X, Out) :- Out is log(X) / log(Base).
'trunc-math'(A, Out)  :- Out is truncate(A).
'ceil-math'(A, Out)   :- Out is ceil(A).
'floor-math'(A, Out)  :- Out is floor(A).
'round-math'(A, Out)  :- Out is round(A).
'sin-math'(A, Out)  :- Out is sin(A).
'cos-math'(A, Out)  :- Out is cos(A).
'tan-math'(A, Out)  :- Out is tan(A).
'asin-math'(A, Out) :- Out is asin(A).
'acos-math'(A, Out) :- Out is acos(A).
'atan-math'(A, Out) :- Out is atan(A).
'isnan-math'(A, Out) :- ( A =:= A -> Out = false ; Out = true ).
'isinf-math'(A, Out) :- ( A =:= 1.0Inf ; A =:= -1.0Inf -> Out = true ; Out = false ).
'min-atom'(List, Out) :- min_list(List, Out).
'max-atom'(List, Out) :- max_list(List, Out).

%%% Random Generators: %%%
'random-int'(Min, Max, Result) :- random_between(Min, Max, Result).
'random-int'('&rng', Min, Max, Result) :- random_between(Min, Max, Result).
'random-float'(Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).
'random-float'('&rng', Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).

%%% Boolean Logic: %%%
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).
xor(false, A, A).
xor(true, A, B) :- not(A, B).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'cons-atom'(H, T, [H|T]).
'decons-atom'([H|T], [H|[T]]).
'first-from-pair'([A, _], A).
first([A, _], A).
'second-from-pair'([_, A], A).
'unique-atom'(A, B) :- list_to_set(A, B).
'sort-atom'(List, Sorted) :- msort(List, Sorted).
'size-atom'(List, Size) :- length(List, Size).
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
decons([H|T], [H|[T]]).
cons(H, T, [H|T]).
'index-atom'(List, Index, Elem) :- nth0(Index, List, Elem).
member(X, L, _) :- member(X, L).
'is-member'(X, List, true) :- member(X, List).
'is-member'(X, List, false) :- \+ member(X, List).
'exclude-item'(A, L, R) :- exclude(==(A), L, R).

%Multisets:
'subtraction-atom'([], _, []).
'subtraction-atom'([H|T], B, Out) :- ( select(H, B, BRest) -> 'subtraction-atom'(T, BRest, Out)
                                                            ; Out = [H|Rest],
                                                              'subtraction-atom'(T, B, Rest) ).
'union-atom'(A, B, Out) :- append(A, B, Out).
'intersection-atom'(A, B, Out) :- intersection(A, B, Out).

%%% Type system: %%%
get_function_type([F|Args], T) :- nonvar(F), match('&self', [':',F,[->|Ts]], _, _),
                                 append(As,[T],Ts),
                                 maplist('get-type',Args,As).

'get-type'(X, T) :- (get_type_candidate(X, T) *-> true ; T = '%Undefined%' ).

get_type_candidate(X, 'Number')   :- number(X), !.
get_type_candidate(X, '%Undefined%') :- var(X), !.
get_type_candidate(X, 'String')   :- string(X), !.
get_type_candidate(true, 'Bool')  :- !.
get_type_candidate(false, 'Bool') :- !.
get_type_candidate(X, T) :- get_function_type(X,T).
get_type_candidate(X, T) :- \+ get_function_type(X, _),
                            is_list(X),
                            maplist('get-type', X, T).
get_type_candidate(X, T) :- match('&self', [':',X,T], T, _).

'get-metatype'(X, 'Variable') :- var(X), !.
'get-metatype'(X, 'Grounded') :- number(X), !.
'get-metatype'(X, 'Grounded') :- string(X), !.
'get-metatype'(true,  'Grounded') :- !.
'get-metatype'(false, 'Grounded') :- !.
'get-metatype'(X, 'Grounded') :- atom(X), fun(X), !.  % e.g., '+' is a registered fun/1
'get-metatype'(X, 'Expression') :- is_list(X), !.     % e.g., (+ 1 2), (a b)
'get-metatype'(X, 'Symbol') :- atom(X), !.            % e.g., a

'is-var'(A,R) :- var(A) -> R=true ; R=false.
'is-expr'(A,R) :- is_list(A) -> R=true ; R=false.
'is-space'(A,R) :- atom(A), atom_concat('&', _, A) -> R=true ; R=false.

%%% Diagnostics / Testing: %%%
'println!'(Arg, true) :- swrite(Arg, RArg),
                         format('~w~n', [RArg]).

'readln!'(Out) :- read_line_to_string(user_input, Str),
                  sread(Str, Out).

'trace!'(In, Content, Content) :- swrite(In,R),
                                  format('~w~n', [R]).

test(A,B,true) :- (A =@= B -> E = '✅' ; E = '❌'),
                  swrite(A, RA),
                  swrite(B, RB),
                  format("is ~w, should ~w. ~w ~n", [RA, RB, E]),
                  (A =@= B -> true ; halt(1)).

assert(Goal, true) :- ( call(Goal) -> true
                                    ; swrite(Goal, RG),
                                      format("Assertion failed: ~w~n", [RG]),
                                      halt(1) ).

%%% Time Retrieval: %%%
'current-time'(Time) :- get_time(Time).
'format-time'(Format, TimeString) :- get_time(Time), format_time(atom(TimeString), Format, Time).

%%% Python bindings: %%%
'py-call'(SpecList, Result) :- 'py-call'(SpecList, Result, []).
'py-call'([Spec|Args], Result, Opts) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')         % ".method"
                                          -> sub_atom(A, 1, _, 0, Fun),
                                             Args = [Obj|Rest],
                                             ( Rest == []
                                               -> compound_name_arguments(Meth, Fun, [])
                                                ; Meth =.. [Fun|Rest] ),
                                             py_call(Obj:Meth, Result, Opts)
                                           ; atomic_list_concat([M,F], '.', A) % "mod.fun"
                                             -> ( Args == []
                                                  -> compound_name_arguments(Call0, F, [])
                                                   ; Call0 =.. [F|Args] ),
                                                py_call(M:Call0, Result, Opts)
                                              ; ( Args == []                      % bare "fun"
                                                  -> compound_name_arguments(Call0, A, [])
                                                   ; Call0 =.. [A|Args] ),
                                                py_call(builtins:Call0, Result, Opts) ).

%%% States: %%%
'bind!'(A, ['new-state', B], C) :- 'change-state!'(A, B, C).
'change-state!'(Var, Value, true) :- nb_setval(Var, Value).
'get-state'(Var, Value) :- nb_getval(Var, Value).

%%% Eval: %%%
eval(C, Out) :- translate_expr(C, Goals, Out),
                call_goals(Goals).

call_goals([]).
call_goals([G|Gs]) :- call(G), 
                      call_goals(Gs).

%%% Higher-Order Functions: %%%
'foldl-atom'([], Acc, _Func, Acc).
'foldl-atom'([H|T], Acc0, Func, Out) :- reduce([Func,Acc0,H], Acc1),
                                        'foldl-atom'(T, Acc1, Func, Out).

'map-atom'([], _Func, []).
'map-atom'([H|T], Func, [R|RT]) :- reduce([Func,H], R),
                                   'map-atom'(T, Func, RT).

'filter-atom'([], _Func, []).
'filter-atom'([H|T], Func, Out) :- ( reduce([Func,H], true) -> Out = [H|RT]
                                                             ; Out = RT ),
                                   'filter-atom'(T, Func, RT).

%%% Prolog interop: %%%
import_prolog_function(N, true) :- register_fun(N).
'Predicate'([F|Args], Term) :- Term =.. [F|Args].
callPredicate(G, true) :- call(G).
assertzPredicate(G, true) :- assertz(G).
assertaPredicate(G, true) :- asserta(G).
retractPredicate(G, true) :- retract(G), !.
retractPredicate(_, false).

%%% Library / Import: %%%
ensure_metta_ext(Path, Path) :- file_name_extension(_, metta, Path), !.
ensure_metta_ext(Path, PathWithExt) :- file_name_extension(Path, metta, PathWithExt).

'import!'(Space, File, true) :- atom_string(File, SFile),
                                working_dir(Base),
                                ( file_name_extension(ModPath, 'py', SFile)
                                  -> absolute_file_name(SFile, Path, [relative_to(Base)]),
                                     file_directory_name(Path, Dir),
                                     file_base_name(ModPath, ModuleName),
                                     py_call(sys:path:append(Dir), _),
                                     py_call(builtins:'__import__'(ModuleName), _)
                                   ; ( Path = SFile ; atomic_list_concat([Base, '/', SFile], Path) ),
                                     ensure_metta_ext(Path, PathWithExt),
                                     exists_file(PathWithExt), !,
                                     load_metta_file(PathWithExt, _, Space) ).

%%% Registration: %%%
:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
unregister_fun(N/Arity) :- retractall(fun(N)),
                           abolish(N, Arity).

:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max, 'change-state!', 'get-state', 'bind!',
                          '<','>','==', '!=', '=', '=?', '<=', '>=', and, or, xor, not, sqrt, exp, log, cos, sin,
                          'first-from-pair', 'second-from-pair', 'car-atom', 'cdr-atom', 'unique-atom',
                          repr, repra, 'println!', 'readln!', 'trace!', test, assert, 'mm2-exec', atom_concat, atom_chars, copy_term, term_hash,
                          foldl, first, last, append, length, 'size-atom', sort, msort, member, 'is-member', 'exclude-item', list_to_set, maplist, eval, reduce, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'is-space', 'get-mettatype',
                          decons, 'decons-atom', 'py-call', 'get-type', 'get-metatype', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id,
                          'pow-math', 'sqrt-math', 'sort-atom','abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math','random-int','random-float',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom',
                          'foldl-atom', 'map-atom', 'filter-atom','current-time','format-time', library, exists_file,
                          import_prolog_function, 'Predicate', callPredicate, assertaPredicate, assertzPredicate, retractPredicate,
                          'cut-first-char']).

'cut-first-char'(Atom, Result) :-
    atom(Atom),
    sub_atom(Atom, 1, _, 0, Result).

% ============================================================
% Unique Combinations Star Implementation
% ============================================================

:- use_module(library(ordsets)).
:- use_module(library(assoc)).

unique_combinations_star(List, Size, Result) :-
    (number(Size) -> K = Size ; atom_number(Size, K)),
    (K > 0 ->
        variabilize_term(List, VariabilizedList),
        maplist(extract_expr_info, VariabilizedList, Infos),
        build_inverted_index(Infos, InvIndex),
        include(is_hub_candidate(K), InvIndex, HubCandidates),
        maplist(generate_combos_for_hub(K, Infos), HubCandidates, NestedCombos),
        append(NestedCombos, FlatCombos),
        sort(FlatCombos, UniqueCombos),
        remove_alpha_duplicates(UniqueCombos, AlphaUniqueCombos),
        maplist(format_combo_output, AlphaUniqueCombos, Conjuncts),
        Result = Conjuncts
    ;
        Result = []
    ).

remove_alpha_duplicates([], []).
remove_alpha_duplicates([H|T], [H|Result]) :-
    exclude(=@=(H), T, Remaining),
    remove_alpha_duplicates(Remaining, Result).

variabilize_term(Term, VarTerm) :-
    empty_assoc(Map),
    variabilize_term(Term, Map, _, VarTerm).

variabilize_term(Var, Map, Map, Var) :- var(Var), !.
variabilize_term(Atom, Map, MapOut, Var) :-
    atom(Atom),
    atom_chars(Atom, ['$'|_]), !,
    ( get_assoc(Atom, Map, Var) ->
        MapOut = Map
    ;
        put_assoc(Atom, Map, Var, MapOut)
    ).
variabilize_term([H|T], Map, MapOut, [HVar|TVar]) :- !,
    variabilize_term(H, Map, MapMid, HVar),
    variabilize_term(T, MapMid, MapOut, TVar).
variabilize_term(Term, Map, MapOut, NewTerm) :-
    compound(Term), !,
    Term =.. [F|Args],
    variabilize_list_args(Args, Map, MapOut, NewArgs),
    NewTerm =.. [F|NewArgs].
variabilize_term(Term, Map, Map, Term).

variabilize_list_args([], Map, Map, []).
variabilize_list_args([H|T], Map, MapOut, [HVar|TVar]) :-
    variabilize_term(H, Map, MapMid, HVar),
    variabilize_list_args(T, MapMid, MapOut, TVar).

extract_expr_info(Expr, info(Expr, Vars, Functor)) :-
    get_all_vars(Expr, Vars),
    get_functor_safe(Expr, Functor).

get_all_vars(Term, Vars) :-
    term_variables(Term, PrologVars),
    get_atom_vars(Term, AtomVars),
    append(PrologVars, AtomVars, AllVars),
    sort(AllVars, Vars).

get_atom_vars(Term, Vars) :-
    get_atom_vars_acc(Term, [], Vars).

get_atom_vars_acc(Var, Acc, Acc) :- var(Var), !.
get_atom_vars_acc(Atom, Acc, [Atom|Acc]) :-
    atom(Atom),
    atom_chars(Atom, ['$'|_]), !.
get_atom_vars_acc(List, Acc, Result) :-
    is_list(List), !,
    foldl(get_atom_vars_acc, List, Acc, Result).
get_atom_vars_acc(Compound, Acc, Result) :-
    compound(Compound), !,
    Compound =.. [_|Args],
    foldl(get_atom_vars_acc, Args, Acc, Result).
get_atom_vars_acc(_, Acc, Acc).

get_functor_safe([F|_], F) :- !.
get_functor_safe(Term, F) :- compound(Term), functor(Term, F, _), !.
get_functor_safe(Atom, Atom) :- atom(Atom).
get_functor_safe(_, '').

build_inverted_index(Infos, InvIndex) :-
    build_inverted_index(Infos, 0, [], InvIndex).

build_inverted_index([], _, Acc, Acc).
build_inverted_index([info(_, Vars, _)|Rest], Idx, Acc, Result) :-
    update_index(Vars, Idx, Acc, NextAcc),
    NextIdx is Idx + 1,
    build_inverted_index(Rest, NextIdx, NextAcc, Result).

update_index([], _, Acc, Acc).
update_index([Var|Rest], Idx, Acc, Result) :-
    update_var_entry(Var, Idx, Acc, NewAcc),
    update_index(Rest, Idx, NewAcc, Result).

update_var_entry(Var, Idx, [], [entry(Var, [Idx])]).
update_var_entry(Var, Idx, [entry(V, Idxs)|Rest], [entry(V, [Idx|Idxs])|Rest]) :-
    Var == V, !.
update_var_entry(Var, Idx, [Entry|Rest], [Entry|NewRest]) :-
    update_var_entry(Var, Idx, Rest, NewRest).

is_hub_candidate(K, entry(_, Indices)) :-
    length(Indices, Len),
    Len >= K.

generate_combos_for_hub(K, Infos, entry(HubVar, Indices), Combos) :-
    maplist(get_pool_item(Infos, HubVar), Indices, Pool),
    find_combos(Pool, K, [], [], [], Combos).

get_pool_item(Infos, HubVar, Index, item(Term, NonHubVars, Functor)) :-
    nth0(Index, Infos, info(Term, Vars, Functor)),
    select_exact(HubVar, Vars, NonHubVars).

select_exact(X, [Y|Ys], Ys) :- X == Y, !.
select_exact(X, [Y|Ys], [Y|Zs]) :- select_exact(X, Ys, Zs).

find_combos(_, 0, Combo, _, _, [Combo]) :- !.
find_combos([], _, _, _, _, []) :- !.
find_combos([item(Term, NonHubVars, Functor)|Rest], K, CurrentCombo, UsedVars, UsedFunctors, Result) :-
    (   (Functor \= '' -> \+ member(Functor, UsedFunctors) ; true),
        check_disjoint(NonHubVars, UsedVars)
    ->  K1 is K - 1,
        append(UsedVars, NonHubVars, NewUsedVars),
        (Functor \= '' -> NewUsedFunctors = [Functor|UsedFunctors] ; NewUsedFunctors = UsedFunctors),
        find_combos(Rest, K1, [Term|CurrentCombo], NewUsedVars, NewUsedFunctors, Res1)
    ;   Res1 = []
    ),
    find_combos(Rest, K, CurrentCombo, UsedVars, UsedFunctors, Res2),
    append(Res1, Res2, Result).

check_disjoint([], _).
check_disjoint([X|Xs], Ys) :-
    \+ member_exact(X, Ys),
    check_disjoint(Xs, Ys).

member_exact(X, [Y|_]) :- X == Y, !.
member_exact(X, [_|Ys]) :- member_exact(X, Ys).

format_combo_output(Combo, [conjunct, [',' | Combo]]).

:- register_fun(unique_combinations_star).

% ============================================================
% Sort Conjunction Implementation
% ============================================================

:- use_module(library(varnumbers)).
:- register_fun(sort_conj).

sort_conj(List, SortedList) :-
    copy_term(List, Copy),
    numbervars(Copy, 0, _),
    msort(Copy, Sorted),
    varnumbers(Sorted, SortedList).


