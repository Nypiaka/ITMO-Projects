init(MAX_N):-
    loop_erat(2, MAX_N, 4),!.

not_primes(1).

loop_erat(F, MAX_N, S):-
    F < MAX_N, \+(not_primes(F)), S =< MAX_N, \+(not_primes(S)), assert(not_primes(S)),!,SP is S+F,loop_erat(F, MAX_N, SP), !.

loop_erat(F,MAX_N,S):-
    F < MAX_N, \+(not_primes(F)), S =< MAX_N, not_primes(S), !, SP is S + F,loop_erat(F, MAX_N, SP), !.

loop_erat(F,MAX_N,S):-
    F < MAX_N, \+(not_primes(F)), S > MAX_N, T is F + 1, D is T * T, loop_erat(T, MAX_N, D), !.

loop_erat(F,MAX_N,S):-
    F < MAX_N, not_primes(F), F1 is F+1, D is F1 * F1, loop_erat(F1, MAX_N, D), !.

prime(N):-
    \+(not_primes(N)).

composite(N):-
    not_primes(N).

list_length([],0).

list_length([_|Xs],L) :- list_length(Xs, N), L is N+1.

prime_divisors(N,[A|B]):-
    \+(not_primes(N)), list_length([A|B], LN), LN is 1, A is N,!.

prime_divisors(N,L):-
   not_primes(N), loop_check(N, L), !.

loop_check(N,[A|B]):-
    \+(1 is N), find_first_divisor(N,2,FD), A is FD, N1 is N/FD, loop_check(N1, B), !.

loop_check(N,B):-
    list_length(B, L), 0 is L, 1 is N, !.

find_first_divisor(N, D, B):-
    0 is N mod D, B is D, !.

find_first_divisor(N, D, B):-
    \+(0 is N mod D), D1 is D + 1 , find_first_divisor(N, D1, B), !.

unique_prime_divisors(N, [A|B]):-
	\+(not_primes(N)), !, list_length([A|B], LN), LN is 1, A is N, !.

unique_prime_divisors(N, [A|B]):-
	\+(1 is N), find_first_divisor(N, 2, FD), A is FD, divide_while_divides(N, FD, N1), unique_prime_divisors(N1, B), !.

unique_prime_divisors(N, L):-
	1 is N, list_length(L, LN), LN is 0, !.

divide_while_divides(N, FD, N2):-
	0 is N mod FD, N1 is N / FD, divide_while_divides(N1, FD, N2).

divide_while_divides(N, FD, N2):-
	\+(0 is N mod FD), N2 is N.