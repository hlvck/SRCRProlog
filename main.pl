% SRCR - Instrumento de Avaliacao Individual

% Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

:- dynamic adjacencia/3.
:- dynamic ponto/7.

:- include('adjacencias.pl').
:- include('pontos.pl').

%------------ Depth-First Search

dfs(Nodo, Destino, [Nodo|Caminho]):-
    dfsa(Nodo, Destino, [Nodo], Caminho).

dfsa(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsa(Nodo, Destino, Visitado, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visitado),
    dfsa(ProxNodo, Destino, [Nodo|Visitado], Caminho).

adjacente(Ponto, ProxPonto):- adjacencia(Ponto, ProxPonto, _).
adjacente(Ponto, ProxPonto):- adjacencia(ProxPonto, Ponto, _).

