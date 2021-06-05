% SRCR - Instrumento de Avaliacao Individual

% Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

%--- adjacencia(pontoID1, pontoID2, distancia).
%--- ponto(pontoID, Longitude, Latitude, Freguesia, Rua, [TipoContentores], CapacidadeTotal).
:- dynamic adjacencia/3.
:- dynamic ponto/7.

:- include('adjacencias.pl').
:- include('pontos.pl').

%----------------------- ESTRATEGIAS DE PROCURA ------------------------------
%--------------------- Pesquisa Nao Informada --------------------------------
%------------ Depth-First Search -- Pesquisa Primeiro em Profundidade --------

depthFirstSearch(Nodo, Destino, [Nodo|Caminho]):-
    dfsa(Nodo, Destino, [Nodo], Caminho).

dfsa(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsa(Destino, Destino, _, _):- !, fail.

dfsa(Nodo, Destino, Visitado, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ memberchk(ProxNodo, Visitado),
    dfsa(ProxNodo, Destino, [Nodo|Visitado], Caminho).

%------------ Depth-limited Search -- Pesquisa Limitada em Profundidade -----

depthLimitedSearch(Nodo, Destino, [Nodo|Caminho]):-
    dlsa(Nodo, Destino, [Nodo], Caminho).

dlsa(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dlsa(Destino, Destino, _, _):- !, fail.

dlsa(Nodo, Destino, Visitado, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ memberchk(ProxNodo, Visitado),
    length(Caminho, Tamanho),
    Tamanho < 100,
    dlsa(ProxNodo, Destino, [Nodo|Visitado], Caminho).

%----------- Breadth-lfirst Search -- Pesquisa Primeiro em Largura ----------

breadthFirstSearch(Nodo, Destino, Caminho) :-
	bfsa([[Nodo]], Solucao, Destino),
	reverse(Solucao, Caminho).

bfsa([[Nodo|Caminho]|_], [Nodo|Caminho], Destino):-
	Nodo == Destino.

bfsa([[Nodo|Caminho]|CaminhoList], Solucao, Destino):-
	bagof([P, Nodo|Caminho],
	(adjacente(Nodo, P), \+ memberchk(P, [Nodo|Caminho])), NovosCaminhos),
	append(CaminhoList, NovosCaminhos, Res), !,
	bfsa(Res, Solucao, Destino);
	bfsa(CaminhoList, Solucao, Destino).

%--------------------- Pesquisa Informada ----------------------------------
%------------ Greedy Search -- Pesquisa Gulosa -----------------------------

greedySearch(Nodo, Destino, Caminho/Custo):-
	estima(Nodo, Destino, Est),
	greedy([[Nodo]/0/Est], InvCaminho/Custo/_, Destino),
	reverse(InvCaminho, Caminho).

greedy(Caminhos, Caminho, Destino):-
	melhorp(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,
	Nodo == Destino.

greedy(Caminhos, SolucaoCaminho, Destino):-
	melhorp(Caminhos, MelhorCaminho),
	selecciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expandirG(MelhorCaminho, ExpCaminhos, Destino),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
	greedy(NovoCaminhos, SolucaoCaminho, Destino).

melhorp([Caminho], Caminho):- !.

melhorp([Caminho1/Custo1/Est1,_/_/Est2|Caminhos], MelhorCaminho):-
	Est1 =< Est2, !,
	melhorp([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).

melhorp([_|Caminhos], MelhorCaminho):-
	melhorp(Caminhos, MelhorCaminho).

expandirG(Caminho, ExpCaminhos, Destino):-
	findall(NovoCaminho, adjacente(Caminho, NovoCaminho, Destino), ExpCaminhos).

estima(P1, P2, Est):-
	ponto(P1, A, B, _,_,_,_),
	ponto(P2, A2, B2,_,_,_,_),
	Est is sqrt(((A-A2)*(A-A2)) + ((B-B2)*(B-B2))).

selecciona(C, [C|Cs], Cs).
selecciona(C, [X|Cs], [X|Xs]):- selecciona(C, Cs, Xs).

adjacente(Ponto, ProxPonto):- adjacencia(Ponto, ProxPonto, _).
adjacente(Ponto, ProxPonto):- adjacencia(ProxPonto, Ponto, _).

adjacente([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est, Destino):-
	adjacencia(Nodo, ProxNodo, PassoCusto),
	\+ memberchk(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estima(ProxNodo, Destino, Est).

