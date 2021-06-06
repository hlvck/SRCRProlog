% SRCR - Instrumento de Avaliacao Individual
%
% Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

:- dynamic adjacencia/3.
:- dynamic ponto/7.
%--- adjacencia(pontoID1, pontoID2, distancia).
%--- ponto(pontoID, Longitude, Latitude, Freguesia, Rua, [TipoContentores], CapacidadeTotal).

%--- DATASET COMPLETO ---
%:- include('adjacencias.pl').
%:- include('pontos.pl').

%--- DATASET PARCIAL ---
:- include('sample_adjacencias.pl').
:- include('sample_pontos.pl').

%----------------------- ESTRATEGIAS DE PROCURA ------------------------------
%----------------------- Pesquisa Nao Informada ------------------------------
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

%----------- Breadth-first Search -- Pesquisa Primeiro em Largura ------------

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

%--------------------- Pesquisa Informada ------------------------------------
%------------ Greedy Search -- Pesquisa Gulosa -------------------------------

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

%-------------------- Adjacencias --------------------------------------------

adjacente(Ponto, ProxPonto):- adjacencia(Ponto, ProxPonto, _).
adjacente(Ponto, ProxPonto):- adjacencia(ProxPonto, Ponto, _).


adjacente([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est, Destino):-
	adjacencia(Nodo, ProxNodo, PassoCusto),
	\+ memberchk(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estima(ProxNodo, Destino, Est).

adjacenteD(Ponto, ProxPonto, D):- adjacencia(Ponto, ProxPonto, D).
adjacenteD(Ponto, ProxPonto, D):- adjacencia(ProxPonto, Ponto, D).

%-------------------- RESOLUCOES ---------------------------------------------
%------ Circuitos Nodo->Destino Residuos Indiferenciados ---------------------

circuitoIndiferenciado(Nodo, Destino, Circuitos):-
	findall(Caminho, breadthFirstSearch(Nodo, Destino, Caminho), Circuitos).

%------- Circuitos Nodo->Destino Residuos Seletivos --------------------------

circuitoSeletivo(Nodo, Destino, Tipo, Circuitos):-
	findall(Caminho, dfscsa(Nodo, Destino, Tipo, Caminho), Circuitos).

dfscsa(Nodo, Destino, Tipo, [Nodo|Caminho]):-
    dfscs(Nodo, Destino, [Nodo], Tipo, Caminho).

dfscs(Nodo, Destino, _, Tipo, [Destino]):-
    adjacente(Nodo, Destino),
    ponto(Destino, _, _, _, _, T, _),
    member(Tipo, T). 

dfscs(Destino, Destino, _, _, _):- !, fail.

dfscs(Nodo, Destino, Visitado, Tipo, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ memberchk(ProxNodo, Visitado),
    ponto(ProxNodo, _, _, _, _, T, _),
    memberchk(Tipo, T),
    dfscs(ProxNodo, Destino, [Nodo|Visitado], Tipo, Caminho).

%------- Circuitos com mais pontos (por tipo) --------------------------------

maisPontosRecolha(Nodo, Destino, Tipo, Circuitos):-
	findall(MLCircuitos, (circuitoSeletivo(Nodo, Destino, Tipo, CircuitosTodos),
	maisLongas(CircuitosTodos, MLCircuitos)), Circuitos).

maxlen([],[]).
maxlen([H|T], [LH|LHT]) :-
	length(H, LH),
	maxlen(T, LHT).

lengthLongest(ListofLists, Max):-
	maxlen(ListofLists, Ls),
	max_list(Ls, Max).

maisLongas(ListofLists, ML):-
	lengthLongest(ListofLists, Len),
	member(ML, ListofLists),
	length(ML, Len).

%------ Indicadores de Produtividade: Distancia Media -----------------------

indicadorProdutividade(Nodo, Destino, Result):-
	findall((PP, DD), dfsp(Nodo, Destino, PP, DD), Result).

dfsp(Nodo, Destino, Caminho, CustoMedio):-
	depthFirstSearch(Nodo, Destino, Caminho),
	distanciaTotal(Caminho, Custo),
	length(Caminho, Total),
	CustoMedio is Custo/Total.

distanciaTotal([P1,P2], D):-
	adjacenteD(P1,P2,D).

distanciaTotal([P1,P2|T], D):-
	adjacenteD(P1, P2, D1),
	distanciaTotal([P2|T], D2),
	D is D1+D2.

%------ Caminho mais rapido (menor distancia) --------------------------------

caminhoMaisRapido(Nodo, Destino, Result):-
	findall((PP, DD), dfsml(Nodo, Destino, PP, DD), Caminhos),
	minimo(Caminhos, Result).

dfsml(Nodo, Destino, Caminho, Custo):-
	depthFirstSearch(Nodo, Destino, Caminho),
	distanciaTotal(Caminho, Custo).

minimo([H|T], R):- minima(T, H, R).

minima([], M, M).

minima([(_, C)|T], (NM, CM), M):-
	C>CM, !,
	minima(T, (NM, CM), M).

minima([(H, C)|T], (_, _), M):-
	minima(T, (H,C), M).

%------ Caminho mais eficiente - Capacidade total / Distancia ----------------

caminhoMaisEficiente(Nodo, Destino, Result):-
	findall((PP, DD), dfsme(Nodo, Destino, PP, DD), Caminhos),
	maximo(Caminhos, Result).

dfsme(Nodo, Destino, Caminho, Eficiencia):-
	depthFirstSearch(Nodo, Destino, Caminho),
	distanciaTotal(Caminho, Custo), 
	capacidadeTotal(Caminho, Capacidade),
	Eficiencia is Capacidade/Custo.

capacidadeTotal([P1,P2], C):-
	ponto(P1,_,_,_,_,_,C1),
	ponto(P2,_,_,_,_,_,C2),
	C is C1+C2.

capacidadeTotal([P1,P2|T], C):-
	ponto(P1,_,_,_,_,_,C1),
	distanciaTotal([P2|T],C2),
	C is C1+C2.

maximo([H|T], R):- maxima(T, H, R).

maxima([], M, M).

maxima([(_, C)|T], (NM, CM), M):-
	C<CM, !,
	maxima(T, (NM, CM), M).

maxima([(H, C)|T], (_, _), M):-
	maxima(T, (H,C), M).


