
		/* ------------------------------------------------------------ */
		/*			Planteamiento del Problema		*/
		/* ------------------------------------------------------------ */


/*
Considero que mi laberinto será una Lista de Listas:
	-[] 		--> Cierto
	-['_']		--> Falso
	-['_'|_]	--> Falso
	-[['_'|_]|_]	--> Cierto

Siendo el inicio de lectura de dicho laberinto(pos(0,0)) la esquina superior izquierda.
La entrada del laberinto sera la letra 'e'
La salida del laberinto sera la letra 's'
Las paredes y bloqueos se representaran mediante el caracter '#'
El camino libre mediante el espacio ' '.

Ej: laberinto( [ ['#','e','#','#','#','#'],
	         ['#',' ',' ',' ',' ','#'],
	         ['#','#','#','#','s','#'] ]).

El camino seria: Inicio--> pos(1,0).			-->Fila 0, columna 1
		mov( delante, pos(1,0), pos(1,1) ).	--> Aqui tb podria ir para atras...
		mov( derecha, pos(1,1), pos(2,1) ). 	-->Solo hacia ese lado
		mov( derecha, pos(2,1), pos(3,1) ).	-->Tb puede a la izq pero repetiria posicion
		mov( derecha, pos(3,1), pos(4,1) ).
		mov( delante, pos(4,1), pos(4,2) ).	-->Salida

*/




		/* ------------------------------------------------------------ */
		/*			Laberintos diseñados			*/
		/* ------------------------------------------------------------ */


laberintonull([]).
laberintoError1(['#']).
laberintoError2([' ']).
laberintoError3(['s']).
laberintoError4(['e']).

laberinto0( [ ['#','e','#'],
	      ['#',' ','#'],
              ['#','s','#'] ]).

laberinto1( [ ['#','e','#','#','#','#'],
	      ['#',' ',' ',' ',' ','#'],
              ['#','#','#','#','s','#'] ]).

laberinto2( [ ['#','e','#','#','#','#'],
	      ['#',' ',' ',' ',' ','#'],
	      ['#',' ','#',' ','#','#'],
	      ['#',' ',' ',' ',' ','#'],
              ['#','#','#','#','s','#'] ]).

laberinto3( [ ['#','e','#','s','#'],
	      ['#',' ','#',' ','#'],
	      ['#',' ','#',' ','#'],
	      ['#',' ',' ',' ','#'],
              ['#','#','#','#','#'] ]).

laberinto4( [ ['#','#','#'],
	      ['#','e','#'],
	      ['#','s','#'],
	      ['#','#','#'] ]).

laberinto5( [ ['#',' ','#'],
	      [' ','e',' '],
	      ['#',' ','#'],
	      ['#','s','#'],
	      ['#','#','#'] ]).

laberinto6( [ ['#','e','#','#','#','#'],
	      ['#',' ',' ',' ',' ','#'],
	      ['#',' ',' ',' ','#','#'],
	      ['#',' ',' ',' ',' ','#'],
              ['#','#','#','#','s','#'] ]).


		/* ------------------------------------------------------------ */
		/*			Resolucion del problema			*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/*		Resolver laberinto(+Lab, -Recorrido)		*/
/* ------------------------------------------------------------ */
/*	Es cierto si Recorrido unifica con el camino seguido
para resolver el laberinto.
*/
resolverLab([],[]).
resolverLab(Lab, Recorrido):-
	posInicioLab(Lab, Ini),
	posFinLab(Lab, Fin),
	camino(Lab, Ini, Fin, [], Recorrido). /*Me falta por terminar*/




/* ------------------------------------------------------------ */
/* camino(+Lab, +EstadoInicial, +EstadoFinal, *Visitados, -R)	*/
/* ------------------------------------------------------------ */
/*	Es cierto si R unifica con una lista de movimientos para pasar del 
EstadoInicial al EstadoFinal sin repetir los estados de la lista de
estados Visitados.

	Inicial --> Mov --> Estado TMP			Final
				------------------------->
								N-1

*/
camino([], _, _, _, []). /*Caso base*/
camino(_,Fin, Fin, _, [] ).
camino(Lab, Ini, Fin, Visitados, [Mov|Camino]):-
	Ini \= Fin,			/*No es el final*/
	mov(Lab, Mov, Ini, Tmp),	/*Me da el movimiento y la siguiente pos*/
	\+ member(Tmp, Visitados),
	camino(Lab, Tmp, Fin, [Tmp|Visitados], Camino).





		/* ------------------------------------------------------------ */
		/*			Movimientos Disponibles			*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/*			Movimientos				*/
/* ------------------------------------------------------------ */
/* Serie de movimientos que tendra para poder recorrer el laberinto 
*/

	/* Moverse hacia abajo */
mov(Lab, adelante, pos(X,Y), pos(X,Y2)):-
	Y2 is Y+1, 
	pertenece(Lab, pos(X, Y2)),
	\+ pared(Lab,pos(X,Y2)).

	/* Moverse hacia arriba */
mov(Lab, atras, pos(X,Y), pos(X,Y2)):-
	Y2 is Y-1, 
	pertenece(Lab, pos(X, Y2)),
	\+ pared(Lab,pos(X,Y2)).

	/* Moverse hacia la derecha */
mov(Lab, derecha, pos(X,Y), pos(X2,Y)):-
	X2 is X+1, 
	pertenece(Lab, pos(X2, Y)),
	\+ pared(Lab,pos(X2,Y)).

	/* Moverse hacia la izquierda */
mov(Lab, izquierda, pos(X,Y), pos(X2,Y)):-
	X2 is X-1, 
	pertenece(Lab, pos(X2, Y)),
	\+ pared(Lab,pos(X2,Y)).




		/* ------------------------------------------------------------ */
		/*			Pertenencia del laberinto		*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/*		pertenece(+Lab,+X)	 			*/
/* ------------------------------------------------------------ */
/*	Es cierto si X unifica con una posicion válida del laberinto
*/
pertenece([[_]], pos(0,0)).
pertenece(Lab, pos(X,Y)):-
	Y > -1,
	length(Lab, Filas),
	Y < Filas,
	perteneceLista(Lab, X).

/* ------------------------------------------------------------ */
/*		perteneceLista(+Lista,+X)	 		*/
/* ------------------------------------------------------------ */
/*	Es cierto si X unifica con una posicion válida de la lista
Lista.
*/
perteneceLista([_], 0).
perteneceLista([Lista|_], X):-
	X > -1,
	length(Lista, Columnas),
	X < Columnas.
	
	




		/* ------------------------------------------------------------ */
		/*			Ver pos de la entrada ('e')		*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/* 		Posicion Inicio: posInicioLab(+Lab, -pos(X,Y))	*/
/* ------------------------------------------------------------ */
/*	 Es cierto si X e Y unifican con las coordenada
de la entrada ('e') del laberinto, considerando X la 
posicion dentro de uno de los arrays, e Y la posicion
de dicho array dentro del laberinto, es decir, X el ancho
e Y el largo

Buscamos las coordenada X e Y del inicio del laberinto, para saber de 
donde partir.


*/
posInicioLab([],0, 0).
posInicioLab([Lista1|_], pos(X1,0)):-
	posInicioLista(Lista1, X),
	length(Lista1, Pos),
	X < Pos,				
	X1 is X.
posInicioLab([Lista1|Resto], pos(X1,Y1)):-
	posInicioLista(Lista1, X),
	length(Lista1,Pos),
	X == Pos, 				
	posInicioLab(Resto,pos(X1,Y)),		
	Y1 is Y + 1.				
	
	

/* ------------------------------------------------------------ */
/* 			posInicioLista(+Lista, -X)		*/
/* ------------------------------------------------------------ */
/*	Es cierto si X unifica con la posicion de 'e' en la Lista 

Lo que buscamos es la coordenada X de la entrada ('e') del
laberinto


Casos bases: Lista vacia --> X=0
Caso base: contenido en la primera pos --> X=0
Caso base: NO lo contenga --> Deberia estar dentro de X = 0+lenght(Laberinto)
	por lo que considerare que nuca puede encontrarse el array vacio, ni
	que la posicion no puede ser el length del array

Ej: [1 2 3] --> el 2 esta en X = 1
    Cab=1 Resto=[2 3] --> X = 0
    Cab=2 Resto=[3]   --> X = 1

Ej de funcionamiento: posInicioLista(['X','X','e','X'], X).
			Cab = 'X' 	Resto=['X','e','X'] 	--> X = 0
    			Cab = 'X' 	Resto=['e','X']   	--> X = 1
    			Cab = 'e' 	Resto=['X']   		--> X = 2



*/
posInicioLista([], 0).			/*Si no esta en dicho array*/
posInicioLista(['e'|_], 0).		/*Si esta en la primera pos*/
posInicioLista([Cab|Resto], X1):-	/*Resto de casos*/
	Cab \= 'e',			/*NO es la entrada*/
	posInicioLista(Resto, X),	/*Seguimos buscando*/
	X1 is X + 1.			/*Incrementamos la X buscada*/
	




		/* ------------------------------------------------------------ */
		/*			Ver pos de la salida ('s')		*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/* 		Posicion Fin: posFinLab(+Lab, -pos(X,Y))	*/
/* ------------------------------------------------------------ */
/*	 Es cierto si X e Y unifican con las coordenada
de la salida ('s') del laberinto, considerando X la 
posicion dentro de uno de las Listas, e Y la posicion
de dicha Lista dentro del laberinto, es decir, X el ancho
e Y el largo

Buscamos las coordenada X e Y del inicio del laberinto, para saber de 
donde partir.


*/
posFinLab([],0, 0).
posFinLab([Lista1|_], pos(X1, 0)):-
	posFinLista(Lista1, X),
	length(Lista1, Pos),
	X < Pos,
	X1 is X.
posFinLab([Lista1|Resto], pos(X1, Y1)):-
	posFinLista(Lista1, X),
	length(Lista1,Pos),
	X == Pos, 				/* Si no lo ha encontrado */
	posFinLab(Resto,pos(X1,Y)),		/* Seguimos buscando */
	Y1 is Y + 1.				/* Incrementamos la Y */
	
	
/* ------------------------------------------------------------ */
/* 			posFinLista(+Lista, -X)			*/
/* ------------------------------------------------------------ */
/*	Es cierto si X1 unifica con la posicion de 's' en la Lista 

Lo que buscamos es la coordenada X de la salida ('s') del
laberinto


Es igual que posInicioLista

*/
posFinLista([], 0).			/*Si no esta en dicho array*/
posFinLista(['s'|_], 0).		/*Si esta en la primera pos*/
posFinLista([Cab|Resto], X1):-		/*Resto de casos*/
	Cab \= 's',			/*NO es la entrada*/
	posFinLista(Resto, X),		/*Seguimos buscando*/
	X1 is X + 1.			/*Incrementamos la X buscada*/




		/* ------------------------------------------------------------ */
		/*			Ver si hay un bloqueo (Pared)		*/
		/* ------------------------------------------------------------ */


/* ------------------------------------------------------------ */
/* 			pared(+Lab,+pos(X,Y))			*/
/* ------------------------------------------------------------ */
/* Es cierto si pos(X,Y) unifican con si hay o no una pared('#')
en dichas posiciones del laberinto, siendo la Y el numero de
Lista del laberinto (Altura) y X con la posicion del elemento
de dicha Lista (Anchura).


*/
pared([Cab|_], pos(X1,0)):-	
	Cab \= '#',		
	Cab \= 's',		
	Cab \= 'e',		
	Cab \= ' ',		
	length(Cab, Tam),	
	X1 < Tam,		
	paredLista(Cab,X1).
pared([_|Resto], pos(X1,Y1)):-	
	Y1 > 0,
	Y is Y1 - 1,
	pared(Resto,pos(X1, Y)).
	



/* ------------------------------------------------------------ */
/* 			paredLista(+Lista, +X)			*/
/* ------------------------------------------------------------ */
/*	Es cierto si X unifica con la posicion de una pared ('#')
	de la Lista


Lo que buscamos es saber si en X de la Lista esta el simbolo de la
pared ('#').

*/
paredLista(['#'|_], 0).
paredLista([_|Resto], X):-
	X > 0,
	X1 is X - 1,
	paredLista(Resto, X1).
	




		/* ------------------------------------------------------------ */
		/*			     RESULTADOS				*/
		/* ------------------------------------------------------------ */

/*

?- laberinto1(A), resolverLab(A, Camino).
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, #, #, #, s, #]],
Camino = [adelante, derecha, derecha, derecha, adelante] ;
false.

?- laberinto2(A), resolverLab(A, Camino).
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', #, ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, adelante, derecha, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', #, ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, derecha, adelante, adelante, derecha, adelante] ;
false.

?- laberinto3(A), resolverLab(A, Camino).
A = [[#, e, #, s, #], [#, ' ', #, ' ', #], [#, ' ', #, ' ', #], [#, ' ', ' ', ' ', #], [#, #, #, #|...]],
Camino = [adelante, adelante, adelante, derecha, derecha, atras, atras, atras] ;
false.

?- laberinto4(A), resolverLab(A, Camino).
A = [[#, #, #], [#, e, #], [#, s, #], [#, #, #]],
Camino = [adelante] ;
false.

?- laberinto5(A), resolverLab(A, Camino).
A = [[#, ' ', #], [' ', e, ' '], [#, ' ', #], [#, s, #], [#, #, #]],
Camino = [adelante, adelante] ;
A = [[#, ' ', #], [' ', e, ' '], [#, ' ', #], [#, s, #], [#, #, #]],
Camino = [atras, adelante, adelante, adelante] ;
A = [[#, ' ', #], [' ', e, ' '], [#, ' ', #], [#, s, #], [#, #, #]],
Camino = [derecha, izquierda, adelante, adelante] ;
A = [[#, ' ', #], [' ', e, ' '], [#, ' ', #], [#, s, #], [#, #, #]],
Camino = [izquierda, derecha, adelante, adelante] ;
false.

?- laberinto6(A), resolverLab(A, Camino).
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, adelante, derecha, atras, atras, derecha, adelante, adelante|...] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, adelante, derecha, atras, derecha, adelante, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, adelante, derecha, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, derecha, adelante, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, derecha, atras, derecha, adelante, adelante, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, adelante, derecha, derecha, adelante, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, adelante, adelante, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, adelante, derecha, adelante, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, adelante, izquierda, adelante, derecha, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, derecha, adelante, adelante, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, derecha, adelante, izquierda, adelante, derecha, derecha, adelante] ;
A = [[#, e, #, #, #, #], [#, ' ', ' ', ' ', ' ', #], [#, ' ', ' ', ' ', #, #], [#, ' ', ' ', ' ', ' '|...], [#, #, #, #|...]],
Camino = [adelante, derecha, derecha, adelante, izquierda, izquierda, adelante, derecha, derecha|...] ;
false.

?- laberintonull(A), resolverLab(A, Camino).
A = Camino, Camino = [] ;
false.

?- laberintoError1(A), resolverLab(A, Camino).
false.

?- laberintoError2(A), resolverLab(A, Camino).
false.

?- laberintoError3(A), resolverLab(A, Camino).
false.

?- laberintoError4(A), resolverLab(A, Camino).
false.

*/
