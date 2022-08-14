% Parte 1

statusDeSangre(harry,mestiza).
statusDeSangre(draco,pura).
statusDeSangre(hermione,impura).
statusDeSangre(neville,pura).

tieneCaracteristica(harry,coraje).
tieneCaracteristica(harry,amistad).
tieneCaracteristica(harry,orgullo).
tieneCaracteristica(harry,inteligencia).
tieneCaracteristica(draco, inteligencia).
tieneCaracteristica(draco,orgullo).
tieneCaracteristica(hermione,inteligencia).
tieneCaracteristica(hermione,orgullo).
tieneCaracteristica(hermione,responsabilidad).
tieneCaracteristica(neville,responsabilidad).
tieneCaracteristica(neville,amistad).
tieneCaracteristica(neville,coraje).

odiariaIrA(harry,slytherin).
odiariaIrA(draco,hufflepuff).

caracteristicaBuscada(gryffindor,coraje).
caracteristicaBuscada(slytherin,orgullo).
caracteristicaBuscada(slytherin,inteligencia).
caracteristicaBuscada(ravenclaw,inteligencia).
caracteristicaBuscada(ravenclaw,responsabilidad).
caracteristicaBuscada(hufflepuff,amistad).

%porque hay que asegurarse que sea tiene casa valida de hogwarts
casa(gryffindor).
casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).

mago(Mago):- statusDeSangre(Mago,_).

%Punto 1
permiteEntrarA(Casa, Mago):-
    mago(Mago),
    casa(Casa),
    Casa \= slytherin.
permiteEntrarA(slytherin,Mago):-
    not(statusDeSangre(Mago,impura)).
% statusDeSangre(Mago,Sangre), Sangre /= impura
    

%Punto 2
tieneCaracterApropiado(Mago, Casa):-
    casa(Casa),
    mago(Mago),
    forall(caracteristicaBuscada(Casa,Caracteristica), tieneCaracteristica(Mago,Caracteristica)).

%Punto 3
puedeQuedarEn(Mago,Casa):- %no necesitamos instanciar mago y casa porque lo hacen los otros predicados
    tieneCaracterApropiado(Mago, Casa),
    permiteEntrarA(Casa,Mago),
    not(odiariaIrA(Mago,Casa)).
puedeQuedarEn(hermione,gryffindor).

%Punto 4
cadenaDeAmistades(Magos):-
    sonAmistosos(Magos),
    cadenaDeCasas(Magos).

sonAmistosos(Magos):-
    forall(member(Mago, Magos), esAmistoso(Mago)).

esAmistoso(Mago):-
    mago(Mago),
    tieneCaracteristica(Mago, amistad).


cadenaDeCasas([Mago1,Mago2|OtrosMagos]):-
    puedeQuedarEn(Mago1,Casa),
    puedeQuedarEn(Mago2,Casa),
    cadenaDeCasas([Mago2|OtrosMagos]).
cadenaDeCasas([_]).

/*
%otra opcion
cadenaDeCasas(Magos):-
    forall(consecutivos(Mago1,Mago2,Magos),
            puedeQuedarEnLaMismaCasa(Mago1,Mago2)).
    
consecutivos(Anterior,Siguiente,Lista):-
    nth1(IndiceAnterior,Lista,Anterior),
    IndiceSiguiente is IndiceAnterior+1,
    nth1(IndiceSiguiente,Lista,Siguiente).

puedeQuedarEnLaMismaCasa(Mago1,Mago2):-
    puedeQuedarEn(Mago1,Casa),
    puedeQuedarEn(Mago2,Casa),
    Mago1 \= Mago2.
*/

%Parte 2 - La copa de las casas
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

hizo(harry,fueraDeLaCama).
hizo(hermione,irA(seccionRestringidaDeLaBiblioteca)).
hizo(hermione,irA(tercerPiso)).
hizo(harry,irA(elBosque)).
hizo(harry,irA(tercerPiso)).
hizo(draco,irA(lasMazmorras)).
hizo(ron, buenaAccion(50, ganarAlAjedrezMagico)).
hizo(hermione, buenaAccion(50, salvarASusAmigos)).
hizo(harry, buenaAccion(60,ganarleAVoldemort)).

%Punto 1
%Item A
esBuenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

hizoAlgunaAccion(Mago):-
    hizo(Mago,_).

hizoAlgoMalo(Mago):-
    hizo(Mago,Accion),
    puntajeQueGenera(Accion,Puntaje),
    Puntaje < 0.

puntajeQueGenera(fueraDeLaCama, -50).
puntajeQueGenera(irA(Lugar), PuntajeQueResta):-
    lugarProhibido(Lugar, Puntaje),
    PuntajeQueResta is Puntaje * -1.
puntajeQueGenera(buenaAccion(Puntaje,_), Puntaje).

lugarProhibido(elBosque, 50).
lugarProhibido(seccionRestringidaDeLaBiblioteca, 10).
lugarProhibido(tercerPiso, 75).

%Item B
