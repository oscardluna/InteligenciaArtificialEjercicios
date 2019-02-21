import System.Environment
import Data.List

type Estado=[Int]
type Visitados=[Estado]

{--Función que representa el movimiento del elemento Cero.
Esta función intercambia dos elementos de la lista a partir del índice
de los elementos.--}
swap::Int->Int->[Int]->[Int]
swap f s xs=zipWith (\x y ->
    if x== f then xs !! s
    else if x==s then xs !! f
    else y) [0..] xs

{--Función que revisa los movimientos disponibles basándose en el 
índice del elemento cero.
Arriba= Index-3      Derecha= Index+1
Abajo= Index+3       Izquierda= Index-1
Solo se regresan los movimientos dentro del rango permitido (0-8)--}
buscaMovs::Int->[Int]
buscaMovs ceroIndex=
   let moves= (map (+ceroIndex) [-3,1,3,-1]) in
      [(x)|x<-moves, x>=0 && x<9]

{--Función para buscar el índice del arreglo donde está el elemento cero--}
buscaCeroIndex::[Int]->Int
buscaCeroIndex estado=
   let index= (elemIndex 0 estado) in
      case index of
         (Just x)-> x
         _-> error "Algo anda mal"

{--Función que expande un nodo. Regresa los hijos del nodo que contengan un
estado que no haya sido visitado. Si el estado fue visitado se descarta.--}
expande::Estado->Visitados->[(Estado, Visitados)]
expande estado visitados=
   let ceroIndex= (buscaCeroIndex estado) in
      let movs= (buscaMovs ceroIndex) in --Se buscan las casillas a donde se pueden hacer movimientos
        let estados= [(swap ceroIndex x estado)|x<-movs] in --Se generan los nuevos estados
            [(x, visitados)|x<-estados, (notElem x visitados)] --Se regresan solo los estados no visitados

{-Función que busca la solución aplicando el algoritmo de búsqueda por amplitud.
Teoricamente debería mostrar cuando el rompecabezas no tiene una solución, pero tarda mucho como
para comprobarlo.-}
buscaSol::[(Estado, Visitados)]->Visitados --Estado a analizar, Estados a expandir, visitados
buscaSol cola= case cola of
                  []->error "no hay solucion"
                  _->let ((estado, visitados), cola')= ((head cola), tail cola)  in --Saca un elemento de la cola
                        case (isFinal estado) of   --Se comprueba si el elemento tiene el estado final
                           True-> visitados++[estado] --Verdadero= Se regresan las casillas visitadas
                           _-> let aCola= (expande estado (visitados++[estado])) in  --Falso=Se expanden los hijos del nodo
                                 let cola''= cola'++aCola in   --Se actualiza la cola con los hijos
                                       buscaSol cola''   --Se vuelve a llamar la función con la cola actualizada

{--Función que revisa si se ha llegado a la solución--}
isFinal::Estado->Bool
isFinal estado
   |estado==[1,2,3,4,5,6,7,8,0]= True
   |otherwise= False


{-Dan formato al resultado para imprimir-}
imprimeNodo::Estado->String
imprimeNodo []= "\n"
imprimeNodo [a,b,c,d,e,f,g,h,i]=
   (show [a,b,c])++"\n"++(show [d,e,f])++"\n"++(show[g,h,i])
imprimeCamino::Visitados->String
imprimeCamino [c]= (imprimeNodo c) ++"\n"
imprimeCamino (c:cs)=
   (imprimeNodo c) ++ "\n"++"   v   "++"\n" ++ (imprimeCamino cs)

main= do 
         --let estado=[([1,2,3,4,5,6,7,0,8],[])]
         let estado= [([0,1,3,4,2,5,7,8,6],[])]
         --let estado= [([0,8,2,1,4,3,7,6,5],[])]
         --let estado= [([1,0,3,2,4,5,6,7,8],[])]
         putStr $imprimeCamino$ (buscaSol estado)
