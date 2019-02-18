import System.Environment
import Data.Tree hiding (Tree )

{--Tipo de datopara representar el árbol. 
Las ramas del árbol pueden tener una hoja u un nodo con un 
subárbol. Hojas y nodos tienen asociado un tipo de datos 'a'--}
data Tree a= Leaf a | Branch a (Tree a)
   deriving(Show, Eq)

{--Tipos de datos para representar el ambiente.
El ambiente se compone de un cuarto dividido en dos casillas 
(izquierda,derecha). Cada casilla tiene un estado ('EstadoC')
que puede ser sucio o limpio--}
data Ambiente= Cuarto EstadoC EstadoC
   deriving (Show, Eq)
data EstadoC= Sucio|Limpio
   deriving (Show, Eq)

{--Tipos de datos para representar el agente.
El agente es una aspiradora que va tener información sobre su posición 
(izquierda, derecha) y una lista de las posiciones ya visitadas--}
data Agente= Aspiradora Posicion Visitados
   deriving (Show, Eq)
data Posicion= Izq|Der
   deriving (Show, Eq)
type Visitados= [Posicion] 

{--Función que representa el movimiento de la aspiradora. Como solo hay
dos posiciones, moverse implica intercambiar la posición en la que se
encuentra la aspiradora.--}
moverAsp::Posicion->Posicion
moverAsp pos
   |pos==Izq = Der
   |pos==Der = Izq

{--Función que revisa si se ha llegado a la solución (ambas casillas
del cuarto limpias)--}
isFinal::Ambiente->Bool
isFinal (Cuarto izq der)
   |izq==Limpio && der==Limpio = True
   |otherwise= False

{--Función con la que se revisa el estado de la casilla (sucio, limpio)
en la que se encuentra la aspiradora.--}
pos2Est:: Posicion->EstadoC->EstadoC->EstadoC
pos2Est pos ei ed
   |pos==Izq = ei
   |pos==Der = ed

{--Función que actualiza el estado del ambiente y de la aspiradora 
en función de la casilla en la que se encuentra la aspiradora y el
estado de dicha casilla.--}
actualiza::Ambiente->Posicion->[Posicion]-> (Ambiente, Agente)
actualiza (Cuarto ei ed) pos vis=
    let (ei', ed', pos', vis')= case (pos, (pos2Est pos ei ed)) of
          (_, Limpio)-> (ei, ed, (moverAsp pos), (vis++[pos]))
          (Izq, Sucio) -> (Limpio, ed, pos, vis)
          (Der, Sucio) -> (ei, Limpio, pos, vis)
    in ((Cuarto ei' ed'), (Aspiradora pos' vis'))

{--Función para buscar la solución.
Si el el ambiente indicado ya es la solución se devuelve la solución.
Si no es solución, se crea un nuevo nodo, se actualiza el estado del
cuarto y la aspiradora y se llama nuevamente a esta función.
Regresa un árbol con una tupla asociada. La tupla contiene un estado
del ambiente y de la aspiradora.--}
buscaCamino:: (Ambiente, Agente) -> Tree (Ambiente, Posicion)
buscaCamino (cuarto, (Aspiradora pos vis))
      |isFinal cuarto = Leaf (cuarto, pos)
      |otherwise =Branch (cuarto, pos) (buscaCamino (actualiza cuarto pos vis))

{--Transforma al árbol en un formato con el que se puede vizualizar mejor--}
toDataTree (Leaf (a,b)) = Node ((show a)++", Asp "++(show b)) []
toDataTree (Branch (a,b) cs) = Node ((show a)++", Asp "++(show b)) [toDataTree cs]

main= do 
         putStrLn $ drawTree (toDataTree (buscaCamino ((Cuarto Sucio Sucio), (Aspiradora Izq []))))
