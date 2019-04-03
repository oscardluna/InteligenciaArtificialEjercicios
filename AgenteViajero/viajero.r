library(igraph)
library(treemap)
library(data.tree)

CUMatrix<- matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 1, 0 ,      
                   0, 0, 0, 1, 0, 1, 0, 1, 0 ,
                   1, 0, 1, 0, 1, 0, 0, 0, 0 ,
                   0, 0, 0, 1, 0, 0, 0, 0, 1 ,
                   0, 0, 1, 0, 0, 0, 1, 0, 0 ,
                   0, 0, 0, 0, 0, 1, 0, 0, 0 ,
                   0, 1, 1, 0, 0, 0, 0, 0, 0 ,
                   0, 0, 0, 0, 1, 0, 0, 0, 0), ncol=9, byrow=T)
ciudades<-c("Conta", "Inge", "Ciencias", "Polacas", "Filos", "Quimica", "Medicina", "Derecho", "Odonto")
rownames(CUMatrix)<-ciudades
colnames(CUMatrix)<-ciudades
CUgraph<-graph_from_adjacency_matrix((CUMatrix))
plot(CUgraph, edge.arrow.size=0)

bfs<-function(mat, inicio,meta, ciudades){
   cola<-inicio
   visitados<-NULL
   padre<-"raiz"
   while (!is.null(cola)){
      nodo<- cola[1]
      cola<-cola[-1]
         visitados<-c(visitados, nodo)
         if(nodo==meta){
            padres<-padre[1:length(visitados)]
            return (data.frame(nodos=visitados, padres=padres))
         }
         aCola<-ciudades[ mat[nodo, ]==1 ]
         aCola<-aCola[(!aCola%in%visitados) & (!aCola%in%cola)]
         cola<-c(cola, aCola)
         padre<- c(padre, rep(nodo, length(aCola)))
         
   }
   stop("No se encontró solución")
}

printCamino<-function(inicio, meta, arbol){
   nod=as.character(arbol$nodos)==meta
   res<-meta
   padre<-""
   while(padre!=inicio){
      padre<-as.character(arbol$padres)[nod]
      res<-c(res,as.character(padre))
      nod<-as.character(arbol$nodos)==padre
   }
   print(rev(res))
}

vCiudades<-function(mat,avisitar, ciudades){
   camino<-avisitar[1]
   while(length(avisitar)>=2){
      inicio<-avisitar[1]
      meta<-avisitar[2]
      avisitar<-avisitar[-1]
      
      nodos<-bfs(mat, inicio, meta, ciudades)
      
      raiz<-as.character(nodos[1,1])
      arbol<-nodos[-1,]
      arbol$pathString<-paste("Busqueda", arbol$padres, arbol$nodo, sep="/")
      arbol<-as.Node(arbol)
      print(arbol)
      camino2<-printCamino(inicio, meta, nodos)[-1]
      camino<-c(camino, camino2 )
   }
   return(camino)
}

camino<-vCiudades(CUMatrix, c("Ciencias", "Inge", "Conta", "Odonto"), ciudades)
print(camino)
