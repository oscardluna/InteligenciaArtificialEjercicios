library(igraph)
library(treemap)
library(data.tree)

CUMatrixP<- read.csv("C:/pCloudSync/ia/viajero2/CU.csv")
CUMatrixP<-as.matrix(CUMatrixP[,-1])
ciudades<-colnames(CUMatrixP)
rownames(CUMatrixP)<-ciudades

CUMatrix<-replace(CUMatrixP, CUMatrixP>0,1)
CUgraph<-graph_from_adjacency_matrix((CUMatrix))
plot(CUgraph, edge.arrow.size=0)

mainP<-function(mat, actual, metas, ciudades){
   
   buscaSegOpt<-function(actual, meta){
      res<-list(Camino="", Peso=Inf)
      buscaCamino<-function(actual, meta, v=NULL, w=0){
         hijos<-ciudades[mat[ciudades==actual]>0]
         for(n in hijos){
            if(n%in%meta){
               #camino<-paste(c(v, n), collapse='->')
               peso<-w+mat[actual, n]
               if(peso<res$Peso)
                  res<<-list(Camino=c(v,n), Peso=peso)
            }
            else if(!n%in%v){
               vis<-c(v, n)
               peso<-w+mat[actual, n]
               buscaCamino(n, meta,vis, peso)
            }
         }
      }
      buscaCamino(actual, meta)
      return(res)
   }
   
   res<-data.frame(Camino="", Peso=Inf)
   buscaOpt<-function(inicio, metas=metas, camino=inicio, wT=0){
      if( identical(metas, character(0)) ){
         res<<-rbind(res, data.frame(Camino=paste(camino,collapse='->'), Peso=wT))
      }
      else{
         for(m in metas){
            r<-buscaSegOpt(inicio, m)
            pesoN<-wT+r$Peso
            caminoN<-c(camino, r$Camino)
            metasN<-metas[metas!=m]
            buscaOpt(m, metasN, caminoN, pesoN)
         }
      }
   }
   buscaOpt(actual, metas)
   #res$Camino<-paste(res$Camino, collapse = '->')
   #res<-res[order(res$Peso),]
   return(res)
}


mainP(CUMatrixP, "Medicina", c("Ciencias", "Polacas", "Quimica"), ciudades)
