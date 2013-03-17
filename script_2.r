library(igraph)
library(statnet)
library(RJSONIO)

rawData <- '{
    "1":{"nombre":"Sistemas y Organizaciones","cc":"","fc":"","cf":"","ff":""},
    "2":{"nombre":"Algoritmos y Estructuras de Datos","cc":"","fc":"","cf":"","ff":""},
    "3":{"nombre":"Arquitectura de Computadoras","cc":"","fc":"","cf":"","ff":""},
    "4":{"nombre":"Ingeniería y Sociedad","cc":"","fc":"","cf":"","ff":""},
    "5":{"nombre":"Álgebra y Geometría Analítica","cc":"","fc":"","cf":"","ff":""},
    "6":{"nombre":"Matemática Discreta","cc":"","fc":"","cf":"","ff":""},
    "7":{"nombre":"Análisis Matemático I","cc":"","fc":"","cf":"","ff":""},
    "8":{"nombre":"Paradigmas de Programación","cc":"6,2","fc":"","cf":"","ff":"6,2"},
    "9":{"nombre":"Sistemas Operativos","cc":"6,2,3","fc":"","cf":"","ff":"6,3,2"},
    "10":{"nombre":"Análisis de Sistemas","cc":"1,2","fc":"","cf":"","ff":"1,2"},
    "11":{"nombre":"Sintaxis y Semántica de los Lenguajes","cc":"6,2","fc":"","cf":"","ff":"6,2"},
    "12":{"nombre":"Probabilidad y Estadística","cc":"7,5","fc":"","cf":"","ff":"7,5"},
    "13":{"nombre":"Análisis Matemático II","cc":"7,5","fc":"","cf":"","ff":"7,5"},
    "14":{"nombre":"Diseño de Sistemas","cc":"10,8","fc":"1,6,2","cf":"","ff":"10,8"},
    "15":{"nombre":"Comunicaciones","cc":"3,13,27","fc":"7,5,26","cf":"","ff":"3,27,13"},
    "16":{"nombre":"Gestión de Datos","cc":"10,8,11","fc":"6,2,1","cf":"","ff":"8,10,11"},
    "17":{"nombre":"Redes de Información","cc":"9,15","fc":"6,2,3,13,27","cf":"","ff":"9,15"},
    "18":{"nombre":"Administración de Recursos","cc":"14,9,30","fc":"3,23,10,8","cf":"","ff":"14,30,9"},
    "19":{"nombre":"Teoría de Control","cc":"28,7","fc":"13,27","cf":"","ff":"29,28"},
    "20":{"nombre":"Proyecto (Sistemas)","cc":"31,18,17,32","fc":"25,9,16,24,4,12,14,30,15","cf":"","ff":""},
    "21":{"nombre":"Administración Gerencial","cc":"18,35","fc":"12,14,9,29,30","cf":"","ff":"35,18"},
    "22":{"nombre":"Inteligencia Artificial","cc":"35,36","fc":"12,14,29","cf":"36,35","ff":""},
    "23":{"nombre":"Inglés I","cc":"","fc":"","cf":"","ff":""},
    "24":{"nombre":"Inglés II","cc":"23","fc":"","cf":"","ff":"23"},
    "25":{"nombre":"Sistemas de Representación","cc":"","fc":"","cf":"","ff":""},
    "26":{"nombre":"Física I","cc":"","fc":"","cf":"","ff":""},
    "27":{"nombre":"Física II","cc":"26,7","fc":"","cf":"","ff":"26,7"},
    "28":{"nombre":"Química General","cc":"","fc":"","cf":"","ff":""},
    "29":{"nombre":"Matemática Superior","cc":"13","fc":"5,7","cf":"","ff":"13"},
    "30":{"nombre":"Economía","cc":"10","fc":"1,2","cf":"","ff":"10"},
    "31":{"nombre":"Legislación","cc":"10,4","fc":"1,2","cf":"","ff":"4,1"},
    "32":{"nombre":"Ingeniería de Software","cc":"12,14,16","fc":"10,11,8","cf":"","ff":"12,16,14"},
    "33":{"nombre":"Sistemas de Gestión","cc":"18,35,36","fc":"12,14,9,29,30","cf":"","ff":"35,18,36"},
    "34":{"nombre":"Práctica Profesional Supervisada (Sistemas)","cc":"","fc":"","cf":"","ff":""},
    "35":{"nombre":"Investigación operativa (Sistemas)","cc":"12,7","fc":"13","cf":"","ff":"12,7"},
    "36":{"nombre":"Simulación (Sistemas)","cc":"12,7","fc":"13","cf":"","ff":"12,7"},
    "37":{"nombre":"Módulo A","cc":"","fc":"","cf":"","ff":""},
    "38":{"nombre":"Módulo B","cc":"","fc":"","cf":"","ff":""}
}'

data <- fromJSON(rawData)
df <- data.frame(data)
df <- t(df)
df <- data.frame(df)

fcel <- function(s) {strsplit(as.character(s),',')}
fcol <- function(c) sapply(df[,c],fcel)
transformdf <- function(df) data.frame(sapply(colnames(df),fcol))
df <- transformdf(df)

n <- nrow(df)

m_cc <- matrix(nrow=0,ncol=n)
for(i in 1:n){
  newRow <- sapply(seq(1:n),function(j) {
    as.character(j) %in% df[i,"cc"][[1]]
  });
  m_cc <- rbind(m_cc,newRow);
}

m_cc <- t(m_cc)
colnames(m_cc) <- seq(1:n)

m_cf <- matrix(nrow=0,ncol=n)
for(i in 1:n){
  newRow <- sapply(seq(1:n),function(j) {
    as.character(j) %in% df[i,"cf"][[1]]
  });
  m_cf <- rbind(m_cf,newRow);
}

m_cf <- t(m_cf)
colnames(m_cf) <- seq(1:n)

m_fc <- matrix(nrow=0,ncol=n)
for(i in 1:n){
  newRow <- sapply(seq(1:n),function(j) {
    as.character(j) %in% df[i,"fc"][[1]]
  });
  m_fc <- rbind(m_fc,newRow);
}

m_fc <- t(m_fc)
colnames(m_fc) <- seq(1:n)

m_ff <- matrix(nrow=0,ncol=n)
for(i in 1:n){
  newRow <- sapply(seq(1:n),function(j) {
    as.character(j) %in% df[i,"ff"][[1]]
  });
  m_ff <- rbind(m_ff,newRow);
}

m_ff <- t(m_ff)
colnames(m_ff) <- seq(1:n)

par(mfrow=c(1,4))

#f <- function(s) {strsplit(as.character(s)," ")[[1]][1]}
#lbls <- sapply(as.vector(df$nombre),f)  
#lbls <- as.vector(df$nombre)
lbls <- seq(1:38)

colnames(m_cc) <- lbls
g <- graph.adjacency(m_cc,mode="undirected")
plot(g,layout=layout.circle)

colnames(m_fc) <- lbls
g <- graph.adjacency(m_fc,mode="undirected")
plot(g,layout=layout.circle)

colnames(m_cf) <- lbls
g <- graph.adjacency(m_cf,mode="undirected")
plot(g,layout=layout.circle)

colnames(m_ff) <- lbls
g <- graph.adjacency(m_ff,mode="undirected")
plot(g,layout=layout.circle)

#nodespos <- matrix(nrow=0,ncol=2)
#for(i in 1:4){for(j in 1:10){nodespos <- rbind(nodespos,c(i,j))}}

#net <- as.network(m_cc, matrix.type = "adjacency", directed = TRUE)
#gplot(net, jitter=FALSE, label=seq(1:38), pad=1,label.cex=0.8,coord=nodespos)

#net <- as.network(m_fc, matrix.type = "adjacency", directed = TRUE)
#gplot(net, jitter=FALSE, label=seq(1:38), pad=1,label.cex=0.8)

#net <- as.network(m_cf, matrix.type = "adjacency", directed = TRUE)
#gplot(net, jitter=FALSE, label=seq(1:38), pad=1,label.cex=0.8)

#net <- as.network(m_ff, matrix.type = "adjacency", directed = TRUE)
#gplot(net, jitter=FALSE, label=seq(1:38), pad=1,label.cex=0.8)

#gplot(net, jitter=FALSE, label=as.vector(df$nombre), pad=1)
#gplot(net, jitter=FALSE, label=as.vector(df$nombre), pad=1,label.cex=0.5,mode="circle")
#get.shortest.paths(g, "1", "32", output="vpath")
#shortest.paths(g,v=V(g),to=V(g),mode="all", algorithm = "dijkstra")