#libraries
library(XML)
library(igraph)

#data source
con <- ("http://www.frba.utn.edu.ar/html/Electrica/pag_electrica.php?pag=correl")
corrHTML <- readLines(con)
corrTablesHTML <- readHTMLTable(corrHTML)

#create Data Frame
df <- data.frame(corrTablesHTML)
colnames(df) <- c("nivel","numero","asignatura","cpc","apc","apr")

#format Data Frame
for(i in 1:6){df[,i] <- as.character(df[,i])}

v <- c("I","II","III","IV","V") #levels
a <-which(df$nivel %in% v)
a[6] <- nrow(df) + 1

for(i in 1:5){for(j in a[i]:a[i+1]-1) {df[j,1] <- v[i]}}

df[44,4] <- "1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-36"
df[44,6] <- "1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-36"
df[40,6] <- "1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30-31-32-33-34-35-36-37-38-39-41-42-43-44-45"

fcel <- function(s) {strsplit(as.character(s),'-')}
fcol <- function(c) sapply(df[,c],fcel)
transformdf <- function(df) data.frame(sapply(colnames(df),fcol))
df <- transformdf(df)

#create adjacency matrix for cpc
m <- matrix(nrow=0,ncol=45)
for(i in 1:45){
	newRow <- sapply(seq(1:45),function(j) {
		as.character(j) %in% df[i,"cpc"][[1]]
		});
	m <- rbind(m,newRow);
}

#create graph
g <- graph.adjacency(m,mode="undirected")

#plot graph
plot(g,layout=layout.circle)