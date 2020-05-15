
install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(RefManageR)
library(bibliometrix)
library(quanteda)
library(ggplot2)
library(ggpubr)

help("bibliometrix")
help("igraph")
help("network")

D <- readFiles("/home/alrier/Descargas/fullcovid.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")
N <- biblioAnalysis(M, sep = ";")

S <- summary(object = N, k = 20, pause = FALSE)
plot(x = N, k = 20, pause = FALSE)

'''primera red matrici de datos cpn su grafica'''
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
P <- normalizeSimilarity(NetMatrix, type = "association")
perrosnet <- networkPlot(P, n = 20, Title = "co-occurrence network", type = "fruchterman", 
                         labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "walktrap",
                         remove.isolates = F, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)

'''segunda red matricial de datos con su grafica'''
NetMatrix2 <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ". ")
n <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix3 <- biblioNetwork(n, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix3, n = 10, Title = "Country Collaboration", type = "fruchterman", labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "spinglass",
                remove.isolates = T, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)



'''Del total de resultados, extraigo los papers más citados'''
AU <- N$MostCitedPapers 
AUT <- AU[1:2]
View(AUT)
View(AU)

'''Lets see most productive countries'''
Paises <- N$Countries
Paises <- S$MostProdCountries
View(Paises)
'''Lets keep the first and thirs column of this dataframe'''
Paises <- Paises[c(1, 3)]
'''Lets change the name of the first column'''
names(Paises)[1] <- "Country"
'''Pongamos los nombres en Español'''
Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")
Paises$Freq <- suppressWarnings(as.numeric(Paises$Freq))
'''Lets see the production'''
Produccion <- S$AnnualProduction
'''Lets change the name of the first column'''
names(Produccion)[1] <- "Year"
'''Lets set as numeric the records of the second column'''
Produccion$Articles <- as.numeric(Produccion$Articles)

'''graficas y plots'''

Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)


'''Minería de datos'''
texto = Corpus(VectorSource(N)) 
'''minuscula (A!=a)'''
discurso=tm_map(texto, tolower)
'''quitamos los espacios en blanco'''
discurso =tm_map(discurso, stripWhitespace)
'''quitamos la puntuacion'''
discurso = tm_map(discurso, removePunctuation)
'''quitamos los numeros'''
discurso = tm_map(discurso, removeNumbers)


'''quitamos palabras genericas'''
discurso=tm_map(discurso, removeWords,stopwords("english"))

############### DATA FRAME DE PALABRAS CON SU FRECUENCIA

'''Creamos matriz de letras'''
letras= TermDocumentMatrix(discurso)
findFreqTerms(letras, lowfreq=5)
matrix=as.matrix(letras)

'''lo ordenamos y sumamos las letras de nuestra matriz'''
vector <- sort(rowSums(matrix),decreasing=TRUE) 
'''creamos la data con las palabras y su frecuencia'''
dataletras <- data.frame(word= names(vector),frequencia=vector) 

################ GRAFICAMOS FRECUENCIA DE LAS PALABRAS

barplot(dataletras[1:10,]$freq, las = 2, names.arg = dataletras[1:10,]$word,
        col ="blue", main ="PALABRAS M?S FRECUENTES", ylab = "Frecuencia de palabras")


############ GRAFICAMOS LA NUBE DE PALABRAS
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 2,
          max.words=70)
'''organizar la nube de palabras'''
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 2,
          max.words=70, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))
