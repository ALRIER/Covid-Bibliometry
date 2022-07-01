
pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "raster","sf","cluster", "factoextra",
              "NbClust","tidyr", "forecast", "semPlot", "semTools", "corrplot", "corrr",
              "haven", "psych", "dplyr", "lavaan", "readr", "cvms","tm","NLP",
              "SnowballC","RColorBrewer","wordcloud","wordcloud2", "RefManageR",
              "bibliometrix","GGally","quanteda","ggplot2", "ggpubr",
              "Factoshiny","VGAM")

pkg(packages)

#i set up the directori in hich im goint to work
setwd("/home/alrier/Documentos/covid")
#upload the document or dataset
file <- ("/home/alrier/Documentos/covid/UniqueTotalCovid.csv")

#convert it into a Dataframe
M <- convert2df(Totalcovid, dbsource = "scopus", format = "excel")
#Playing with duplicates.
newM <- duplicatedMatching(M, Field = "Title", exact=FALSE, tol = 0.95)
#I will write my new DF without duplicates.
write.table(cvstotal, file="UniqueTotalCovid.csv", row.names= T)
#create my summary
S1 <- biblioAnalysis(newM, sep = ";")
options(width=100)
S2 <- summary(object = S1, k = 1000, pause = FALSE)
#first general plots
plot(x = S1, k = 20, pause = FALSE)



#'''Paso los documentos a formato tibble para trabajar un filtro
#por años a partir del año 2018 en adelante, pero es solo para hacer
#un análisis más liviano, después puedo retomar el objeto M que es un
#DF y contiene todos los resultados'''
#N1 <- as_tibble(M)
#N2<- N1 %>% filter(PY >=2018)
#convierto nuevamente a DF para continuar trabajando sobre mi filtro
#M1 <- as.data.frame(N2)
#agrupo y resumo resultados

#To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
MFCM<-cbind(CR$Source[1:10])
view(MFCM)
write.table(MFCM, file="most frequent cited manuscripts source
            covid.csv", sep=";", row.names= T)

#I will extract the most cited papers and manualy i will look for the
#info about each one of those articles###################
b<-S2$MostCitedPapers

write.table(b, file="MostCitedPapers.csv", sep=";", row.names= T)
#To obtain the most frequent cited first authors:
#CR1 <- citations(M, field = "author", sep = ";")
#MFCA<-cbind(CR$Year[1:15])
#view(MFCA)
#write.table(MFCA, file="most frequent cited AUTHORSfull.csv", sep=";",
#            row.names= T)
#s<- CR$Authors[1:15,]
#Authors’ Dominance ranking ----------------------
DF <- dominance(S1, k = 10)
write.table(DF, file="Dominance covid.csv", sep=";", row.names= T)

##Authors’ h-index-------------------------------------
authors=gsub(","," ",names(S1$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";",
                  years = 50)
indices$H
indices$CitationList
cl2<-citationslist[[2]]
view(cl)
write.table(cl, file="citationslist.csv", sep=";", row.names= T)

#Top-Authors’ Productivity over the Time ----------------
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

## Table: Author's productivity per year -----------------
top<-topAU$dfAU
write.table(top, file="productividadxaño.csv", sep=";", row.names= T)
#Bipartite networks --------------------------
A <- cocMatrix(M, Field = "SO", sep = ";")
a1<-sort(Matrix::colSums(A), decreasing = TRUE)[1:10]
view(a1)

#Citation network -------------------------
cit <- cocMatrix(M, Field = "CR", sep = ".  ")
cit1<-sort(Matrix::colSums(cit), decreasing = TRUE)[1:10]
view(cit1)

#Author network ------------------------------
AUTors <- cocMatrix(M, Field = "AU", sep = ";")
AUTors1<-sort(Matrix::colSums(AUTors), decreasing = TRUE)[1:10]
view(AUTors1)

#Country network ------------------------
country <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
C <- cocMatrix(country, Field = "AU_CO", sep = ";")
C <-sort(Matrix::colSums(C), decreasing = TRUE)[1:10]
view(C)

# Create keyword co-occurrences network --------------------
cooccurrences <- biblioNetwork(M, analysis = "co-occurrences",
                               network = "keywords", sep = ";")
net=networkPlot(cooccurrences, normalize="association", weighted=T,
                n = 10, Title = "Keyword Co-occurrences",
                type = "fruchterman", size=T,edgesize = 5,
                labelsize=0.7)

#Bibliographic co-citation ---------------------------------
cocitation <- biblioNetwork(M, analysis = "co-citation",
                            network = "references", sep = ".  ")
net=networkPlot(cocitation, normalize="association", weighted=T,
                n = 10, Title = "co-citation", type = "fruchterman",
                size=T,edgesize = 5,labelsize=0.7)

#Bibliographic collaboration---------------------------
collaboration <- biblioNetwork(M, analysis = "collaboration",
                               network = "authors", sep = ";")
net=networkPlot(collaboration, n = 10, Title = "Collaboration",
                type = "circle", size=TRUE, remove.multiple=F,
                labelsize=0.7,cluster="none")

# Conceptual Structure using keywords (method="CA")--------------
CS <- conceptualStructure(M, field="ID", method="CA", minDegree=4,
                          clust=2, stemming=FALSE, labelsize=10,
                          documents=10)

# Conceptual Structure using keywords (method="CA")--------------
t<-as.tibble(M)
ti<- t %>% filter((PY==2020 | PY==2021)& TC>80)
tib<- as.data.frame(ti)
CS <- conceptualStructure(tib,field="ID", method="MCA", minDegree=4,
                          clust=3, stemming=FALSE, labelsize=10,
                          documents=3)
# Create a country collaboration network
countrycolab <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
countrycolab <- biblioNetwork(countrycolab,
                              analysis = "collaboration", network = "countries",
                              sep = ";")
net=networkPlot(countrycolab, n = 4,
                Title = "Country Collaboration", type = "circle",
                size=TRUE, remove.multiple=F,labelsize=0.7,cluster="none")
#Create a historical citation network
t1<-as.tibble(M)
ti2<- t1 %>% filter(PY >=2021)
tib3<- as.data.frame(ti2)
class(M) <- c("bibliometrixDB", "data.frame")
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")
#Plot a historical co-citation network
histPlot(histResults, n = 65, size = 4, labelsize = 4, verbose = TRUE)
#Top authors prod-over time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
topAU
topAU$dfAU
#Del total de resultados, extraigo los papers más citados--------
# '''los puedo retirar del sumario o del total de observaciones'''
# '''Estraidos del total de observaciones'''
AU <- S2$MostCitedPapers
# '''Extraidos del sumario -> de aquí va a estraer los 20 más
# importantes, o el número de observaciones que yo haya pedido a R'''
AU1 <- S2$MostCitedPapers
AUT <- AU1[1:2]
View(AUT)
View(AU)

#hago lo mismo con los países'''
Paises <- S2$Countries
Paises <- S2$MostProdCountries
View(Paises)
Paises <- Paises[c(1, 3)]

#re nombro la columna paises'''

names(Paises)[1] <- "Country"

#'''esto aplica solo para revistas en español'''
#Paises$Country <- c("USA", "Taiwan", "Korea",
# "Reino Unido", "Alemania", "Holanda", "Italia",
# "Canada", "España", "China")
#quito algonos errores'''
Paises$Freq <- suppressWarnings(as.numeric(Paises$Freq))

#miro los años de producción'''
Produccion <- S2$AnnualProduction
names(Produccion)[1] <- "Year"

#seteo la segunda columna como numerica'''
Produccion$Articles <- as.numeric(Produccion$Articles)



#gphics and y plots-----------------
Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq),y=Freq))+
   geom_bar(stat = "identity", fill="blue")+
   coord_flip()+
   xlab("Country") + ylab("Frequency")

Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles))+
   geom_bar(stat = "identity", fill="blue") + xlab("Year") +
   ylab("Articles") + theme(axis.text.x = element_text(angle = 90,
                                                       hjust = 1))
ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
#graficas y plots por citas y palabras clave
Fig1A <- ggplot(key, aes(x=1 , y=2)) +
   geom_bar(stat = "identity", fill="blue") + coord_flip() +
   xlab("KeyWords") + ylab("Articles")

Fig1B <- ggplot(AUT, aes(x=2 , y=1))+
   geom_bar(stat = "identity",fill="blue")+
   xlab("Paper") + ylab("TotalC")+theme(axis.text.x=element_text(angle = 90,
                                                                 hjust = 1))

ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
plot(Fig1A)
#este trabajo de minería se hace sobre el total de observaciones N3--------
texto = Corpus(VectorSource(S2$MostRelKeywords))
discurso=tm_map(texto, tolower)
discurso =tm_map(discurso, stripWhitespace)
discurso = tm_map(discurso, removePunctuation)
discurso = tm_map(discurso, removeNumbers)
#quitamos palabras genericas y todas las que
# molesten en el resultado. PDT: hay que cambiar la lista de
# palabras que es de otra bibliometría
discurso=tm_map(discurso,removeWords, c(stopwords("english"),"ddimer","ccovid","covid","zhang", "human", "usa", "kingdom", "korea", "institute", "national", "lee", "china", "univ", "cell", "london", "medical","coronavirus", "gene", "chen", "kong", "hong", "infectious", "animal", "wang", "diseases", "veterinary", "center", "centre", "college", "sciences", "school", "protein", "public", "control", "state", "liu", "clinical", "chinese", "van", "department", "affiliated", "united", "universit", "kim", "viral", "california", "vaccine"))
### DATA FRAME DE PALABRAS CON SU FRECUENCIA -------------
letras= TermDocumentMatrix(discurso)
findFreqTerms(letras, lowfreq=1)
matrix=as.matrix(letras)
vector <- sort(rowSums(matrix),decreasing=TRUE)
dataletras <- data.frame(word= names(vector),frequencia=vector)
## GRAFICAMOS FRECUENCIA DE LAS PALABRAS ---------------
barplot(dataletras[1:10,]$freq, las = 2, names.arg = dataletras[1:10,]$word,
        col ="blue", main ="Most Relevant Keywords",
        ylab = "Frecuencia de palabras")
## GRAFICAMOS LA NUBE DE PALABRAS ---------------------------------
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 1,
          max.words=70)
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(7, "Dark2"))
#Este trabajo de minería se hace sobre los keyWords --------------------
texto = Corpus(VectorSource(key))
discurso=tm_map(texto, tolower)
discurso =tm_map(discurso, stripWhitespace)
discurso = tm_map(discurso, removePunctuation)
discurso = tm_map(discurso, removeNumbers)
# quitamos palabras genericas y todas las que
# molesten en el resultado. PDT: hay que cambiar la lista de
# palabras que es de otra bibliometría
discurso=tm_map(discurso, removeWords, c(stopwords("english"), "zhang", "human", "usa", "kingdom", "korea", "institute", "national", "lee", "china", "univ", "cell", "london", "medical","coronavirus", "gene", "chen", "kong", "hong", "infectious", "animal", "wang", "diseases", "veterinary", "center", "centre", "college", "sciences", "school", "protein", "public", "control", "state", "liu", "clinical", "chinese", "van", "department", "affiliated", "united", "universit", "kim", "viral", "california", "vaccine"))
## DATA FRAME DE PALABRAS CON SU FRECUENCIA keywords------------------
letras= TermDocumentMatrix(discurso)
findFreqTerms(letras, lowfreq=5)
matrix=as.matrix(letras)
vector <- sort(rowSums(matrix),decreasing=TRUE)
dataletras <- data.frame(word= names(vector),frequencia=vector)
# GRAFICAMOS FRECUENCIA DE LAS PALABRAS --------
barplot(dataletras[1:30,]$freq, las = 2, names.arg = dataletras[1:30,]$word,
        col ="blue", main ="PALABRAS MÁS FRECUENTES",
        ylab = "Frecuencia de palabras")
# GRAFICAMOS LA NUBE DE PALABRAS-----------
wordcloud(words = dataletras$word, freq = dataletras$freq,
          min.freq = 2,max.words=70)
wordcloud(words = dataletras$word, freq = dataletras$freq,
          min.freq = 5,max.words=150, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(7, "Dark2"))


############################ Text Analysis ###################################

#replacing NA from PAGE COUNT-------------------------
# -------------------------->DUDA1: (SPANISH)
#el conteo de paginas tiene demasiados datos perdidos,
# son muchos 0, no se si valga la pena tener encuenta esta viarable
# o no en el análisis. ---> creo que no, de todas formas la dejé ahí.

pc=newM$page_count
pc[is.na(pc)] <- 0
unlist(pc)
pc=as.numeric(pc)
pc=as.data.frame(pc)

#languajes-----------------------
#now i will recode language row to make it as categorical nominal.
#now i extract the Languages list.
LA=newM$LA
#recode the languages.
LA = recode(LA, "BULGARIAN" = "1","CHINESE"= "2" ,"CZECH" = "3",
            "DANISH" = "4","ENGLISH"= "6" ,"ENGLISH; CHINESE" = "7",
            "ENGLISH; FRENCH" = "8","ENGLISH; GERMAN"= "9",
            "ENGLISH; GREEK" = "10","ENGLISH; PORTUGUESE"="32",
            "ENGLISH; PORTUGUESE; SPANISH" = "11","ENGLISH; RUSSIAN" = "12",
            "ENGLISH; SLOVAK" = "13","ENGLISH; SPANISH" = "14",
            "ENGLISH; UKRAINIAN" = "15","FRENCH" = "16","GERMAN" = "17",
            "DUTCH" = "5","GREEK" = "18", "HUNGARIAN" = "19",
            "ITALIAN" = "20", "JAPANESE" = "21","KOREAN" = "22",
            "NORWEGIAN" = "23","PERSIAN" = "24", "POLISH" = "25",
            "PORTUGUESE" = "26", "RUSSIAN" = "27","SLOVENIAN" = "28",
            "SPANISH" = "29","TURKISH" = "30", "UKRAINIAN" = "31")
LA=as.numeric(LA)
table(LA)
str(LA)
#Document type ----------------------
DT=newM$DT
table(DT)
DT=recode(DT,"ARTICLE"="1","BOOK"="2" ,
          "BOOK CHAPTER"="3","CONFERENCE PAPER"="4",
          "CONFERENCE REVIEW"="5" ,
          "DATA PAPER"="6","EDITORIAL"="7",
          "ERRATUM"="8","LETTER"="9",
          "NOTE"="10","RETRACTED"="11",
          "REVIEW"="12","SHORT SURVEY"="13")
#now i will recode all the publication Year-----------------
table(newM$PY)
PY=newM$PY
PY=recode(PY, "1986"="1","1994"="2","2003"="3","2004"="4","2005"="5",
          "2006"="6","2007"="7","2008"="8","2009"="9","2010"="10","2011"="11",
          "2012"="12","2013"="13","2014"="14","2015"="15","2016"="16",
          "2017"="17","2018"="18","2019"="19","2020"="20","2021"="21",
          "2022"="22","2023"="23")
#Total citations---------------------------
#in order to make a list as numeric, first unlist, after that
#its possible to set as numeric and finally set as df.
TC<-sort(newM$TC, decreasing= T)

#splitting Total citations in categories-------------------

#--------------------------> DUDA 2: (SPANISH)
# Lo más ximo que pude centrar la muestra de citationes fue así como
# está ahí, porque no me había finado pero de 0 citationes hay casi 8 mil
# registros!... siendo la variable dependiente no se si eso sea negativo
# (no lo creo porque de todas formas 0 citationes sigue siendo un número).
#
# A pesar de esto es muy dificil encontrar un centro para que esta variable tenga
# punto de equilibrio, en el table(TCcat) de abajo puedes ver como quedó.
#
# Me cuentas qué opinas al respecto.
unlist(TC)
str(TC)
TC=as.numeric(TC)
TCcat = ifelse(TC <=0, 1, ifelse(TC <= 2, 2, 3))
table(TCcat)

#################### MULTINOMIAL GENERALIZED LINEARL MODEL ####################
#i will change each variable to numeric type.
DT=unlist(DT)
DT=as.numeric(DT)
#Cbind to create my dataframe---------------
modeloA=cbind(TCcat,PY,LA,DT,pc)
modeloA=as.data.frame(modeloA)
#I will create My GLM model.
require(VGAM)
#Modelo con/with Publication YEAR--------------
mcovidPY<- vglm(TCcat~PY+LA+DT+pc,
                family= multinomial,
                data = modeloA)
summary(mcovidPY)
#modelo SIN/Without PUB YEAR--------------
mcovid<- vglm(TCcat~LA+DT+pc,
              family= multinomial,
              data = modeloA)
summary(mcovid)
































#Its just an experiment... related with the code and not at the same time.
#posible loop para contar palabras por titulo------
vector = c()
for(i in 1:length(newM$TI)){
   temp<-(strsplit(newM$TI[[i]], "\\.")[[1]])
   print(temp)
   vector <- c(vector, temp)
}
