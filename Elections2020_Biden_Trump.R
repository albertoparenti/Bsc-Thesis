
setwd("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS")

# carico i codici di connessione a twitter
# source("./funzioni/conn_API_tw.R")
library(rtweet)
library(dplyr)
library(RColorBrewer)
library(ngram)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(syuzhet)
library(udpipe)

source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/funzioniTesto.R")
source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/sentimentFunctions.R")
source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/new_funzioni.R")

# caricamento modelli per lemmatizzazione e pos tagging con udpipe
# ud_model_I <- udpipe_load_model("./funzioni/italian-isdt-ud-2.4-190531.udpipe")
# ud_model_V <- udpipe_load_model("./funzioni/italian-vit-ud-2.4-190531.udpipe")
# ud_model_T <- udpipe_load_model("./funzioni/italian-postwita-ud-2.4-190531.udpipe")
# ud_model_P <- udpipe_load_model("./funzioni/italian-partut-ud-2.4-190531.udpipe")
ud_model_EN <- udpipe_download_model("english-ewt-ud-2.4-190531.udpipe")
# scarico archivio con il modello per lemmatizzazione in inglese da udpipe
ud_model_EN <- udpipe_load_model("./funzioni/english-ewt-ud-2.4-190531.udpipe")

appname <- "Elections2020_Biden_Trump"
consumer_key <- "QTUQqQsBqanpDaq9GFm520PHN"
consumer_secret <- "92cTOWt0buUWRldvLXep9ydy9dhqxbyuBRZjLHnu1ed7thbs3z"
access_token <- "1262757204035162119-GEiFYnzwFD4NneIndMDzdIO08qqEcq"
access_secret <- "80EczH28nz6Gimnw3iPDPXnzTrYgp9lIfq3dz0X4L1rtb"

twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

twT <- search_tweets("trump",n=20000,geocode = lookup_coords("usa"),include_rts = F)
twTrump20201009 <- twT
save(twTrump20201010e,file = "twTrump20201010e.RData")
twB <- search_tweets("biden",n=20000,geocode = lookup_coords("usa"),include_rts = F)
twBiden20201009 <- twB
save(twBiden20201010e,file = "twBiden20201010e.RData")

tmp <- twB[!twB$status_id %in% twT$status_id,] # tweet su Biden che non sono presenti in trump

grep("trump",twB$text,ignore.case = T) # quali record di Biden contengono anche la parola trump
grep("biden",twT$text,ignore.case = T) # quali record di trump contengono anche la parola biden

head(twB[grep("trump",twB$text,ignore.case = T),"text"]) # testi dei primi 6 tweet di biden che contengono anche la parola trump

summary(twB$created_at)
summary(twT$created_at)

# controllo del rate limit e del tempo rimanente prima dell'eventuale riattivazione
rr <- rate_limits()
rr[grep("search/tweets",rr$query),]

# -----------------------------------------
tw <- search_tweets("#Elections2020",n=2000,geocode = lookup_coords("usa"),lang="en",include_rts = F)

tw$text[1:10]
tw$text2 <- tw$text
# correzione apostrofi/
# serve nella lemmatizzazione: udpipe ha problemi nel distinguere ’ e  '
grep(intToUtf8(8217),twT$text,value = T)[1:3]  #          "It’s"
grep(intToUtf8(39),twT$text,value = T)[1:3]    #          "Trump's"
tw$text2 <- gsub(intToUtf8(8217),intToUtf8(39),tw$text2)

# pulizia del testo
# in questo caso elimino la punteggiatura, mantengo i numeri, non trasformo tutto in minuscolo e mantengo gli hashtag
tw$txt <- cleanTesto(tw$text2, punteggiatura = T, numeri = F, minuscolo = F, hashtag = F)
tw$txt[1:10]
tw$txt <- gsub("amp","",tw$txt)

# ricerca forme composte
visNGram(tw$txt,ngrF = 4,nn = 20)

# vettore con correzioni delle forme composte (vedi note alla funzione in new_funzioni.R)
correz <- c("Joe Biden",NA,
            "Vice President",NA,
            "United States",NA)
tw$txt <- corFrmComp(vText = tw$txt, correzioni=correz)

# ------------------- lemmatizzazione -------------------
# identifico quante parole in ogni testo
tw$id <- 1:nrow(tw)
tw$nparole <- sapply(tw$txt,wordcount)
summary(tw$nparole)
# ad esempio decido di considerare solo i testi con almeno 10 parole
twr <- tw[tw$nparole>10,]
twr$id <- 1:nrow(twr) # aggiungo un identificato progressivo ai testi selezionati

# lemmatizzazione con udpipe
# attenzione: tra le stopwords inglesi, nella libreria tm (tm::stopwords("en")), ci sono anche negazioni (not, nor,...),
# intensificatori (very more) o de-intensificatori (few) che potrebbero essere utili in fase di sentiment analysis
# potrebbe quindi essere opportuno definirsi un proprio vettore di stopwords

outUV <- lemmaUDP(twr$txt,model = ud_model_EN,doc_id = twr$id, stopw = tm::stopwords("en"))

head(outUV[,c(1:3,5:9,15)],20) # contenuto dell'output della lemmatizzazione
# sarebbe necessario trovare una tabella con l'esatta descrizione dei codci upos e xpos utilizzati da udpipe
# vedere https://universaldependencies.org/treebanks/en_ewt/index.html
table(outUV$upos)
table(outUV$xpos)
table(outUV$xpos,outUV$upos)

head(outUV[outUV$xpos=="PRP",c(1:3,5:9,15)],10)
head(outUV[outUV$xpos=="DT",c(1:3,5:9,15)],10)
head(outUV[outUV$upos=="SCONJ",c(1:3,5:9,15)],10)
head(outUV[outUV$upos=="CCONJ",c(1:3,5:9,15)],10)

# ricreo il vettore con i testi lemmatizzati
# problema: in questo caso vengono esclusi i lemmi che sono stopwords, 
#           ci sono upos, non tra le stopwords, che possono essere esclusi?
#           in altri termini: quali sono gli upos che possono essere rilevanti nella sentiment?
txtL_UV <- outUV %>%
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = " "))
txtL_UV
# elimino il simbolo # dai testi
txtL_UV$txtL <- gsub(" # "," ",txtL_UV$txtL)

# versione con eliminazione degli xpos e upos "inutili"
xpos_del <- c("PRP","DT")
upos_del <- c("SCONJ","CCONJ")
txtL_UV2 <- outUV %>%
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE & !outUV$xpos %in% xpos_del & !outUV$upos %in% upos_del) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = " "))
txtL_UV2
txtL_UV2$txtL <- gsub(" # "," ",txtL_UV2$txtL)

mystpw <- c("elections2020","election2020","election","2020","vote")
risIDF2 <- creaTDM(tolower(txtL_UV2$txtL), lemmatiz = F, mystopwords = mystpw, output = "all",
                   weight = "tfidf", lang = "english")
names(risIDF2)
head(risIDF2$freq.Frm)
library(ggsci) # libreria con tavolozze di colori
par(mar=c(0,0,0,0))
wordcloud(risIDF2$freq.Frm$forme,risIDF2$freq.Frm$freq,
          max.words = 125,random.order = F, scale = c(2, 0.3),
          colors = rev(pal_lancet("lanonc")(4)))

# cluster
crps <- Corpus(VectorSource(tolower(txtL_UV2$txtL)))
crps <- tm_map(crps,removeNumbers)
crps <- tm_map(crps,removeWords,(tm::stopwords("english")))
ndtm <- TermDocumentMatrix(crps)
ndtm <- weightTfIdf(ndtm)
ndtm2 <- removeSparseTerms(ndtm,sparse = 0.98)
library(proxy)
distA <- dist(as.matrix(ndtm2),method = "cosine")
hhA <- hclust(distA,method = "ward.D2")
plot(hhA, hang = -1,sub = "", cex=0.7,
     main="Dendrogramma Cluster Gerarchica",
     cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.6)
library(igraph)
library(factoextra)
gg <- fviz_dend(hhA, k = 10, # Cut in four groups
                k_colors = "jco",
                type = "phylogenic", repel = TRUE,
                phylo_layout = "layout.gem")
gg+ggtitle("Dendrogramma")


library(qdap)
qdapDictionaries::negation.words
qdapDictionaries::amplification.words
qdapDictionaries::deamplification.words


# ---------------- SENTIMENT ANALYSIS ----------------

dfsyuz <- get_sentiment_dictionary()
colnames(dfsyuz) <- c("term","polarity")
scoresUI <- txt_sentiment(x = outUV[,c(1:3,5,6,7,8)],
                          term = "lemma",
                          polarity_terms = dfsyuz)

# versione alternativa indicando vettori con negazioni, amplifiers e deamplifiers
# in questo caso si debbono aggiustare le stopwords in fase di lemmatizzazione e 
# debbono essere definti i vettori di negazioni, amplifiers e deamplifiers
scoresUI <- txt_sentiment(x = outUV[,c(1:3,5,6,7,8)],
                          term = "lemma",
                          polarity_terms = dfsyuz,
                          polarity_negators = qdapDictionaries::negation.words,
                          polarity_amplifiers = qdapDictionaries::amplification.words,
                          polarity_deamplifiers = qdapDictionaries::deamplification.words,
                          n_before=4,n_after=3)


head(scoresUI$overall)
par(mar=c(2.5,2.5,2,1))
hist(scoresUI$overall$sentiment_polarity)
summary(scoresUI$overall$sentiment_polarity)
summary(twr$created_at)

tmp <- data.frame(tempo=twr$created_at,sentiment=scoresUI$overall$sentiment_polarity,giorno=as.Date(twr$created_at))
ggplot(tmp, aes(x=tempo, y=sentiment)) +
  geom_line(colour="blue") +
  theme_bw()+ggtitle("sentiment nel tempo")
tmp %>% ts_plot("hour")+ggtitle("frequenza tweet")+theme_bw()

# sentiment media per giorno
tapply(tmp$sentiment,tmp$giorno,mean)

# stesso metodo ma con altra libreria di sentiment
dfaffi <- get_sentiment_dictionary('afinn')
colnames(dfaffi) <- c("term","polarity")
scoresUI <- txt_sentiment(x = outUV[,c(1:3,5,6,7,8)],
                          term = "lemma",
                          polarity_terms = dfaffi)


# ----- metodo alternativo di calcolo -----
syuz_sent <- get_nrc_sentiment(txtL_UV2$txtL)
valence <- (syuz_sent[, 9]*-1) + syuz_sent[, 10]
hist(valence)
tmp2 <- data.frame(tempo=twr$created_at,sentiment=valence,giorno=as.Date(twr$created_at))
ggplot(tmp2, aes(x=tempo, y=sentiment)) +
  geom_line(colour="blue") +
  theme_bw()+ggtitle("sentiment nel tempo - syuzhet")

simple_plot(valence,
            title = "Interpolazione dell'andamento della sentiment")

percent_vals <- get_percentage_values(valence, bins = 50)
ggplot(mapping=aes(x=seq_along(percent_vals), y=percent_vals)) +
  geom_line(col="blue")+
  theme_light()+
  ylab("Emotional Valence") +
  xlab("Narrative time") +
  ggtitle("Andamento del sentiment per gruppi di 20 tweet")
