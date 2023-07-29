#Step 1: Opening the sample of texts
# this local folder is a clone of the GitHub Repo
setwd("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/Testing")
pathcwd<-getwd()
listado <- data.frame(dir(pathcwd))
listado
library(readtext)
library(tm)

# Get the data directory from readtext
DATA_DIR <- system.file("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/Testing", package = "readtext")
library(readtext)
textos <- readtext(pathcwd,ignore_missing_files = TRUE)
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)

# Step 2: Creating a corpus from texts
library(quanteda)
Textos <- tolower(corpus(textos))
TOKS <- corpus(textos$text) %>% 
  tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% 
  tokens_remove(stopwords("en"))
DTM <- dfm(TOKS, tolower = FALSE)
SoftSkills <- c("generate", "evaluate", "lead", "teams", "analyze", "manage", "strengthen", "identify", "create", "understand") #Eigenvector
toks_inside <- tokens_keep(TOKS, pattern = SoftSkills, window = 0)
DTM2 <- dfm(toks_inside)
DTM3 <- as.matrix(DTM2)
colnames(DTM3)
DTM3
ncol(DTM3)

library(bipartite)
plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")

# For reporting purposes, we changed the names of the columns as follows:
colnames(DTM3)[1:6] <- c("Understanding", "Analytical", "Leadership", "Creativity", "Evaluate", "Management")
# this is the actual  code
#colnames(DTM3)[1:10] <- c("Understanding", "Generate", "Identify", "Analytical", "Strength", "Leadership", "Teamwork", "Creativity", "Evaluate", "Management") 


plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
mod <- computeModules(t(DTM3))
plotModuleWeb(mod)
compart(DTM3)
H2fun(DTM3, H2_integer=TRUE)
networklevel(DTM3, index="ALLBUTDD", level="both", weighted=TRUE, 
             ISAmethod="Bluethgen",  SAmethod = "Bluethgen", extinctmethod = "r", 
             nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE, 
             logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE, 
             fcdist="euclidean", legacy=FALSE)


# SampleAnalysis.R
library(readr)
Muestra <- read_csv("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/data2.csv")
Muestra <- na.omit(Muestra)
table(Muestra$Type_of_Institute)
table(Muestra$Program_Name)
library(tidyverse)
Muestra <- mutate(Muestra, Programa = ifelse(grepl("Bachelor", Program_Name), "Graduate",
                                             ifelse(grepl("Master", Program_Name), "Post Graduate", 
                                                    ifelse(grepl("Ph.D.", Program_Name), "Doctor of Philosophy",
                                                           "PG Diploma"))))
table(Muestra$Programa)

institution <- data.frame(table(Muestra$Institute_Name))
Sector <- data.frame(table(Muestra$Type_of_Institute))
Central <- data.frame(subset(Muestra, Muestra$Type_of_Institute == "Central University"))
LevelsCentral <- data.frame(table(Central$Programa))
Private <- data.frame(subset(Muestra, Muestra$Type_of_Institute == "Private University"))
LevelsPrivate <- data.frame(table(Private$Programa))
State <- data.frame(subset(Muestra, Muestra$Type_of_Institute == "State University"))
LevelsState <- data.frame(table(State$Programa))
Deemed <- data.frame(subset(Muestra, Muestra$Type_of_Institute == "Deemed University"))
LevelsDeemed <- data.frame(table(Deemed$Programa))
Technical <- data.frame(subset(Muestra, Muestra$Type_of_Institute == "Public Technical University"))
LevelsTechnical <- data.frame(table(Technical$Programa))



# Step 3: Tagging the texts according to
# their program type and accreditation
docvars(Textos, "Programa") <- Muestra$Program_Name
docvars(Textos, "Program.Level") <- Muestra$Programa
docvars(Textos, "Institution") <- Muestra$Institute_Name
docvars(Textos, "Duration") <- Muestra$Duration
summary(Textos)
aja <- data.frame(summary(Textos, n = length(Textos)))
SPEC <- corpus_subset(Textos, Program.Level == "Graduate")
MS <- corpus_subset(Textos, Program.Level == "Post Graduate")
PhD <- corpus_subset(Textos, Program.Level == "Doctor of Philosophy")
phd <- data.frame(summary(PhD, n = length(PhD)))
phd



# Step 4. Soft Skills theoretically driven identification
# Keywords-in-context Search
pc <- data.frame(kwic(Textos, pattern = phrase("critical thinking")))
sp <- data.frame(kwic(Textos, pattern = phrase("problem-solving")))
comunicar <- data.frame(kwic(Textos, pattern = "communicate"))
creatividad <- data.frame(kwic(Textos, pattern = "creativity"))
paciencia <- data.frame(kwic(Textos, pattern = "patience"))
crear <- data.frame(kwic(Textos, pattern = "create"))
liderar <- data.frame(kwic(Textos, pattern = "lead"))
resolver <- data.frame(kwic(Textos, pattern = "solve"))
comprometer <- data.frame(kwic(Textos, pattern = "commit"))
comprometerse <- data.frame(kwic(Textos, pattern = "commit oneself"))
gestionar <- data.frame(kwic(Textos, pattern = "manage"))
reflexionar <- data.frame(kwic(Textos, pattern = "reflect"))
controlar <- data.frame(kwic(Textos, pattern = "control"))
etico <- data.frame(kwic(Textos, pattern = "ethical"))
tolerar <- data.frame(kwic(Textos, pattern = "tolerate"))
argumentar <- data.frame(kwic(Textos, pattern = "argue"))
conflicto <- data.frame(kwic(Textos, pattern = "conflicts"))
negociar <- data.frame(kwic(Textos, pattern = "negotiate"))
comprender <- data.frame(kwic(Textos, pattern = "understand"))
equipo <- data.frame(kwic(Textos, pattern = "teams"))
planificar <- data.frame(kwic(Textos, pattern = "plan"))
generar <- data.frame(kwic(Textos, pattern = "generate"))
empatia <- data.frame(kwic(Textos, pattern = "empathy"))
compartir <- data.frame(kwic(Textos, pattern = "share"))
analizar <- data.frame(kwic(Textos, pattern = "analyze"))
reconocer <- data.frame(kwic(Textos, pattern = "recognize"))
orientar <- data.frame(kwic(Textos, pattern = "guide"))
respetar <- data.frame(kwic(Textos, pattern = "respect"))
motivar <- data.frame(kwic(Textos, pattern = "motivate"))
cooperar <- data.frame(kwic(Textos, pattern = "cooperate"))
fortalecer <- data.frame(kwic(Textos, pattern = "strengthen"))
impulsar <- data.frame(kwic(Textos, pattern = "drive"))
acercar <- data.frame(kwic(Textos, pattern = "bring closer"))
ayudar <- data.frame(kwic(Textos, pattern = "help"))
cambiar <- data.frame(kwic(Textos, pattern = "change"))
apreciar <- data.frame(kwic(Textos, pattern = "appreciate"))
dirigir <- data.frame(kwic(Textos, pattern = "direct"))
fomentar <- data.frame(kwic(Textos, pattern = "promote"))
interactuar <- data.frame(kwic(Textos, pattern = "interact"))
identificar <- data.frame(kwic(Textos, pattern = "identify"))
competir <- data.frame(kwic(Textos, pattern = "compete"))
manifestar <- data.frame(kwic(Textos, pattern = "express"))
responsable <- data.frame(kwic(Textos, pattern = "responsible"))
evaluar <- data.frame(kwic(Textos, pattern = "evaluate"))
innovar <- data.frame(kwic(Textos, pattern = "innovate"))
decidir <- data.frame(kwic(Textos, pattern = "decide"))
td <- data.frame(kwic(Textos, pattern = phrase("make decisions")))
flex <- data.frame(kwic(Textos, pattern = "flexibility"))
persu <- data.frame(kwic(Textos, pattern = "persuade*"))
conven <- data.frame(kwic(Textos, pattern = "convince"))

rm(institution, LevelsCentral,
   LevelsPrivate, LevelsDeemed, LevelsState, LevelsTechnical, listado, Muestra,
   Central, Private, Deemed, State, Technical, Sector, Textos,
   Textos, DATA_DIR)
TODAS <- rbind(persu, conven, flex, td, decidir, sp,
               pc, creatividad, paciencia, crear,
               innovar, acercar, analizar, apreciar,
               argumentar, ayudar, cambiar, compartir,
               competir, comprender, comprometer,
               comprometerse, comunicar, conflicto,
               controlar, cooperar, dirigir, empatia,
               equipo, etico, evaluar, fomentar, fortalecer,
               generar, gestionar, identificar, impulsar,
               interactuar, liderar, manifestar, motivar,
               negociar, orientar, planificar, reconocer,
               reflexionar, resolver, respetar,
               responsable, tolerar)

colnames(aja)[1] <- "docname"
library(dplyr)
TODAS2 <- TODAS %>%
  select(-from, -to, -pre, -post, -pattern) %>%
  left_join(aja, by = "docname")
Spec <- TODAS2 %>% filter(., Program.Level == "Graduate")
MS <- TODAS2 %>% filter(., Program.Level == "Post Graduate")
PhD <- TODAS2 %>% filter(., Program.Level ==  "Doctor of Philosophy")




# Step 5. Plotting results
load("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/DataForFigure4AFinal.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS[TODAS=="acercar"] <- "S1"
TODAS[TODAS=="analizar"] <- "S2"
TODAS[TODAS=="argumentar"] <- "S3"
TODAS[TODAS=="ayudar"] <- "S4"
TODAS[TODAS=="cambiar"] <- "S5"
TODAS[TODAS=="compartir"] <- "S6"
TODAS[TODAS=="competir"] <- "S7"
TODAS[TODAS=="comprender"] <- "S8"
TODAS[TODAS=="comprometerse"] <- "S9"
TODAS[TODAS=="comunicar"] <- "S10"
TODAS[TODAS=="conflictos"] <- "S11"
TODAS[TODAS=="controlar"] <- "S12"
TODAS[TODAS=="crear"] <- "S13"
TODAS[TODAS=="creatividad"] <- "S14"
TODAS[TODAS=="decidir"] <- "S15"
TODAS[TODAS=="dirigir"] <- "S16"
TODAS[TODAS=="empatıa"] <- "S17"
TODAS[TODAS=="equipos"] <- "S18"
TODAS[TODAS=="´etico"] <- "S19"
TODAS[TODAS=="evaluar"] <- "S20"
TODAS[TODAS=="flexibilidad"] <- "S21"
TODAS[TODAS=="fomentar"] <- "S22"
TODAS[TODAS=="fortalecer"] <- "S23"
TODAS[TODAS=="generar"] <- "S24"
TODAS[TODAS=="gestionar"] <- "S25"
TODAS[TODAS=="identificar"] <- "S26"
TODAS[TODAS=="impulsar"] <- "S27"
TODAS[TODAS=="innovar"] <- "S28"
TODAS[TODAS=="interactuar"] <- "S29"
TODAS[TODAS=="liderar"] <- "S30"
TODAS[TODAS=="manifestar"] <- "S31"
TODAS[TODAS=="motivar"] <- "S32"
TODAS[TODAS=="orientar"] <- "S33"
TODAS[TODAS=="pensamiento crıtico"] <- "S34"
TODAS[TODAS=="persuasi´on"] <- "S35"
TODAS[TODAS=="planificar"] <- "S36"
TODAS[TODAS=="reconocer"] <- "S37"
TODAS[TODAS=="reflexionar"] <- "S38"
TODAS[TODAS=="resolver"] <- "S39"
TODAS[TODAS=="respetar"] <- "S40"
TODAS[TODAS=="responsable"] <- "S41"
TODAS[TODAS=="solucionar problemas"] <- "S42"
TODAS[TODAS=="tomar decisiones"] <- "S43"



# Figure 4 Panel A
Network <- TODAS[,c(1,5)]
table(Network$keyword)
Network <- Network[!duplicated(Network[c(1,2)]),]
library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"
bn2.pr <- bipartite.projection(bn2)
Terms <- bn2.pr$proj2
centrality_scores <- degree(Terms)
CS <- centrality_scores
# Normalize the centrality scores to a range between 0 and 1,
# as follows:
# centrality_scores - min(centrality_scores) (in the numerator)
# (max(centrality_scores) - min(centrality_scores) (in the denominator)
normalized_scores <- (CS - min(CS)) / (max(CS) - min(CS))
normalized_scores

# Create a color palette with different colors
color_palette <- colorRampPalette(c("red", "pink", "lightgreen", "green"))
(length(unique(normalized_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette(rank(normalized_scores))

# Plot the network with node colors based on centrality
plot(Terms, vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.color = node_colors,
     vertex.size = 15, edge.width = 0.5,
     edge.color = "lightgray",
     layout = layout_components, 
     main = "")


load("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/DataForFigure4AFinal.RData")
#rm(list=setdiff(ls(), "TODAS2"))
TODAS2$keyword <- tolower(TODAS2$keyword)
library(dplyr)
SpecPrograms <- TODAS2 %>% filter(., Program.Level=="Graduate") %>% 
  select(., docname, keyword)
MasterPrograms <- TODAS2 %>% filter(., Program.Level=="Post Graduate") %>% 
  select(., docname, keyword)
DoctoratePrograms <- TODAS2 %>% filter(., Program.Level == "Doctor of Philosophy") %>% 
  select(., docname, keyword)


library(igraph)
BNS <- graph.data.frame(SpecPrograms, directed = FALSE)
BNM <- graph.data.frame(MasterPrograms, directed = FALSE)
BND <- graph.data.frame(DoctoratePrograms, directed = FALSE)


Spec <- data.frame(Degree = igraph::degree(BNS),
                   Closeness = igraph::closeness(BNS),
                   Betweennes = igraph::betweenness(BNS),
                   Eigen = igraph::eigen_centrality(BNS))
Spec <- Spec[ -c(5:25) ]
rownames(Spec)
Spec <- Spec[97:135,]
Spec$SS <- rownames(Spec)
Spec$Level <- "Specialization"

MS <- data.frame(Degree = igraph::degree(BNM),
                 Closeness = igraph::closeness(BNM),
                 Betweennes = igraph::betweenness(BNM),
                 Eigen = igraph::eigen_centrality(BNM))
MS <- MS[ -c(5:25) ]
rownames(MS)
MS <- MS[83:118,]
MS$SS <- rownames(MS)
MS$Level <- "Master"


Doc <- data.frame(Degree = igraph::degree(BND),
                  Closeness = igraph::closeness(BND),
                  Betweennes = igraph::betweenness(BND),
                  Eigen = igraph::eigen_centrality(BND))
Doc <- Doc[ -c(5:25) ]
rownames(Doc)
Doc <- Doc[26:58,]
Doc$SS <- rownames(Doc)
Doc$Level <- "Doctorate"

Centralities <- list(Spec, MS, Doc)
Centralities <- as.data.frame(do.call(rbind, Centralities))

Resumen <- data.frame(table(Centralities$SS))

library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(Centralities, aes(x = Eigen.vector, y = Level, fill = Level)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Academic Program") + 
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=20)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))


IM <- as_incidence_matrix(BNS, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNS)$type)
IM2 <- as.matrix(IM)

bipartite.mapping(BNS)
V(BNS)$type <- bipartite_mapping(BNS)$type
V(BNS)$color <- ifelse(V(BNS)$type, "red", "green")
V(BNS)$shape <- ifelse(V(BNS)$type, "circle", "square")
V(BNS)$label.cex <- ifelse(V(BNS)$type, 0.8, 1)
V(BNS)$size <- sqrt(igraph::degree(BNS))
E(BNS)$color <- "lightgrey"
plot(BNS, 
     vertex.label = NA, 
     layout = layout_in_circle, 
     main = "")

library(bipartite)
plotweb(IM2, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
bipartite::visweb(IM2)


library(GGally)

table(Resumen$Freq)
library(dplyr)
SoftSkillsCentrality <- Centralities %>% filter(., grepl('analizar|ayudar|compartir|competir|comprender|comunicar|crear|creatividad|dirigir|equipos|ético|evaluar|flexibilidad|fomentar|fortalecer|generar|gestionar|identificar|impulsar|innovar|interactuar|liderar|orientar|pensamiento crítico|persuasión|planificar|reconocer|reflexionar|resolver|responsable|tomar decisiones', SS))

SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'generar'] <- 'Generate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'creatividad'] <- 'Creativity'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'crear'] <- 'Create'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'liderar'] <- 'Leadership'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'identificar'] <- 'Identify'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'analizar'] <- 'Analytical'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'resolver'] <- 'Solving'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'evaluar'] <- 'Evaluate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'equipos'] <- 'Teamwork'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'gestionar'] <- 'Management'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'dirigir'] <- 'Addressing'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'tomar decisiones'] <- 'Decision Making'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'reconocer'] <- 'Acknowledge'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'innovar'] <- 'Innovate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'responsable'] <- 'Accountability'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'pensamiento crítico'] <- 'Critical Thinking'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'comprender'] <- 'Understanding'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'ético'] <- 'Ethical Thinking'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'fortalecer'] <- 'Strength'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'orientar'] <- 'Guidance'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'compartir'] <- 'Sharing'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'fomentar'] <- 'Foment'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'interactuar'] <- 'Social Interaction'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'comunicar'] <- 'Communication'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'flexibilidad'] <- 'Flexibility'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'reflexionar'] <- 'Thoughtfulness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'ayudar'] <- 'Helping others'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'persuasión'] <- 'Persuasiveness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'impulsar'] <- 'Thrust'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'competir'] <- 'Competitiveness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'planificar'] <- 'Planning'


dat <- SoftSkillsCentrality[1:5]
options(scipen = 999)
dat <- SoftSkillsCentrality %>% filter(., Level == "Doctorate")

library(datawizard)
dat <- dat %>% mutate(., degree.rescaled = ifelse(Degree == 0, 0.00, rescale(dat$Degree, to = c(0,1))))
dat <- dat %>% mutate(., closeness.rescaled = ifelse(Closeness == 0, 0.00, rescale(dat$Closeness, to = c(0,1))))
dat <- dat %>% mutate(., betweennes.rescaled = ifelse(Betweennes == 0, 0.00, rescale(dat$Betweennes, to = c(0,1))))
dat <- dat %>% mutate(., eigenvector.rescaled = ifelse(Eigen.vector == 0, 0.00, rescale(dat$Eigen.vector, to = c(0,1))))

summary(dat$eigenvector.rescaled)
summary(dat$degree.rescaled)
summary(dat$betweennes.rescaled)
summary(dat$closeness.rescaled)
colnames(dat)


p1 <- ggplot(dat, aes(x = reorder(SS, degree.rescaled), y = degree.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Degree Centrality (rescaled 0-1)")

p2 <- ggplot(dat, aes(x = reorder(SS, closeness.rescaled), y = closeness.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Closeness Centrality (rescaled 0-1)")

p3 <- ggplot(dat, aes(x = reorder(SS, betweennes.rescaled), y = betweennes.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() +
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Betweenness Centrality (rescaled 0-1)")

p4 <- ggplot(dat, aes(x = reorder(SS, eigenvector.rescaled), y = eigenvector.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Eigenvector Centrality (rescaled 0-1)")



library(ggpubr)
figure <- ggarrange(p1, p2, p3, p4, 
                    labels = c("(A)", "(B)", "(C)", "(D)"),
                    ncol = 2, nrow = 2)

figure


# Figure 4 Panel B
load("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/DataForFigure4BFinal.RData")
#rm(list=setdiff(ls(), "SkillsProgramsCentrality"))
library(psych)
pairs.panels(SkillsProgramsCentrality,
             method = "spearman",
             hist.col = "green",
             density = TRUE,
             ellipses = TRUE,
             pch = 21,
             cex = 2.5,
             cex.axis = 1.8,
             cex.labels = 4.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE
)



# Figure 4 Panel C
load("C:/Users/SHIVAM/OneDrive/Desktop/ResearchPaper/DataForFigure4BFinal.RData")
#rm(list=setdiff(ls(), "DTM3"))
# The DTM3 object is a matrix with 10 columns (with the soft skills
# that proved to be more central and all programs as rows. In this
# matrix several programs don’t have a connection with any of these
# central skills. Thus, we will discard these programs to decpict
# a bipartite Network for illustrative purposes.)
DTM4 <- apply(DTM3, 1, function(row) any(row != 0))
BN <- DTM3[DTM4, ]
library(bipartite)
plotweb(BN, method = "normal",
        col.high = "lightgreen",
        bor.col.high = "lightgreen",
        col.low = "pink",
        bor.col.low = "pink",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 2)

#Figure 5a and 5B

library(ggplot2)
library(ggridges)
p1 <- ggplot(Centralities, aes(x = Eigen.vector, y = Level, fill = Level)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Academic Program") + 
  theme(axis.text.x=element_text(size=35)) +
  theme(axis.text.y=element_text(size=35)) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=35)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=35))


#p2 <- ggplot(Centralities, aes(x = Eigen.vector, y = Accreditation, fill = Accreditation)) +
  #geom_density_ridges(alpha = 0.3) +
  #theme_ridges() + 
 # theme(legend.position = "none") + 
 # xlab("Eigenvector Centrality") + 
 # ylab("Type of Accreditation") + 
  #theme(axis.text.x=element_text(size=35)) +
 #theme(axis.text.y=element_text(size=35)) +
  #theme(axis.title.x=element_text(face="italic", colour="black", size=35)) +
  #theme(axis.title.y=element_text(face="italic", colour="black", size=35))


library(ggpubr)
ggarrange(p1, labels = c("(A)", "(B)"), ncol = 1, nrow = 2)