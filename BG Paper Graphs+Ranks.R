library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

setwd
setwd("D:/OneDrive - The University of Sydney (Students)/Desktop/PhD Work/Blue Groper Paper - 2022/Data Analysis")

##Blue Groper Data Processing
BGmaxn <- read.csv("BermaguiMaxn.csv")

BGmaxn <- BGmaxn[-1]
colnames(BGmaxn)[4] <- c("ScientificName")
BGmaxn <- BGmaxn %>%
  mutate(Species = replace(Species, Species=="jackoniensis", "jacksoniensis"))

##Fixing Tamar Goby
filter <- BGmaxn %>%
  filter(Species == "tamarensis")

filter$Genus <- "Afurcagobius"
filter <- filter %>%
  unite(ScientificName, c(Genus, Species), sep= " ", remove= FALSE)

BGmaxn <- BGmaxn %>%
  filter(Species != "tamarensis")
BGmaxn <- rbind(BGmaxn, filter)

##updating the crested oyster goby columns as there are some errors in the entries
filter <- BGmaxn %>%
  filter(ScientificName=="Cryptocentroides gobioides")
filter$Species <- "gobiodes"
#removing problem rows out of the dataset
BGmaxn <- BGmaxn %>%
  filter(ScientificName != "Cryptocentroides gobioides")
#adding fixed rows back in 
BGmaxn <- rbind(BGmaxn,filter)

BGmaxn <- BGmaxn %>%
  unite(ScientificName, c(Genus,Species), sep=" ", remove= FALSE)


##Adding in species codes into the dataset
codes <- read_xlsx("SpeciesList+Codes.xlsx")

codes <- codes %>%
  unite(ScientificName, c(Genus, Species), sep= " ", remove= FALSE)

BGmaxn <- merge(x=BGmaxn, y=codes, by= "ScientificName", all.x=TRUE, all.y=FALSE)
BGmaxn <-BGmaxn[c(-9,-10,-11)]
colnames(BGmaxn)[4] <- "Family"
colnames(BGmaxn)[5] <- "Genus"
colnames(BGmaxn)[6] <- "Species"

BGmaxn <- BGmaxn %>%
  select("ScientificName","Family","Genus","Species","MaxN","Common", "Species Code", "SurveyID")
BGmaxn$Site <- "BG-BR"
BGmaxn$Season <- "Summer"
BGmaxn$Year <- "2021"
colnames(BGmaxn)[7] <- "SpeciesCode"



##Loading in MaxN Dataset
MaxN <- read.csv("MaxN+ReefMetrics.csv")
MaxN <- MaxN %>%
  select("ScientificName","Family.x","Genus.x","Species.x","MaxN","Common", "SpeciesCode", "SurveyID", "Season", "Site", "Year")
colnames(MaxN)[2] <- c("Family")
colnames(MaxN)[3] <- "Genus"
colnames(MaxN)[4] <- "Species"

MaxN <- rbind(BGmaxn, MaxN)

MaxN <- MaxN %>%
  filter(Season=="Summer") %>%
  filter(Site != "SY-PH")

filter <- MaxN %>%
  filter(Common=="Threadfin Butterflyfish")
filter$Species <- "auriga"
filter$Genus <- "Chaetodon"
filter$ScientificName <- "Chaetodon auriga"

MaxN <- MaxN %>%
  filter(Common !="Threadfin Butterflyfish")

MaxN <- rbind(MaxN, filter)


##Analysis
library(fishualize)
fishapes()
fishapes()

##Bermagui
bg <- MaxN %>%
  filter(Site=="BG-BR")

bgmaxn <- bg %>%
  group_by(ScientificName, Common) %>%
  summarise(sumMaxN = sum(MaxN))

bgmaxn <- bgmaxn %>%
  filter(Common != "Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish")

bgmaxn10 <- bgmaxn %>%
  filter(Common %in% c("Luderick","Sea Mullet","Yellowfin Bream","Yellowfin Leatherjacket","Oyster Blenny",
                       "Easten Blue Groper","Exquisite Sandgoby","Sand Whiting","Silver Trevally", "Stripey"))

bgplot <- ggplot(data=bgmaxn10, aes(x=reorder(ScientificName,-sumMaxN), y=sumMaxN,
                        fill=factor(if_else(ScientificName=="Achoerodus viridis","Highlighted","Normal"))))+
  geom_bar(stat="identity")+
  scale_fill_manual(name= "Common", values=c("#0072B2","grey50"))+
  ylab("Total Abundnace (Sum of MaxN)")+
  xlab("Species")+
  guides(fill="none")+
  ggtitle("Bermagui")+
  labs(tag="A)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  # add_fishape(family="Labridae",
  #             option = "Symphodus_melops")
bgplot
Scarus_rivulatus
Scarus_oviceps

fishapes()
cr <- MaxN %>%
  filter(Site=="OP-CR") %>%
  filter(Year=="2021")

crmaxn <- cr %>%
  group_by(ScientificName,Common)%>%
  summarise(sumMaxN = sum(MaxN)) %>%
  filter(Common !="Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish") %>%
  filter(Common != "Ogilby's Hardyhead")

#number of ranks
unique(crmaxn$sumMaxN)

crmaxn$Rank <- rank(crmaxn$sumMaxN, ties.method = "max")


crmaxn10 <- crmaxn %>%
  filter(Common %in% c("Luderick", "Yellowfin Bream", "Oyster Blenny", "Sea Mullet", "Silver Sweep", "Eastern Fortescue","Fan-bellied leatherjacket",
                          "Easten Blue Groper", "Common Silverbiddy", "Exquisite Sandgoby"))

crplot <- ggplot(data=crmaxn10, aes(x=reorder(ScientificName,-sumMaxN), y=sumMaxN,
                        fill=factor(if_else(ScientificName=="Achoerodus viridis","Highlighted","Normal"))))+
  geom_bar(stat="identity")+
  scale_fill_manual(name= "Common", values=c("#0072B2","grey50"))+
  ylab("Total Abundnace (Sum of MaxN)")+
  xlab("Species")+
  guides(fill="none")+
  labs(tag="B)")+
  ggtitle("Crookhaven")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
crplot

tp <- MaxN %>%
  filter(Site=="SY-TP") %>%
  filter(Year=="2021")

tpmaxn <- tp %>%
  group_by(ScientificName,Common)%>%
  summarise(sumMaxN = sum(MaxN)) %>%
  filter(Common !="Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish")  %>%
  filter(Common != "Ogilby's Hardyhead")

tpmaxn10 <- tpmaxn %>%
  filter(Common %in% c("Luderick","Yellowfin Bream","Oyster Blenny","Sand Grey Mullet","Sea Mullet","Stripey",
                       "Exquisite Sandgoby","Sand Whiting","Largemouth Goby","Rotund Blenny","Easten Blue Groper"))

tpplot <- ggplot(data=tpmaxn10, aes(x=reorder(ScientificName,-sumMaxN), y=sumMaxN,
                        fill=factor(if_else(ScientificName=="Achoerodus viridis","Highlighted","Normal"))))+
  geom_bar(stat="identity")+
  scale_fill_manual(name= "Common", values=c("#0072B2","grey50"))+
  ylab("Total Abundnace (Sum of MaxN)")+
  xlab("Species")+
  guides(fill="none")+
  ggtitle("Towra Point")+
  labs(tag="C)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
tpplot

library(grid)
library(gridExtra)
library(cowplot)
library(lemon)
finalplot <- grid.arrange(bgplot, crplot, tpplot, nrow=3, ncol=1)
finalplot <- grid.arrange(finalplot,
                   left= textGrob("Total Abundance (Sum of MaxN)", rot=90, vjust=0.5))
textGrob
bg <- merge(bg, bgmaxnAV, by="Common")


sp.1 <- split(bg, bg$Common)
str(sp.1)
res <- list()

for(n in names(sp.1)){
  dat <- sp.1[[n]]
  res[[n]] <- data.frame(Common=n,
                         sd=sqrt(sum(dat$MaxN-dat$AvMaxN)^2/18))
  }
res

sdbg <- do.call(rbind,res)
print(sdbg)

sp %>% 
  lapply(sqrt(MaxN))
