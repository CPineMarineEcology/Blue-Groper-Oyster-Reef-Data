###Loading in packages
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

#loading in Blue Groper Data
MaxN <- read.csv("MaxNDataset.csv")

####################Loading in Bermagui Data
bg <- MaxN %>%
  filter(Site=="BG-BR")

#calculating the sum of the MaxN
bgmaxn <- bg %>%
  group_by(ScientificName, Common) %>%
  summarise(sumMaxN = sum(MaxN))

#removing hyperabundants from the dataset
bgmaxn <- bgmaxn %>%
  filter(Common != "Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish")

####################Ranking Bermagui Species
#number of ranks
unique(bgmaxn$sumMaxN)

bgmaxn$Rank <- rank(bgmaxn$sumMaxN, ties.method = "max")

####################Bermagui MaxN Plot - top 10 species
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
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1, size=8),
        axis.title.y = element_blank(),
        axis.title.x= element_blank(),
        axis.text.y= element_text(size=6),
        axis.title = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size=8, face="bold"))

####################Loading in Crookhaven Data
cr <- MaxN %>%
  filter(Site=="OP-CR") %>%
  filter(Year=="2021")

#calculating the sum of the MaxN
crmaxn <- cr %>%
  group_by(ScientificName,Common)%>%
  summarise(sumMaxN = sum(MaxN)) %>%
  filter(Common !="Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish") %>%
  filter(Common != "Ogilby's Hardyhead")

####################Ranking Crookhaven Species
#number of ranks
unique(crmaxn$sumMaxN)

crmaxn$Rank <- rank(crmaxn$sumMaxN, ties.method = "max")

####################Crookhaven MaxN Plot - top 10 species
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
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1, size=8),
        axis.title.y = element_blank(),
        axis.title.x= element_blank(),
        axis.text.y= element_text(size=6),
        axis.title = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size=8, face="bold"))


####################Loading in Towra Point Data
tp <- MaxN %>%
  filter(Site=="SY-TP") %>%
  filter(Year=="2021")

#removing hyperabundant species
tpmaxn <- tp %>%
  group_by(ScientificName,Common)%>%
  summarise(sumMaxN = sum(MaxN)) %>%
  filter(Common !="Sandy Sprat") %>%
  filter(Common != "Port Jackson Glassfish")  %>%
  filter(Common != "Ogilby's Hardyhead")

####################Ranking Towra point Species
#number of ranks
unique(tpmaxn$sumMaxN)

tpmaxn$Rank <- rank(tpmaxn$sumMaxN, ties.method = "max")



####################Towra Point MaxN Plot - top 10 species
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
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1, size=8),
        axis.title.y = element_blank(),
        axis.title.x= element_text(size=9),
        axis.text.y= element_text(size=6),
        axis.title = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size=8, face="bold"))
tpplot

##############merging plots together and creating the final plot
library(grid)
library(gridExtra)
library(cowplot)
library(lemon)

finalplot <- grid.arrange(bgplot, crplot, tpplot, nrow=3, ncol=1, heights = c(6,6,6))
plot(finalplot)
finalplot <- arrangeGrob(finalplot,
                         left= textGrob("Total Abundance (Sum of MaxN)", rot=90, vjust=0.5))
##Outputting plot
tiff(filename = "test.tiff", height=5600, width=5200, units="px", res=800, compression = "lzw")

