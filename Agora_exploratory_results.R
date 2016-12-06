library(ggplot2); library(dplyr); library(marmap)
setwd("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Results/")
options(scipen=8)    #estableix el nombre per començar a fer servir la notació científica dels números


#### Obrir les dades ####
#ITEMS
lines <- readLines("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/Items.csv")
lines <- gsub('"','',lines,fixed=TRUE)
items <- read.csv(textConnection(lines), sep=";", dec=".")
li <- length(items$ItemID)
print(paste("There are", li, "items"))
# time Items
as.POSIXct(min(items$TimeStamp[!is.na(items$TimeStamp)]+10*24*60*62.5), origin="1970-01-01")
items <- subset(items,items$TimeStamp>min(items$TimeStamp[!is.na(items$TimeStamp)]+10*24*60*62.5)) #Filtrar primeres descàrregues fins 15/6/16
items$Time <- as.POSIXct(items$TimeStamp, origin="1970-01-01")
items$Day <- as.character(round(items$Time,"day"))
nitems <- length(levels(items$ItemID))
ultimdia <- max(items$Day[!is.na(items$Day)])
ultimmes <- as.Date(ultimdia)-30
itemsmes <- subset(items,items$Day>=ultimmes)
nitemsmes <- length(itemsmes$ItemID)
prevmes <- as.Date(ultimmes)-30
itemsprev <- subset(items,items$Day>=prevmes & items$Day<ultimmes)
nitemsprev <- length(itemsprev$ItemID)
difitems <- nitemsmes - nitemsprev

#USERS
lines <- readLines("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/Users.csv")
lines <- gsub('"','',lines,fixed=TRUE)
users <- read.csv(textConnection(lines), sep=";", dec=".")
# time users
users <- users %>% mutate(TimeStampn=ifelse(TimeStamp>5e+11,TimeStamp/1000,ifelse(is.na(TimeStamp),NA,TimeStamp)))
users$Time <- as.POSIXct(users$TimeStampn, origin="1970-01-01")
users$Day <- as.character(round(users$Time,"day"))
users$Month <- as.character(cut(users$Time,"month"))
nusers <- length(levels(users$UserID))
usersmes <- subset(users,users$Day>=ultimmes)
nusersmes <- length(usersmes$UserID)
usersprev <- subset(users,users$Day>=prevmes & users$Day<ultimmes)
nusersprev <- length(usersprev$UserID)
difusers <- nusersmes - nusersprev

#CATEGORIES
cats <- read.csv("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/Categories.csv", sep=";", dec=".")
colnames(cats) <- c("ES","Type","Category","Dummy","EN","Destiny","TimeStamp")
cats$Time <- as.POSIXct(cats$TimeStamp, origin="1970-01-01")  #NO ESTÀ BÉ
items$CategoryName <- cats$EN[match(items$Category,cats$Category)]
items$Type <- cats$Type[match(items$Category,cats$Category)]
items$CatTime <- cats$TimeStamp[match(items$Category,cats$Category)]
items$CategoryName <- factor(items$CategoryName,levels=cats$EN[order(cats$TimeStamp)])
REQ <- subset(cats,cats$Type=="REQ")

#TAGS
itemtags <- read.csv("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/ItemTags.csv", sep=";", dec=".")
total <- length(unique(itemtags$Tag))
naitemtag <- subset(itemtags,Language=="")
hashtags <- length(unique(naitemtag$Tag))
lanitemtag <- subset(itemtags,Language!="")
tags <- length(unique(lanitemtag$Tag))

print(paste("There are a total of", total, "tags and hashtags"))
print(paste("There are", tags, "tags"))
print(paste("There are", hashtags, "hashtags"))

#DOWNLOADS
lines <- readLines("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/DescargasApple.csv")
lines <- gsub('"','',lines,fixed=TRUE)
downs <- read.csv(textConnection(lines), sep=";", dec=".")
colnames(downs) <- c("Date","Downloads")
downs$Day <- as.Date(downs$Date,"%d/%m/%Y")
downs <-na.omit(downs)
downs$Day <- as.POSIXct(downs$Day[!is.na(downs$Day)])
downs$downloads <- "downloads"
ndowns <- sum(downs$Downloads)
downsmes <- subset(downs,downs$Day>=as.POSIXct(ultimmes))
ndownsmes <- sum(downsmes$Downloads)
downsprev <- subset(downs,downs$Day>=as.POSIXct(prevmes) & downs$Day<as.POSIXct(ultimmes))
ndownsprev <- sum(downsprev$Downloads)
difdowns <- ndownsmes - ndownsprev

###### --------------- COMENÇA PDF -----------------------------##########
pdf(file="Agora_exploratory_results.pdf",onefile=TRUE)

#Text per situar
plot(0:16, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(9, 15, cex=2,font=2,"AGORA exploratory results")
text(9, 14, paste("Last Date= ",ultimdia))
text(9,11,"Users       Downloads       Items        Tags       Hashtags", font=2)
text(9,10,paste(nusers,"      ",ndowns,"      ",nitems,"      ",tags,"     ",hashtags))
text(9,8,paste0("New users in the last 30 days = ", nusersmes, " (", round(nusersmes*100/nusers,digits=1), " % total)"))
text(9,7,paste0(if (difusers>0) "+", difusers, " users than previous 30 days"),col=ifelse(difusers>0,"blue","red"))
text(9,5,paste0("New items in the last 30 days = ", nitemsmes, " (", round(nitemsmes*100/nitems,digits=1), " % total)"))
text(9,4,paste0(if (difitems>0) "+", difitems, " items than previous 30 days"),col=ifelse(difitems>0,"blue","red"))
text(9,2,paste0("New downloads in the last 30 days = ", ndownsmes, " (", round(ndownsmes*100/ndowns,digits=1), " % total)"))
text(9,1,paste0(if (difdowns>0) "+", difdowns, " downloads than previous 30 days"),col=ifelse(difdowns>0,"blue","red"))


#Items per user and day
days_i <- as.POSIXct(items$Day[!is.na(items$Day)])
days_u <- as.POSIXct(users$Day[!is.na(users$Day)])
daysi <- data.frame(items=days_i,
                    var=rep("items",length(days_i)))
daysu <- data.frame(users=days_u,
                    var=rep("users",length(days_u)))
daysd <- data.frame(users=days_d,
                    var=rep("downloads",length(days_d)))
colnames(daysi) <- c("Time","var")
colnames(daysu) <- c("Time","var")
colnames(daysd) <- c("Time","var")
cdaysiu <- rbind(daysi,daysu)
cdaysiu$var <- factor(cdaysiu$var, levels=c("users","items"))

giu <- ggplot(data=cdaysiu, aes(Time))+
  geom_bar(aes(fill=var),position="dodge")+
  scale_fill_manual(values=c("navy","red"))+
  geom_line(data=downs, aes(Day,Downloads, col=downloads),lwd=1.5,lty=1)+ 
  scale_color_manual(name="downloads",values=c("darkgreen"))+
  labs(y="Counts\n", x="\nTime", title= "New items, users and downloads per day")+ 
  theme(axis.line=element_line(size=.5,colour="black"),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 12), 
        axis.text.y=element_text(colour="black", size = 12),
        title=element_text(size=15),
        legend.text=element_text(size=12),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title=element_blank(),legend.box = "horizontal")
giu


#% Users with and without images
nusersitems <- length(levels(items$OwnerID))
propusersitems <- nusersitems*100/nusers
nitemsuser <- nitems/nusersitems
nusersnoitems <- nusers-nusersitems

plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(5, 8, paste("There are", nusers, "users in Agora"))
text(5, 6, paste(nusersitems, "users have uploaded pics"))
text(5,4,paste("That represents a", round(propusersitems,0), "% of total users"))
text(5,2,paste("There are a mean of", round(nitemsuser,0), "pics per user that upload"))

y <- matrix(c(rep(1,round(propusersitems,0)),rep(10,100-round(propusersitems,0))), 10, 10)
image(y,xaxt="n",yaxt="n",xlim=c(-0.05,1.05),ylim=c(-0.05,1.05),col=c("navy","red"),main="Users")
abline(h=seq(-0.05,1.0,0.111),col="white",lty=1)
abline(v=seq(-0.05,1.05,0.107),col="white",lty=1)
mtext("No items",side=2,at=0.85,line=0.3,font=2)
mtext("With items",side=2,at=0.3,line=0.3,font=2)
text(x=0.23, y=0.23, paste(round(propusersitems,0),"%"),col="white",font=2)
text(x=0.23, y=0.89, paste(round(100-propusersitems,0),"%"),col="white",font=2)

# items per category per time
catitems <- as.data.frame(table(items$CategoryName,items$Day))
catitems$Type <- cats$Type[match(catitems$Var1,cats$EN)]
requestsItems <- subset(catitems,catitems$Type=="REQ")
catitems$Var2 <- strptime(catitems$Var2,format="%Y-%m-%d")
requestsItems$Var2 <- as.Date(requestsItems$Var2)
requestsItems$Requests <- requestsItems$Var1
downs$Day <- as.Date(downs$Day)
gic <- ggplot(data=requestsItems,aes(x=Var2,y=Freq))+
  geom_area(aes(fill=Requests))+
  ylim(c(0,1500))+
  scale_fill_manual(values=c(rainbow(9),rainbow(length(unique(requestsItems$Requests))-9),rainbow(length(unique(requestsItems$Requests))-18)))+
  labs(y="Counts\n", x="\nTime", title= "New items per request and downloads per day")+
  theme(axis.line=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        title=element_text(size=15),
        legend.text=element_text(size=8),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title=NULL)

gic2 <- ggplot(data=downs, aes(Day,Downloads))+
  geom_line(lwd=1.3,lty=1,show.legend = FALSE, col="darkgreen",alpha=0.6)+ 
  labs(y="Downloads", x="\nTime", title= "",bg="darkgreen")+
  ylim(c(-500,3300))+
  theme(axis.line=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        title=element_text(size=15),
        legend.text=element_text(size=8),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))

ggplot_dual_axis(gic, gic2, "y")

# Items per request per julian day from start
reqs <- as.vector(factor(levels(REQ$EN),levels=(levels(REQ$EN)[order(REQ$TimeStamp)])))
reqs <- reqs[!is.na(reqs)]
requestsItems$Time <- format(requestsItems$Var2, "%j")
requestsItems$Time <- as.numeric(requestsItems$Time)

for(i in reqs){
  subreqs <- subset(requestsItems,requestsItems$Requests==i)
  subreqs <- subreqs[subreqs$Freq>0,]
  subreqs$Time <- subreqs$Time - min(subreqs$Time)
  subreqs <- subreqs %>% mutate(cumsumtime=cumsum(Freq))
  subreqs$freq_pc <- (subreqs$cumsumtime*100)/max(subreqs$cumsumtime)
  if (which(reqs==i)==1) reqstemps <- subreqs
  else reqstemps <- rbind(reqstemps,subreqs)
}
cols=c(rainbow(9),rainbow(length(reqs)-9),rainbow(length(reqs)-18))

# Cumsum Items per request per julian day from start
grdc <- ggplot(data=reqstemps,aes(x=Time,y=cumsumtime,col=Requests,group=Requests,linetype=Requests))+
  geom_line(size=1.3)+
  scale_colour_manual(values=cols)+
  scale_linetype_manual(values=c(rep("solid",9),rep("dotdash",length(reqs)-9),rep("dotted",length(reqs)-9)))+ #also longdash,twodash,dashed
  labs(y="Cumulated items", x="Days from start", title= "Cumulated items per request per day")+
  theme(axis.line=element_line(size=.5,colour="black"),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        title=element_text(size=15),
        legend.text=element_text(size=8),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title=NULL)+
  scale_x_continuous(breaks=c(seq(0,120,5)))+
  geom_vline(xintercept = 15,colour="black",lty=3)
grdc

# %Cumsum Items per request per julian day from start
cols=c(rainbow(9),rainbow(length(reqs)-9))
grdpc <- ggplot(data=reqstemps,aes(x=Time,y=freq_pc,col=Requests,group=Requests,linetype=Requests))+
  geom_line(size=1.3)+
  scale_colour_manual(values=cols)+
  scale_linetype_manual(values=c(rep("solid",9),rep("dotdash",length(reqs)-9)))+
  labs(y="% Cumulated items", x="Days from start", title= "% Cumulated items per request per day")+
  theme(axis.line=element_line(size=.5,colour="black"),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        title=element_text(size=15),
        legend.text=element_text(size=8),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title=NULL)+
  scale_x_continuous(breaks=c(seq(0,120,5)))+
  geom_hline(yintercept = 75,colour="black",lty=3)+
  geom_vline(xintercept = 15,colour="black",lty=3)
grdpc

#Items in request
pf <- plot(items$Type, xlab="Type of item", ylab="Items count",font=2, main="Item type", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$Type))), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("Category","Request"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=c(seq(0,35000,5000)),at=c(seq(0,35000,5000)),las=2,font=2, lwd=0, line=-1)
niCAT <- length(items$Type[items$Type=="CAT"])
niREQ <- length(items$Type[items$Type=="REQ"])
text(pf, c(25000,25000), paste(c(round(niCAT*100/(niCAT+niREQ),0),round(niREQ*100/(niCAT+niREQ),0)),"%"),font=2)

#Users with items per type
userreqs <- as.data.frame(table(items$OwnerID,items$Type))
usersCAT <- userreqs[userreqs$Var2=="CAT" & userreqs$Freq>0,]
nCAT <- length(usersCAT$Freq)
usersREQ <- userreqs[userreqs$Var2=="REQ" & userreqs$Freq>0,]
nREQ <- length(usersREQ$Freq)
pf <- barplot(c(nCAT,nREQ), xlab="Type of item", ylab="Users count",font=2, main="Users that participate", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$Type))), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("Category","Request"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=c(seq(0,35000,1000)),at=c(seq(0,35000,1000)),las=2,font=2, lwd=0, line=-1)
text(pf, c(5000,5000), paste(c(round(nCAT*100/(nCAT+nREQ),0),round(nREQ*100/(nCAT+nREQ),0)),"%"),font=2)

#Items type per user type ##################################
userreqs$level <- users$LevelName[match(userreqs$Var1,users$UserID)]
userreqs <- na.omit(userreqs)
userreqs$level <- factor(userreqs$level,levels=c("Junior","Advanced","Pro","Master"))
ggplot(data=userreqs,aes(x=Var2,y=Freq,col=level))+
  geom_boxplot(size=1.5)+
  scale_fill_manual(values=rainbow(length(unique(userreqs$level))))+
  labs(y="Users count\n", x="\nType of item")+
  theme(axis.line=element_line(size=.5,colour="black"),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank(),
        panel.background=element_blank())+
  theme(axis.text.x=element_text(colour="black", size = 12,face="bold"), 
        axis.text.y=element_text(colour="black", size = 12,face="bold"),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        strip.background=element_rect(colour="black",fill="white",size=2),
        strip.text=element_text(size=14,vjust=1))+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title=element_blank())


#Items per user
propREQ <- round(niREQ/nREQ,0)
propCAT <- round(niCAT/nCAT,0)
pf <- barplot(c(propCAT,propREQ), xlab="Type of item", ylab="Items per user",font=2, main="Items per user by type", las=2,xaxt="n",yaxt="n", col=c("red","navy"), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("Category","Request"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=seq(0,10,1),at=seq(0,10,1),las=2,font=2, lwd=0, line=-1)
text(pf, c(3,3), paste(c(round(propCAT*100/(propCAT+propREQ),0),round(propREQ*100/(propCAT+propREQ),0)),"%"),font=2)


  # Stars histogram
par(lwd = 2, mar=c(5,5,2,2))
items$Stars_class <- cut(items$Stars,c(seq(0,20,1),seq(30,90,30),seq(100,110,1),seq(120,200,20),seq(220,300,20),122000),right=FALSE)
pf <- plot(items$Stars_class, xlab="Stars number", ylab="Items count",font=2, main="Item stars", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$Stars_class))), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c(seq(0,20,1),seq(30,90,30),seq(100,110,1),seq(120,200,20),seq(220,280,20),">300"),las=2,font=1, at=pf, lwd=0)
axis(2,labels=c(seq(0,20000,1000)),at=c(seq(0,20000,1000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)
abline(v=c(24.3,28.8, 40.9), lty=2, col="gray")

# Stars won by


# Superstars
par(lwd = 2, mar=c(5,5,2,2))
items$SuperStars_class2 <- cut(items$SuperStars,c(seq(1,20,1),1216),right=FALSE)
pf2 <- plot(items$SuperStars_class2, xlab="SuperStars number", ylab="Items count",font=2, main="Item SuperStars", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$SuperStars_class2))), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c(seq(1,19,1),">20"),las=2,font=2, at=pf2, lwd=0)
axis(2,labels=c(seq(0,30000,2000)),at=c(seq(0,30000,2000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)

# Categories
par(lwd = 2, mar=c(7,5,2,2))
ordercats <- cats$Type[order(cats$Type,decreasing=TRUE)]
pf2 <- plot(items$CategoryName, xlab="", ylab="Items count",border=ordercats,font=2, main="Item Category", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$Category))), lwd=2, cex.lab=1.5)
axis(1,labels=levels(items$CategoryName),las=2,cex.axis=0.6,font=2, at=pf2, lwd=0, line=-1)
axis(2,labels=c(seq(0,30000,1000)),at=c(seq(0,30000,1000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)
text(x=c(8, 30), y= c(5000,5000), labels=c("Requests","Categories"), font=2)
nreqs <- 1+sum(as.data.frame(table(items$CategoryName[items$Type=="REQ"]))$Freq>0)
abline(v=nreqs*1.2-1.1, lty=2, col="black")

# Item price
items$Price <- gsub(",",".", items$Price)
items$Price <- factor(items$Price, levels=c("", "0.99", "1.99", "4.99", "9.99"))
par(lwd = 2, mar=c(5,5,2,2))
pf2 <- plot(items$Price, xlab="Item price", ylab="Items count",font=2, main="Item Price", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(items$Price))), lwd=2, cex.lab=1.5)
axis(1,labels=c("NA", "0.99", "1.99", "4.99", "9.99"),las=2,font=2, at=pf2, lwd=0)
axis(2,labels=c(seq(0,300000,5000)),at=c(seq(0,300000,5000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)

# Levels by user type
#levels by level names -- JUNIOR
talls <- c(200,500,1000,5000)
users$Level_class <- cut(users$Level,c(seq(0,talls[1],10),seq(250,talls[2],50),seq(600,talls[3],100),seq(2000,talls[4],1000),max(users$Level)),right=FALSE)
par(lwd = 2)
pf <- plot(users$Level_class, xlab="Level", ylab="Users count",font=2, main="Junior user level", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
labs <- sub(",.*", "",levels(users$Level_class)[1:length(levels(users$Level_class))-1])
labs <- as.numeric(sub("\\[", "",labs))
axis(1,labels=c(labs,">5000"),las=2,font=2, at=pf, lwd=0)
axis(2,labels=seq(0,10000,1000),at=seq(0,10000,1000),las=2,font=2, lwd=0,line=-1)
breaks <- which(labs==talls[1]|labs==talls[2]|labs==talls[3]|labs==talls[4])
breaks[length(breaks)]=breaks[length(breaks)]+1
abline(v=breaks*1.2-1.1, lty=2, col="gray")
par(lwd = 1)
#levels by level names -- ADVANCED
talls <- c(5000,10000,20000,100000)
users$Level_class <- cut(users$Level,c(seq(talls[1],talls[2],1000),seq(talls[3],talls[4],10000),max(users$Level)),right=FALSE)
par(lwd = 2)
pf <- plot(users$Level_class, xlab="Level", ylab="Users count",font=2, main="Advanced user level", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
labs <- sub(",.*", "",levels(users$Level_class)[1:length(levels(users$Level_class))-1])
labs <- as.numeric(sub("\\[", "",labs))
axis(1,labels=c(labs,">100000"),las=2,font=2, at=pf, lwd=0,line=-1)
axis(2,labels=seq(0,10000,10),at=seq(0,10000,10),las=2,font=2, lwd=0,line=-1)
breaks <- which(labs==talls[2]|labs==90000|labs==talls[4])
breaks[length(breaks)]=breaks[length(breaks)]+1
abline(v=breaks*1.2-1.1, lty=2, col="gray")
par(lwd = 1)
#levels by level names -- PRO
talls <- c(100000,250000)
users$Level_class <- cut(users$Level,c(seq(talls[1],talls[2],5000),max(users$Level)),right=FALSE)
par(lwd = 2)
pf <- plot(users$Level_class, xlab="Level", ylab="Users count",font=2, main="Pro user level", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
labs <- sub(",.*", "",levels(users$Level_class)[1:length(levels(users$Level_class))-1])
labs <- as.numeric(sub("\\[", "",labs))
axis(1,labels=c(labs,">250000"),las=2,font=2, at=pf, lwd=0,line=-1)
axis(2,labels=seq(0,10000,10),at=seq(0,10000,10),las=2,font=2, lwd=0,line=-1)
par(lwd = 1)
#levels by level names -- MASTER
talls <- c(250000,300000,400000,1000000)
users$Level_class <- cut(users$Level,c(seq(talls[1],talls[2],5000),seq(talls[3],talls[4],100000),max(users$Level)),right=FALSE)
par(lwd = 2)
pf <- plot(users$Level_class, xlab="Level", ylab="Users count",font=2, main="Master user level", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
labs <- sub(",.*", "",levels(users$Level_class)[1:length(levels(users$Level_class))-1])
labs <- as.numeric(sub("\\[", "",labs))
axis(1,labels=c(labs,">100000"),las=2,font=2, at=pf, lwd=0,line=-1)
axis(2,labels=seq(0,10000,1),at=seq(0,10000,1),las=2,font=2, lwd=0,line=-1)
breaks <- which(labs==talls[2]|labs==talls[4])
breaks[length(breaks)]=breaks[length(breaks)]+1
abline(v=breaks*1.2-1.1, lty=2, col="gray")
par(lwd = 1)


# Language
par(lwd = 2)
users$Language <- factor(users$Language,levels=c("ES","EN",""))
pf <- plot(users$Language, xlab="Language", ylab="Users count",font=2, main="User language", las=2,xaxt="n",yaxt="n", col=rainbow(3), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("Spanish","English","No specified"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=c(0,2000,4000,6000,8000,10000),at=c(0,2000,4000,6000,8000,10000),las=2,font=2, lwd=0)
par(lwd = 1)

# Stars by user
users$Stars_class <- cut(users$Stars,c(seq(0,30,1),seq(35,50,5),seq(60,100,10),seq(200,1000,100),seq(2000,10000,1000),71000), right=FALSE)
pf <- plot(users$Stars_class, xlab="Stars number", ylab="Users count",font=2, main="User stars", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c(seq(0,30,1),seq(35,50,5),seq(60,100,10),seq(200,1000,100),seq(2000,9000,1000),">10000"),las=2,font=1.5, at=pf, lwd=0)
axis(2,labels=c(seq(0,2000,200)),at=c(seq(0,2000,200)),las=2,font=2, lwd=0,line=-1)
abline(v=c(37.3, 42.3, 46.8, 57.5), lty=2, col="gray")
par(lwd = 1)

#Stars by level
levelslog <- users[users$Level<1000000,]
levelslog$group <- rep(1,length(levelslog$UserID))
levelslog$group[levelslog$Level>99000] <- 2
levelslog$group[levelslog$Level>249000] <- 3
plot(levelslog$Stars,levelslog$Level,col=levelslog$group, xlab="User Stars",ylab="User Levels", main="Level by Stars",font=2, lwd=2,cex.lab=1.4)

falselevels <- levelslog[levelslog$group>1,]
falselevelsusers <- 

# Facebook
nf <- length(levels(users$FID))-1
print(paste("% users wiwh Facebook=", round((length(levels(users$FID))-1)*100/length(users$FID),digits=0),"%"))
fpc <- round((length(levels(users$FID))-1)*100/length(users$FID))
par(lwd = 2)
pf <- barplot(c(length(users$FID)-nf,nf), xlab="Users", ylab="Users count",font=2, main="User with Facebook", las=2,xaxt="n",yaxt="n", col=c("red","navy"), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("No","Facebook"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=c(0,2000,4000,6000,8000,10000),at=c(0,2000,4000,6000,8000,10000),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)
text(pf, c(5000,5000), paste(c(100-fpc,fpc),"%"),font=2)

# Image at profile
np <- length(levels(users$ProfileImage))-1
print(paste("% users wiwh Image in Profile=", round(np*100/length(users$ProfileImage),digits=0),"%"))
ppc <- round(np*100/length(users$ProfileImage),digits=0)
par(lwd = 2)
pf <- barplot(c(length(users$ProfileImage)-np,np), xlab="Users", ylab="Users count",font=2, main="User with Image in Profile", las=2,xaxt="n",yaxt="n", col=c("red","blue"), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c("No","Yes"),las=1,font=2, at=pf, lwd=0)
axis(2,labels=c(0,2000,4000,6000,8000,10000),at=c(0,2000,4000,6000,8000,10000),las=2,font=2, lwd=0,line=-1)
par(lwd = 1)
text(pf, c(3500,3500), paste(c(100-ppc,ppc),"%"),font=2)

# Followers
par(lwd = 2)
users$Followers_class <- cut(users$Followers,c(seq(0,10,1),seq(12,20,2),seq(30,100,10),seq(200,1000,100),1500),right=FALSE)
pf2 <- plot(users$Followers_class, xlab="Followers number", ylab="Users count",font=2, main="User followers", las=2,xaxt="n",yaxt="n", col=rainbow(length(levels(users$Followers_class))), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c(substr(levels(users$Followers_class)[-length(levels(users$Followers_class))],2,unlist(gregexpr(pattern=",",levels(users$Followers_class[-length(levels(users$Followers_class))])))-1),">1000"),las=2,font=2, at=pf2, lwd=0)
axis(2,labels=c(seq(0,10000,1000)),at=c(seq(0,10000,1000)),las=2,font=2, lwd=0,line=-1)
abline(v=c(13.3,19.4,29,37.3), lty=2, col="gray")
par(lwd = 1)

# Follows
users$Follws_class2 <- cut(users$Follows,c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,50,75,100,200,2500),right=FALSE)
par(lwd = 2)
pf <- plot(users$Follws_class2, xlab="Follows", ylab="Users count",font=2, main="Follows by user", las=2,xaxt="n",yaxt="n", col=rainbow(19), lwd=2,border="black", cex.lab=1.5)
axis(1,labels=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,50,75,100,">200"),las=2,font=2, at=pf, lwd=0)
axis(2,labels=seq(0,10000,1000),at=seq(0,10000,1000),las=2,font=2, lwd=0,line=-1)
abline(v=c(13.3,16.9), lty=2, col="gray")
par(lwd = 1)

# Tags and cloudtag
ENlanitemtag <- lanitemtag[lanitemtag$Language=="EN",]
tabtags <- table(ENlanitemtag$Tag)
ordtags <- tabtags[order(tabtags,decreasing=TRUE)]
ordtags50 <- ordtags[1:50]
par(lwd = 2, mar=c(6,5,2,2))
ph <- plot(ordtags50, xlab="", ylab="Item count", font=1, main="50 most abundant Item Tags", las=2,xaxt="n",yaxt="n", col=rainbow(50), lwd=2, cex.lab=1.5)
axis(1,labels=names(ordtags50),las=2,font=1, at=0.6:50, lwd=0)
axis(2,labels=c(seq(0,30000,2000)),at=c(seq(0,30000,2000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)
library(slam); library(wordcloud);library(tm)
word_freq <- sort(table(ENlanitemtag$Tag), decreasing = TRUE)[1:100]
wordcloud(words=names(word_freq), freq=word_freq,
          min.freq=1, random.order=F)

# Number of tags per item
head(lanitemtag)
nitemstags <- length(unique(lanitemtag$ItemID))
ntags <- length(lanitemtag$ItemID)
print(paste("Tags per item=",round(ntags/nitemstags,0)))

tabtag <- as.data.frame(table(lanitemtag$ItemID))
promtag <- mean(tabtag$Freq)
mintags <- min(tabtag$Freq)
maxtags <- max(tabtag$Freq)
print(paste("Tags per item=",round(promtag,0)))
print(paste("Range from",mintags,"to",maxtags,"tags per item"))
nitemstagsN <- length(tabtag[tabtag$Freq!=0,]$Freq)

colnames(items)
items$CatTime <- cats$TimeStamp[match(items$Category,cats$Category)]
lanitemtag$Category <- items$CategoryName[match(items$Category,cats$Category)]

# Hashtags and cloudhashtags
tabtags <- table(naitemtag$Tag)
ordtags <- tabtags[order(tabtags,decreasing=TRUE)]
ordtags50 <- ordtags[1:50]
par(lwd = 2, mar=c(8,5,2,2))
ph <- plot(ordtags50, xlab="", ylab="Item count", font=1, main="50 most abundant Item Hashtags", las=2,xaxt="n",yaxt="n", col=rainbow(50), lwd=2, cex.lab=1.5)
axis(1,labels=names(ordtags50),las=2,cex.axis=0.8, at=0.6:50, lwd=0)
axis(2,labels=c(seq(0,30000,2000)),at=c(seq(0,30000,2000)),las=2,font=2, lwd=0, line=-1)
par(lwd = 1)
word_freq <- sort(table(naitemtag$Tag), decreasing = TRUE)[1:100]
wordcloud(words=names(word_freq), freq=word_freq,
          min.freq=1, random.order=F)

# Number of hashtags per item
head(naitemtag)
nitemshash <- length(unique(naitemtag$ItemID))
nhash <- length(naitemtag$ItemID)
print(paste("Hashtags per item=",round(nhash/nitemshash,0)))

tabhash <- as.data.frame(table(naitemtag$ItemID))
promhash <- mean(tabhash$Freq)
minhashs <- min(tabhash$Freq)
maxhashs <- max(tabhash$Freq)
print(paste("Hashtags per item=",round(promhash,0)))
print(paste("Range from",minhashs,"to",maxhashs,"hashtags per item"))

#Text sobre els tags i hashtags
centrar <- 7
plot(0:14, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(centrar, 13, cex=2,font=1.5, paste("Summary of tags and hashtags info"))
text(centrar, 11, paste("Total tags=",tags))
text(centrar, 10, paste("Tags per item=",round(promtag,0)))
text(centrar,9,paste("Range from",mintags,"to",maxtags,"tags per item"))
text(centrar,8,paste("Total items with tags",nitemstagsN))
text(centrar,7,paste("Total items without tags",nitemstags-nitemstagsN))
text(centrar,5,paste0("Total hashtags =",hashtags))
text(centrar,4,paste0("Hashtags per item=",round(promhash,0)))
text(centrar,3,paste("Range from",minhashs,"to",maxhashs,"hashtags per item"))
text(centrar,2,paste("Total items with hashtags",nitemshash))


# User map world and Spain
#world
datm <- getNOAA.bathy(-180,180,-90,90,res=30, keep=TRUE)
world <- autoplot(datm, geom="r") + scale_fill_etopo(guide="none")+
  scale_x_continuous(expand=c(0,0),breaks=c(-150,-100,-50,0,50,100,150), labels=c("150ºW","100","50","0","50","100","150ºE")) +
  scale_y_continuous(expand=c(0,0),breaks=c(-80,-40,0,40,80), labels=c("80ºS","40","0","40","80ºN")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(axis.text.x = element_text(size = 12,colour="black"),axis.text.y = element_text(size = 12,colour="black"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(panel.border = element_rect(linetype = 1, colour="black", fill=NA,size=2))
points <- data.frame(Lat=users$Latitude,
                     Long=users$Longitude,
                     Level=users$LevelName)
points$Level <- factor(points$Level, levels=c("Junior","Advanced","Master","Pro"))
world + 
  geom_point(data=points,aes(x=Long, y=Lat,color=factor(Level, levels=c("Junior","Advanced","Master","Pro"))),size=1, shape=21,stroke = 2)+
  scale_colour_manual(values=c("red","yellow","purple","black"), 
                      guide = guide_legend(title = "User level"),drop=FALSE)+
  geom_point(data=points[points$Level=="Advanced",],aes(x=Long, y=Lat),color="yellow",size=2, shape=21,stroke = 2)+
  geom_point(data=points[points$Level=="Master",],aes(x=Long, y=Lat),color="purple",size=2, shape=21,stroke = 2)+
  geom_point(data=points[points$Level=="Pro",],aes(x=Long, y=Lat),color="black",size=2, shape=21,stroke = 2)+
  theme(legend.key=element_rect(fill="skyblue"))+
  theme(legend.title= element_text(face= "bold"))

#spain
datsp <- getNOAA.bathy(-10.25,5.21,35.08,44.06,res=1, keep=TRUE)
points_sp <- points[!is.na(points$Lat),]
points_sp <- points_sp[points_sp$Lat<44.06,]
points_sp <- points_sp[points_sp$Lat>35.08,]
points_sp <- points_sp[points_sp$Long<5.21,]
points_sp <- points_sp[points_sp$Long>-10.25,]

sp <- autoplot(datsp, geom="r") + scale_fill_etopo(guide="none")+
  scale_x_continuous(expand=c(0,0),breaks=c(-150,-100,-50,0,50,100,150), labels=c("150ºW","100","50","0","50","100","150ºE")) +
  scale_y_continuous(expand=c(0,0),breaks=c(-80,-40,0,40,80), labels=c("80ºS","40","0","40","80ºN")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(axis.text.x = element_text(size = 12,colour="black"),axis.text.y = element_text(size = 12,colour="black"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(panel.border = element_rect(linetype = 1, colour="black", fill=NA,size=2))
sp + 
  geom_point(data=points_sp,aes(x=Long, y=Lat,color=factor(Level, levels=c("Junior","Advanced","Master","Pro"))),size=2, shape=21,stroke = 2)+
  scale_colour_manual(values=c("red","yellow","purple","black"), 
                      guide = guide_legend(title = "User level"),drop=FALSE)+
  geom_point(data=points_sp[points_sp$Level=="Advanced",],aes(x=Long, y=Lat),color="yellow",size=2, shape=21,stroke = 2)+
  geom_point(data=points_sp[points_sp$Level=="Master",],aes(x=Long, y=Lat),color="purple",size=2, shape=21,stroke = 2)+
  geom_point(data=points_sp[points_sp$Level=="Pro",],aes(x=Long, y=Lat),color="black",size=2, shape=21,stroke = 2)+
  theme(legend.key=element_rect(fill="skyblue"))+
  theme(legend.title= element_text(face= "bold"))


# Item map world and Spain
#world
pointsi <- data.frame(Lat=items$Latitude,
                     Long=items$Longitude,
                     n=items$Stars)
pointsi$Stars <- cut(pointsi$n,c(0,1,5,10,100,200),right=FALSE)
world + 
  geom_point(data=pointsi,aes(x=Long, y=Lat,color=Stars),size=1, shape=21,stroke = 2)+
  scale_colour_manual(values=rainbow(5), 
                     guide = guide_legend(title = "Item stars", levels=c(0,1,5,10,">100")))+
  theme(legend.key=element_rect(fill="skyblue"))+
  theme(legend.title= element_text(face= "bold"))

#spain
datsp <- getNOAA.bathy(-10.25,5.21,35.08,44.06,res=1, keep=TRUE)
points_sp <- pointsi[!is.na(pointsi$Lat),]
points_sp <- points_sp[points_sp$Lat<44.06,]
points_sp <- points_sp[points_sp$Lat>35.08,]
points_sp <- points_sp[points_sp$Long<5.21,]
points_sp <- points_sp[points_sp$Long>-10.25,]

sp <- autoplot(datsp, geom="r") + scale_fill_etopo(guide="none")+
  scale_x_continuous(expand=c(0,0),breaks=c(-150,-100,-50,0,50,100,150), labels=c("150ºW","100","50","0","50","100","150ºE")) +
  scale_y_continuous(expand=c(0,0),breaks=c(-80,-40,0,40,80), labels=c("80ºS","40","0","40","80ºN")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(axis.text.x = element_text(size = 12,colour="black"),axis.text.y = element_text(size = 12,colour="black"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(panel.border = element_rect(linetype = 1, colour="black", fill=NA,size=2))
sp + 
  geom_point(data=points_sp,aes(x=Long, y=Lat,color=Stars),size=2, shape=21,stroke = 2)+
  scale_colour_manual(values=rainbow(5), 
                      guide = guide_legend(title = "Item stars", levels=c(0,1,5,10,">100")))+
  theme(legend.key=element_rect(fill="skyblue"))+
  theme(legend.title= element_text(face= "bold"))


dev.off()
