library(ggplot2); library(dplyr); library(rpart);library(rattle);library(rpart.plot);library(RColorBrewer);library(xlsx); library(jsonlite)
setwd("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Results/User_classification/")

#Users List by categories, altogether
View(listUsers)

scorelist <- read.delim("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Data/Lista_usuarios_Agora_prioritats_Octavi.txt", dec=".")
View(scorelist)


listUsers$score <- scorelist$Score[match(listUsers$UserID,scorelist$UserID)]
train <- listUsers[listUsers$score>0,]
test <- listUsers[listUsers$score==0,]

#Decission trees
#all users in list
fit <- rpart(score ~ Stars + Items + followers, data=train, method = "anova")
plot(fit)
text(fit)
fancyRpartPlot(fit)

#only users >10 items
listUsers10
listUsers10$score <- scorelist$Score[match(listUsers10$UserID,scorelist$UserID)]
train10 <- listUsers10[listUsers10$score>0,]
test10 <- listUsers10[listUsers10$score==0,]
fit10 <- rpart(score ~ Stars + Items + followers, data=train10, method = "anova")
fancyRpartPlot(fit10)

Prediction <- predict(fit10,listUsers10,type="vector")
hist(Prediction)

listUsers10$pred <- Prediction

colnames(catusersord)
users$pred <- 0
users$pred <- listUsers10$pred[match(users$UserID,listUsers10$UserID)]
listUsers10$score <- scorelist$Score[match(listUsers10$UserID,scorelist$UserID)]

#Save each category list
setwd("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Results/UserLists/")
for (categoria in catsname){
  catuser <- subset(listUsers10,listUsers10$Category==categoria)
  catusersord <- catuser[order(catuser$pred,decreasing = TRUE),]
  catusersord <- catusersord[,c(1:6,10:11)]
  assign(paste0("list_users_score_",categoria),catusersord)
  write.xlsx(assign(paste0("list_users_score_",categoria),catusersord), paste0("list_users_score_",categoria,".xlsx"),row.names=FALSE)
}


################   V2   #############################
#Users lists by categories
library(xlsx)
CAT <- subset(cats,cats$Type=="CAT")
catsname <- cats$EN[cats$Type=="CAT"]
setwd("D:/Angel/Documents/Biblio_gral/Estadistica/Scripts_R/AGORA/Results/UserLists/")


userList <- data.frame(User=users$UserID,
                       Stars=users$Stars,
                       Followers=users$Followers)
catuseritems <- as.data.frame(table(items$OwnerID))
colnames(catuseritems) <- c("UserID","Items")
userList$Items <- catuseritems$Items[match(userList$User,catuseritems$UserID)]
userList$Items[is.na(userList$Items)] <- 0
userList$score <- scorelist$Score[match(userList$User,scorelist$UserID)]
train <- userList
fit <- rpart(score ~ Stars + Items + Followers, data=train, method = "anova")
fancyRpartPlot(fit)

minitems <- 5
for (categoria in catsname){
  catuser <- subset(items,items$CategoryName==categoria)
  catuserstars <- aggregate(catuser$Stars, list(catuser$OwnerID), sum)
  colnames(catuserstars) <- c("UserID","Stars")
  catuseritems <- as.data.frame(table(catuser$OwnerID))
  colnames(catuseritems) <- c("UserID","Items")
  catusersord <- merge(catuserstars,catuseritems,by="UserID")
  catusermin <- catusersord[catusersord$Items>minitems,]
  catusermin$Category <- rep(categoria,length(catusermin$UserID))
  catusermin$name <- users$Name[match(catusermin$UserID,users$UserID)]
  catusermin$Followers <- users$Followers[match(catusermin$UserID,users$UserID)]
  catusermin <- catusermin[,c(1,5,2,3,6,4)]
  catusermin$score <- scorelist$Score[match(catusermin$UserID,scorelist$UserID)]
  test <- catusermin
  catusermin$pred <- predict(fit,test,type="vector")
  catusermin <- catusermin[order(catusermin$pred,decreasing = TRUE),]
  nmax <- ifelse(length(catusermin$UserID)>100,100,length(catusermin$UserID))
  catusermin <- catusermin[0:nmax,]
  
  colnames(catusermin) <- c("userid","name","stars","items","followers","category","qualitative","score")
  
  assign(paste0("list_users_",categoria),catusermin)
  write.xlsx(assign(paste0("list_users_",categoria),catusermin), paste0("list_users_",categoria,".xlsx"),row.names=FALSE)

  exportJson <- toJSON(catusermin)
  write(exportJson, paste0("list_users_",categoria,".json"))
  
  n <- which(catsname==categoria)
  if (n==1){
    juntcats <- catusermin}
  else
  {juntcats <- rbind(juntcats,catusermin)}
}
write.xlsx(juntcats, "list_ALL_relevant_users.xlsx",row.names=FALSE)
exportJson <- toJSON(juntcats)
write(exportJson, "list_ALL_relevant_users.json")


#Si no funciona l'exportació a json (http://stackoverflow.com/questions/25550711/convert-data-frame-to-json)
library(rjson)
x <- toJSON(unname(split(res1, 1:nrow(res1))))
cat(x)
# [{"id":1,"value":"server1"},{"id":2,"value":"server2"},
# {"id":3,"value":"server3"},{"id":4,"value":"server4"},
# {"id":5,"value":"server5"}]
#By using split() we are essentially breaking up the large data.frame into a separate data.frame for each row. And by removing the names from the resulting list, the toJSON function wraps the results in an array rather than a named object.

#també --- SEMBLA QUE VA BÉ, a falta de confirmació
library(jsonlite)
x <- toJSON(catusermin)
cat(x)
## [{"id":1,"value":"server1"},{"id":2,"value":"server2"},
## {"id":3,"value":"server3"},{"id":4,"value":"server4"},
## {"id":5,"value":"server5"}]
