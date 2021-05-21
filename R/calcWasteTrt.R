#' @title calcWasteDirTrt
#' @description Calculates shares of waste treatments by type based on Dirichlet regression on gdp using WhataWaste2.0 data
#' note that each type is independent - treatments for each type all sum to 1
#' @param weight population weights or "none"
#' @param SSP SSP scenario
#' @author David Chen
#' @return magpie object of waste treatment by type share
#' @export
#' @importFrom tidyr gather spread unite
#' @importFrom dplyr select inner_join filter
#' @importFrom magclass time_interpolate as.data.frame
#' @importFrom brms brm
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats na.omit fitted
#' @importFrom tidyselect contains


calcWasteTrt <- function(weight="pop", SSP="SSP2"){
 # rstan_options(auto_write = TRUE)
  options(mc.cores = 4)

RegTrt <-function(type, weight="pop"){

x <- calcOutput("NlWasteDistrib", aggregate=F)
#gdp
gdppc <- calcOutput("GDPpc",aggregate=F)
gdp<- time_interpolate(gdppc, interpolated_year= getYears(x) , extrapolation_type = "linear")
gdp <- as.data.frame(gdp[,,"SSP2"])
colnames(gdp) <- c( "cell", "region", "year", "data1", "gdp")
#pop
pop <- calcOutput("Population",aggregate=F)
pop<- time_interpolate(pop, interpolated_year= getYears(x) , extrapolation_type = "linear")
pop <- as.data.frame(pop[,,"pop_SSP2"])
colnames(pop) <- c( "cell", "region", "year", "data1", "pop")

prepDirReg <- function(input, remove){
  #specify type and remove
  #sum up wood and paper, and rubber and other
  x[,,"other"] <- x[,,"rubber"] + x[,,"wood"] + x[,,"other"]
  a<-as.data.frame(x[,,input])
  a<- a[,-1]
  a<-  na.omit(a)
  #some optim values (have small negative values solved, round and then  make the others 0 for now,,get closed anwyays)
  a[,5] <- round(a[,5],4)
  negs <- which(a[,5] <0)
  a[negs,5] <- 0
  a <- spread(a, key="Data2", value="Value") %>%
    select(-c(3, remove)) %>%
    unite(col="reg_year", c("Region", "Year"))

  gdp <- unite(gdp,col="reg_year", c("region", "year")) %>%
    select(-c("cell","data1"))

  a <-  inner_join(a, gdp, by="reg_year")

  pop <- unite(pop,col="reg_year", c("region", "year")) %>%
    select(-c("cell","data1"))
  a <-  inner_join(a, pop, by="reg_year")
a <-    filter(a, .data$gdp < 90000)


  WD <- a[,2:(ncol(a)-2)]/rowSums(a[,2:(ncol(a)-2)])
  WD <- (WD * (nrow(WD) - 1) + 1/ncol(WD))/nrow(WD)
 rm <- which(is.na(rowSums(WD)))
   WD <- WD[-rm,]
  WD <- as.matrix(WD, ncol=4)

  a <- a[-rm,]
  a$pop <- length(a$pop)*(a$pop/sum(a$pop))

  return(list(a,WD))
}

organic <- prepDirReg(input="organic", remove="recyc")
glass <- prepDirReg(input="glass", remove=c("compost", "incineration"))
metal<- prepDirReg(input="metal", remove=c("compost", "incineration"))
other<- prepDirReg(input="other", remove=c("compost"))
paper<- prepDirReg(input="paper", remove=c("compost"))
plastic<- prepDirReg(input="plastic", remove=c("compost"))

results_list <- c(organic,glass,metal,other,paper,plastic)
names <- c("organic","glass","metal","other","paper","plastic")

WD1 <- as.list(results_list[seq(from=2,to=12,by=2)] )
names(WD1) <- names
a1 <- as.list(results_list[seq(from=1,to=12,by=2)] )
names(a1) <- names

WD <- WD1[[type]]
df <- a1[[type]]


WD <- matrix(WD, ncol=ncol(df)-3)

colnames(WD) <- c(1:ncol(WD))
df$WD <- with(df,cbind(WD))
df$gdp <- log(df$gdp)
df$pop <- length(df$pop)*(df$pop/sum(df$pop))


if(weight=="pop"){
  reg <- brm(WD|weights(pop) ~ gdp, data=df, family='dirichlet',
             inits='0', cores=4)
}
if (weight=="none"){
  reg <- brm(WD ~ gdp, data=df, family='dirichlet',
             inits='0', cores=4)
}

return(reg)
}

gdppc <- calcOutput("GDPpc", aggregate=F)[,,SSP]
gdppc <- as.data.frame(gdppc)
gdppc$Value <- log(gdppc$Value)

types <- c("organic", "paper","plastic","glass","metal","other")
if(weight=="pop"){
  tmp <- lapply(types, RegTrt, weight="pop")
  tmp2 <- lapply(tmp, fitted, newdata=data.frame(gdp=gdppc$Value))
}
if(weight=="none"){
  tmp <- lapply(types, RegTrt, weight="none")
  tmp2 <- lapply(tmp, fitted, newdata=data.frame(gdp=gdppc$Value))
}

names(tmp2) <- types
tmp2 <- as.data.frame(tmp2)
tmp3 <- select(tmp2, -contains("Error"))
colnames(tmp3) <- gsub(".5","5",colnames(tmp3))

colnames(tmp3)[1:12] <- gsub("P.Y...1.","compost", colnames(tmp3)[1:12])
colnames(tmp3)[1:12] <- gsub("P.Y...2.","incineration", colnames(tmp3)[1:12])
colnames(tmp3)[1:12] <- gsub("P.Y...3.","landfills", colnames(tmp3)[1:12])
colnames(tmp3)[1:12] <- gsub("P.Y...4.","dumps", colnames(tmp3)[1:12])

colnames(tmp3)[13:24] <- gsub("P.Y...1.","recyc", colnames(tmp3)[13:24])
colnames(tmp3)[13:24] <- gsub("P.Y...2.","incineration", colnames(tmp3)[13:24])
colnames(tmp3)[13:24] <- gsub("P.Y...3.","landfills", colnames(tmp3)[13:24])
colnames(tmp3)[13:24] <- gsub("P.Y...4.","dumps", colnames(tmp3)[13:24])

colnames(tmp3)[25:36] <- gsub("P.Y...1.","recyc", colnames(tmp3)[25:36])
colnames(tmp3)[25:36] <- gsub("P.Y...2.","incineration", colnames(tmp3)[25:36])
colnames(tmp3)[25:36] <- gsub("P.Y...3.","landfills", colnames(tmp3)[25:36])
colnames(tmp3)[25:36] <- gsub("P.Y...4.","dumps", colnames(tmp3)[25:36])

colnames(tmp3)[37:45] <- gsub("P.Y...1.","recyc", colnames(tmp3)[37:45])
colnames(tmp3)[37:45] <- gsub("P.Y...2.","landfills", colnames(tmp3)[37:45])
colnames(tmp3)[37:45] <- gsub("P.Y...3.","dumps", colnames(tmp3)[37:45])

colnames(tmp3)[46:54] <- gsub("P.Y...1.","recyc", colnames(tmp3)[46:54])
colnames(tmp3)[46:54] <- gsub("P.Y...2.","landfills", colnames(tmp3)[46:54])
colnames(tmp3)[46:54] <- gsub("P.Y...3.","dumps", colnames(tmp3)[46:54])

colnames(tmp3)[55:66] <- gsub("P.Y...1.","recyc", colnames(tmp3)[55:66])
colnames(tmp3)[55:66] <- gsub("P.Y...2.","incineration", colnames(tmp3)[55:66])
colnames(tmp3)[55:66] <- gsub("P.Y...3.","landfills", colnames(tmp3)[55:66])
colnames(tmp3)[55:66] <- gsub("P.Y...4.","dumps", colnames(tmp3)[55:66])



#set impossible combinations to 0
zeros <- c("organic.Estimate.recyc", "organic.Q25.recyc","organic.Q975.recyc",
           "paper.Estimate.compost", "paper.Q25.compost","paper.Q975.compost",
           "plastic.Estimate.compost", "plastic.Q25.compost","plastic.Q975.compost",
           "metal.Estimate.compost", "metal.Q25.compost","metal.Q975.compost",
           "metal.Estimate.incineration", "metal.Q25.incineration","metal.Q975.incineration",
           "glass.Estimate.compost", "glass.Q25.compost","glass.Q975.compost",
           "glass.Estimate.incineration", "glass.Q25.incineration","glass.Q975.incineration",
           "other.Estimate.compost", "other.Q25.compost","other.Q975.compost")
tmp3[zeros] <-0



df <- cbind(gdppc, tmp3)
df <- df[,-c(1,5)]
colnames(df)[3] <- "scenario"
df <- gather(df, key="trt",value = "value", 4:93)
x <- as.magpie(df)
getNames(x) <- gsub("_","\\.", getNames(x))
getSets(x) <- c("region","year","scenario", "type", "bounds","trt")

x <- dimOrder(x, c(2,4,3,1))

return(list(
  x=x,
  weight=NULL,
  unit="share",
  description="Share of waste treatments by types"))
}
