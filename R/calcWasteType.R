#' @title calcWasteType
#' @description Calculates shares of waste types based on Dirichlet regression on gdp using WhataWaste2.0 data
#' @param weight population weights or other weights or NULL
#' @param SSP SSP scenario
#' @author David Chen
#' @return magpie object of waste shares


calcWasteType <- function(weight="pop", SSP="SSP2"){

rstan_options(auto_write = TRUE)
options(mc.cores = 4)


tmp <- readSource("Waste", subtype="Composition")
gdppc <- calcOutput("GDPpc",aggregate=F)
gdp<- time_interpolate(gdppc, interpolated_year= getYears(tmp) , extrapolation_type = "linear")
gdp <- as.data.frame(gdp[,,"SSP2"])
colnames(gdp) <- c( "cell", "region", "year", "data1", "gdp")

pop <- calcOutput("Population",aggregate=F)
pop<- time_interpolate(pop, interpolated_year= getYears(tmp) , extrapolation_type = "linear")
pop <- as.data.frame(pop[,,"pop_SSP2"])
colnames(pop) <- c( "cell", "region", "year", "data1", "pop")

#closure
tmp <- tmp/dimSums(tmp, na.rm=T, dim=3)
#years where data exists
years <- as.data.frame(where(dimSums(tmp, dim=3, na.rm=T)==0)$false$`individual`)
years$year <- gsub("y", x=years$year, replacement="")
colnames(years)[1] <- "region"
tmp <- as.data.frame(tmp)
colnames(tmp) <-  c("cell","region","year","type","value") 

#only the years that have value
tmp <- merge(tmp, years[c("region", "year")])
tmp[which(is.na(tmp$value)),"value"] <- 0
tmp<- select(tmp, -cell)

tmp <- unite(tmp, reg_year, c("region","year")) %>% 
  spread(key=type, value=value)

df <- unite(gdp,col="reg_year", c(region, year)) %>% 
  select(-c(cell,data1)) %>% 
  inner_join(tmp, ., by="reg_year") 


pop <- unite(pop,col="reg_year", c(region, year)) %>% 
  select(-c(cell,data1))
df<- inner_join(df, pop, by="reg_year") %>% 
  filter(gdp<100000)

# combined
df[,"other"] <- df[,"rubber_leather"] + df[,"wood_waste"] + df[,"other"]  
#df[,"other"] <- df[,"other"] + df[,"rubber_leather"]  
WD <- DR_data(df[,2:7])


WD <- matrix(WD, ncol=6)

colnames(WD) <- c(1:ncol(WD))
#WD[,c(1,2)] <- WD[ ,c(2,1)]

df$WD <- with(df,cbind(WD))
#log scale
df$gdp <- log(df$gdp)

df$pop <- length(df$pop)*(df$pop/sum(df$pop))

if (weight== "pop") {
  reg <- brm(WD|weights(pop) ~ gdp, data = df, family = 'dirichlet', inits="0", cores=4)  
}

else if (weight == "none") {
  reg <- brm(WD~ gdp, data = df, family = 'dirichlet', inits="0", cores=4)
}

gdppc <- calcOutput("GDPpc", aggregate=F)[,,SSP]
gdppc <- as.data.frame(gdppc)
gdppc$Value <- log(gdppc$Value)

tmp <- fitted(reg, newdata=data.frame(gdp=gdppc$Value))
tmp <- tmp[,c(1,3,4),]
dimnames(tmp)[[3]] <- c("organic","glass","metal", "other", "paper", "plastic")
df1 <- cbind(gdppc, tmp)
df1 <- df1[,-c(1,5)]
colnames(df1)[3] <- "scenario"
colnames(df1) <- gsub(".5","5",colnames(df1))

df2 <- gather(df1, key="type",value = "value", 4:21)
x <- as.magpie(df2)
getSets(x) <- c("Region","Year","scenario","bounds","type")    

x <- dimOrder(x, c(2,1,3))

return(list(
  x=x,
  weight=NULL,
  unit="share",
  description="Share of waste types"))

}
