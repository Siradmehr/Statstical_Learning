###################################################
#         Arthour : Radmehr karimian              #
#                   98103556                      #
#           Covid 19 deaths in Iran               #
###################################################
####load libaries####
library(data.table)
library(ggplot2)
####load datas####
data = fread('/Users/apple/Documents/iranprovs_mortality_monthly.csv',encoding = 'UTF-8')
data$yandm = data$y + data$m / 13
data_pmy = data[, .(n = sum(n)) , .(y,m,prov,yandm)]
data_pmy$covid_deaths = 0
Covid_19_start  = 1398 + 10/13 ## because year has 12 month and because 12/12 equals 1 we use 13 for not overlapping
####regression####
for (i in 1 : 31){
  prv = unique(data_pmy$prov)[i]
  for (j in 1 : 12){
    month = j
    data_pmy_clear  = data_pmy[prov == prv & m == month]
    data_pmy_clear$death = 0
    data_pmy_clear = data_pmy_clear[yandm > Covid_19_start - 5]
    data_pmy_fit = data_pmy_clear[yandm < Covid_19_start]
    fit = lm(n ~ yandm,data_pmy_fit)
    pvalue = summary(fit)$coefficients[,4][2]
    if(pvalue > 0.6){
      data_pmy_clear$predict = mean(data_pmy_fit$n)
    } else{
      data_pmy_clear$predict = predict(fit,data_pmy_clear)
    }
    sigma = summary(fit)$sigma
    ####plot####
    data_pmy_clear[2*sigma + data_pmy_clear$predict < data_pmy_clear$n]$covid_deaths = data_pmy_clear[2*sigma + data_pmy_clear$predict < data_pmy_clear$n]$n - data_pmy_clear[2*sigma + data_pmy_clear$predict < data_pmy_clear$n]$predict
    data_pmy[prov == prv & m == month & yandm > Covid_19_start - 5]$covid_deaths = data_pmy_clear$covid_deaths
    ggplot(data_pmy_clear)+
      geom_smooth(aes(yandm,predict),method = 'lm', color = 'red')+
      geom_point(aes(yandm,n),color = 'blue')+
      scale_x_continuous(breaks = 1393:1401)+
      ggtitle(label = prv,subtitle = paste("month=",month))
  }
}
####heatmap####
heatmap <- ggplot(data_pmy[yandm>Covid_19_start],aes(data_pmy[yandm>Covid_19_start]$yandm,data_pmy[yandm>Covid_19_start]$prov))+
  geom_tile(aes(fill = data_pmy[yandm>Covid_19_start]$covid_deaths))+ 
  scale_fill_gradient(low = "green",high="red")
heatmap
total_deaths = sum(data_pmy[yandm >Covid_19_start]$covid_deaths)
ceiling(total_deaths)
ratio = numeric()
provs = numeric()
provs_death = numeric()
####death ratios & province deaths####
for (i in 1:31){
  prv = unique(data_pmy$prov)[i]
  prov_death = ceiling(sum(data_pmy[yandm > Covid_19_start & prov == prv]$covid_deaths))
  all_death = sum(data_pmy[yandm > Covid_19_start & prov == prv]$n)
  ratio_prov = prov_death / all_death
  ratio <- append(ratio,ratio_prov)
  provs <- append(provs,prv)
  provs_death <- append(provs_death,prov_death)
}
deaths_ratio <- data.frame(provs, ratio,provs_death)
####save datas####
write.csv(deaths_ratio, "/Users/apple/Documents/ deaths_ratio.csv", row.names=FALSE)
