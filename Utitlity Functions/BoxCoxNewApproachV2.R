library(FSAdata)
library(TSA)
library(AID)

BoxCoxSearch = function(y, lam=seq(-3,3,0.01), 
                        m= c("sf", "sw","ad" ,"cvm", "pt", "lt", "jb","ac"), plotit = T, verbose = T){
  N = length(m)
  BC.y = array(NA,N)
  for (i in 1:N){
    if (m[i] == "sf"){
      wrt = "Shapiro-Francia Test"
    } else if (m[i] == "sw"){
      wrt = "Shapiro-Wilk  Test"
    } else if (m[i] == "ad"){
      wrt = "Anderson-Darling Test"
    } else if (m[i] == "cvm"){
      wrt = "Cramer-von Mises Test"
    } else if (m[i] == "pt"){
      wrt = "Pearson Chi-square Test"
    } else if (m[i] == "lt"){
      wrt = "Lilliefors Test"
    } else if (m[i] == "jb"){
      wrt = "Jarque-Bera Test"
    } else if  (m[i] == "ac"){
      wrt = "Artificial covariate method"
    }
    
    print(paste0("------------- ",wrt," -------------"))
    out = boxcoxnc(y, method = m[i], lam = lam,plotit = plotit, verbose = verbose)
    BC.y[i] = as.numeric(out$p.value)
  }
  return(p.values = BC.y)
}

# --- Example 1 ---
data("ChinookKR")

y1 = ChinookKR$spawners
shapiro.test(y1)

spawners.transform = BoxCox.ar(y1)
spawners.transform$ci
lambda = 0
BC.chinook.spawners = log(y1) #((y^lambda)-1)/lambda
qqnorm(BC.chinook.spawners)
qqline(BC.chinook.spawners, col = 2)
shapiro.test(BC.chinook.spawners)

a = BoxCoxSearch(y1, plotit=T, verbose = T)


# --- Example 2 ---

data(gold)
y2 =  gold
shapiro.test(y2)

gold.transform = BoxCox.ar(y2)
gold.transform$ci
lambda = -1.8
BC.gold = ((y2^lambda)-1)/lambda
qqnorm(BC.gold)
qqline(BC.gold, col = 2)
shapiro.test(BC.gold)


lam=seq(-3,3,0.01)
#m = c("sf", "sw","ad" ,"cvm", "pt", "sf", "lt", "jb", "ac")
m = c("sw", "pt", "sf", "jb")
a = BoxCoxSearch(y, lam=lam, m= m)
