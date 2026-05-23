set.seed(2302)

# Receitas Empréstimos
remp <- c(17240972.62, 22887819.44, 27154113.17, 28431184.15, 27837381.39)

# Custos Empréstimos
cemp <- c(9881917.66, 13717674.33, 16529890.75, 17006139.08, 13422037.97)

# Perdas
pauf <- c(4273206.25, 1707172.27, 3413051.51, 4303517.55, 3118459.23)

# Receita Captação
rcap <- c(17082981.10, 21857139.60, 27286675.43, 29830049.65, 25437200.99)

# Custos Captação
ccap <- c(12144863.89, 16468313.68, 20952502.24, 22800071.35, 18767726.39)

# Receitas Serviços
rvs <- c(4404808.64, 3621758.53, 4154535.69, 3922513.87, 5619494.85)

# Custos Fixos Administrativos
cadm <- c(5941205.94, 6970836.13, 6880157.87, 7426871.81, 7132300.12)

mRemp <- mean(remp)
sRemp   <- sd(remp)

mCemp <- mean(cemp)
sCemp   <- sd(cemp)

mPauf <- mean(pauf)
sPauf   <- sd(pauf)

mRcap <- mean(rcap)
sRcap   <- sd(rcap)

mCcap <- mean(ccap)
sCcap   <- sd(ccap)

mRsv  <- mean(rvs)
sRsv    <- sd(rvs)

mCadm <- mean(cadm)
sCadm   <- sd(cadm)

simular_cenario <- function(n_sim = 50000, mult_perdas = 1, cadm = 1,
                            mean_Remp = mRemp, sd_Remp = sRemp,
                            mean_Cemp = mCemp, sd_Cemp = sCemp,
                            mean_Pauf = mPauf, sd_Pauf = sPauf,
                            mean_Rcap = mRcap, sd_Rcap = sRcap,
                            mean_Ccap = mCcap, sd_Ccap = sCcap,
                            mean_Rsv = mRsv, sd_Rsv = sRsv,
                            mean_Cadm = mCadm, sd_Cadm = sCadm) {
  
  Remp <- rnorm(n_sim, mean_Remp, sd_Remp)
  Cemp <- rnorm(n_sim, mean_Cemp, sd_Cemp)
  Pauf <- rnorm(n_sim, mean_Pauf * mult_perdas, sd_Pauf)
  
  Rcap <- rnorm(n_sim, mean_Rcap, sd_Rcap)
  Ccap <- rnorm(n_sim, mean_Ccap, sd_Ccap)
  
  Rsv  <- rnorm(n_sim, mean_Rsv, sd_Rsv)
  
  Cadm <- rnorm(n_sim, mean_Cadm * cadm, sd_Cadm)
  
  # Fórmula do artigo
  Run <- (Remp - Cemp - Pauf) +
    (Rcap - Ccap) +
    Rsv - Cadm
  
  return(Run)
}

# Cenário 1 (base)
cen1 <- simular_cenario()

# Cenário 2 (perdas maiores - pessimista)
cen2 <- simular_cenario(mult_perdas = 3)

# Cenário 3 (redução custos administrativos)
cen3 <- simular_cenario(cadm = 0.30)

# Cenário 4 (otimista - perdas menores + custos menores)
cen4 <- simular_cenario(mult_perdas = 0.8, cadm = 0.75)

resumo <- function(x) {
  c(
    Media = mean(x),
    Prob_lucro = mean(x > 0)
  )
}

rbind(
  Cenário1 = resumo(cen1),
  Cenário2 = resumo(cen2),
  Cenário3 = resumo(cen3),
  Cenário4 = resumo(cen4)
)

par(mfrow=c(2,2))

hist(cen1/1e6, main="Cenário 1", col="lightblue", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia" ,xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen2/1e6, main="Cenário 2", col="lightcoral", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen3/1e6, main="Cenário 3", col="lightgreen", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen4/1e6, main="Cenário 4", col="lightgoldenrod", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))


# Cenário 5 Custos Administrativo Mais Altos
cen5 <- simular_cenario(cadm = 3)

# Cenário 6 Maior variação nas Perdas
cen6 <- simular_cenario(mult_perdas = 2, sd_Pauf = sPauf*2)

# Cenário 7 Menos Empréstimos
cen7 <- simular_cenario(mean_Remp = mRemp/2, mean_Cemp = mCemp/2)

# Cenário 8 Mais Investimentos
cen8 <- simular_cenario(mean_Rcap = 2*rcap, mean_Ccap = 2*ccap)

hist(cen5/1e6, main="Cenário 5", col="blue", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia" ,xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen6/1e6, main="Cenário 6", col="red", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen7/1e6, main="Cenário 7", col="green", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))
hist(cen8/1e6, main="Cenário 8", col="gold", breaks=30, xlab = "Lucro Anual em Milhões", ylab = "Frênquecia", xlim = c(-30, 50), xaxt = "n")
axis(1, at = seq(-30, 50, by = 10))

rbind(
  Cenário5 = resumo(cen5),
  Cenário6 = resumo(cen6),
  Cenário7 = resumo(cen7),
  Cenário8 = resumo(cen8)
)
