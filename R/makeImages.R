
plot_colour <- "#1F4E79" 
plot_colour_light <- "#5B9BD5"
  
#########################
### TWEEDIE MODELS: MOVIE
#########################

add_xi_slider <- function(xi, xi_min = 1, xi_max = 4){
  top_edge <- 0.55
  bottom_edge <- 0.5
  left_edge <- 2.75
  right_edge <- 4.75
  
  polygon( x = c(left_edge, left_edge, right_edge, right_edge),
           y = c(bottom_edge, top_edge, top_edge, bottom_edge),
           lwd = 1,
           border = "slateblue2",
           col = "white")
  
  xi_to_coordinates <- function(xi){
    (xi - xi_min) / (xi_max - xi_min) * (right_edge - left_edge) + left_edge 
  }
  
  # Moving xi-slider
  segments(x0 = xi_to_coordinates(xi_vec[i]),
           x1 = xi_to_coordinates(xi_vec[i]),
           y0 = bottom_edge,
           y1 = top_edge,
           lwd = 3,
           col = "slateblue4"
           )
  # Indicate xi == 2
  segments(x0 = xi_to_coordinates(2),
           x1 = xi_to_coordinates(2),
           y0 = bottom_edge,
           y1 = top_edge,
           lwd = 2,
           col = "slateblue2"
  )
  
  # Annotations
  text(x = left_edge,
       y = top_edge,
       pos = 3,
       expression(xi==1))
  text(x = right_edge,
       y = top_edge,
       pos = 3,
       expression(xi==4))
  text(x = xi_to_coordinates(2),
       y = top_edge,
       pos = 3,
       expression(xi==2))
  
  text(x = xi_to_coordinates( (xi_max + xi_min) / 2),
       y = bottom_edge,
       pos = 1,
       bquote(xi==.(xi_vec[i])) )
}




# Generate some Tweedie distributions
library(tweedie)

mu <- 1
phi <- 1
xi_vec <- c(1.01, 
            1.02,
            1.03,
            1.04,
            1.05,
            seq(1.1, 2,
                by = 0.1),
            seq(2.25, 4,
                by = 0.25)
            )

x <- seq(0, 4.5,
         length = 500)
maxy <- 1.2

for (i in 1:length(xi_vec)){
  png(paste0("TweedieDist",i,".png"), 
      width = 9, 
      height = 6,
      units = "in",
      res = 300)
  par( mar = c(5, 5, 4, 2) + 0.1, 
       xpd = NA)
  
  y <- tweedie::dtweedie(x,
                         mu = 1,
                         phi = 1,
                         xi = xi_vec[i])
  if ( xi_vec[i] < 2) {
    plot(y[x > 0] ~ x[x > 0],
         type = "l",
         ylab = "",
         xlab = expression(Values~of~the~variable),
         main = expression( "("*mean==1~and~variance==1*")"),
         col = plot_colour,
         lwd = 3,
         ylim = c(0, maxy),
         axes = FALSE)
    points(x = 0,
           y = y[x == 0],
           col = plot_colour,
           cex = 2,
           pch = 19)
    text(x = 0,
         y = y[x == 0],
         pos = 2,
         "Prob. zero")
  } else {
    plot(y[x > 0] ~ x[x > 0],
         type = "l",
         ylim = c(0, maxy),
         ylab = "",
         xlab = expression(Values~of~the~variable),
         main = expression(atop( mu==1~and~variance==1)),
         col = plot_colour,
         lwd = 3,
         axes = FALSE)
  }
  
  add_xi_slider(xi = xi_vec[i])
  axis(side = 1)

  # AXES
  abline(v = 0, 
         lwd = 2,
         col = "grey")  
  abline(h = 0, 
         lwd = 2,
         col = "grey")  
  dev.off()
}


# Package as a movie
library(gifski)
png_files <- c("TweedieDist1.png",
               "TweedieDist2.png",
               "TweedieDist3.png",
               "TweedieDist4.png",
               "TweedieDist5.png",
               "TweedieDist6.png",
               "TweedieDist7.png",
               "TweedieDist8.png",
               "TweedieDist9.png",
               "TweedieDist10.png",
               "TweedieDist11.png",
               "TweedieDist12.png",
               "TweedieDist13.png",
               "TweedieDist14.png",
               "TweedieDist15.png",
               "TweedieDist16.png",
               "TweedieDist17.png",
               "TweedieDist18.png",
               "TweedieDist19.png",
               "TweedieDist20.png",
               "TweedieDist21.png",
               "TweedieDist22.png",
               "TweedieDist23.png")
gifski(png_files, 
       "Images/Tweedie_animation.gif", 
       delay = 0.75)

# Delete those pdf files now
file.remove(list.files(pattern = "\\.png$"))

# Restore figure defaults
par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults

graphics.off()



#########################
### QUILPIE RAINFALL
#########################


par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults
qRain <- read.csv("Data/quilpie.csv") # Appears to be monthly data from JULYs

# Compute the decade:
qRain$Decade <- (qRain$Year %/% 10) * 10

mn <- tapply(qRain$Rain,
             list(qRain$Decade),
             mean)
vr <- tapply(qRain$Rain,
             list(qRain$Decade),
             var)

png("Images/Quilpie_logplot.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)
plot(log(vr) ~ log(mn),
     xlab = "Log of mean",
     ylab = "Log of variance",
     main = "Data (grouped by decade)",
     las = 1,
     pch = 19)
abline(lm(log(vr) ~ log(mn) ),
       lwd = 2)
text(x = 3.1,
     y = 5.8,
     cex = 1.25,
     expression(bold(Slope)==1.55))
dev.off()


# coef( lm(log(vr) ~ log(mn) ) )
# (Intercept)     log(mn) 
# 1.345490    1.554453 

png("Images/Quilpie_profileLLH.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

out <- tweedie_profile(qRain$Rain ~ 1,
                       do.plot = TRUE,
                       line_args = list(col = plot_colour,
                                        lwd = 3))
title(main = "Profile log-likelihood")
abline(v = out$xi.max,
       lwd = 2,
       lty = 2)
text(x = out$xi.max,
     y = -250,
     pos = 4,
     expression(hat(xi)==1.44)) 
dev.off()
# $xi.max
# [1] 1.444898







#########################
### QUILPIE RAINFALL: WITH COVARIATES
#########################
par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults

SOI_BOM <- read.csv("./Data/SOI_BoM.csv")


# CONVERT SOI data file: Do once, and save)
fixSOI <- FALSE
if (fixSOI){
  names(SOI_BOM) <- c("YM", 
                      "Value")

  SOI_BOM$Year  <- substr(SOI_BOM$YM, 1, 4)
  SOI_BOM$Month <- substr(SOI_BOM$YM, 5, 6)

  SOI_BOM <- SOI_BOM[, c("Year", "Month", "Value")]
  
  write.csv(SOI_BOM,
            "./Data/SOI.csv")
}


# ALIGN and COMBINE both files: Do once, and save)
fixAlignment <- FALSE
if (fixAlignment){
  # Reshape
  qRain_long <- reshape(
    qRain,
    direction = "long",
    varying = names(qRain),
    v.names = "Rain",
    timevar = "Month",
    times = 1:12,
    idvar = "Year"
  )
  
  # Add names
  qRain_long$Month <- sprintf("%02d", 
                              qRain_long$Month)

#  # Ensure 2 digits in SOI
#  SOI$Month <- sprintf("%02d", 
#                         SOI_BOM$Month)

  # Merge
  qSOI <- merge(qRain_long, 
                 SOI_BOM, 
                 by = c("Year", 
                        "Month"), 
                 all = TRUE)
  
  # Now remove bits where Rainfall data is nt available
  omit_Rows <- which(is.na(qSOI$Rain))
  qSOI <- qSOI[ -omit_Rows, ]
  
  write.csv(qSOI,
            "./Data/qSOI.csv")
}


library(statmod)
out <- tweedie_profile(Rain ~ SOI,
                       data = qRain )

png("Images/Quilpie_profile_plot.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)
out <- tweedie_profile(Rain ~ SOI,
                       data = qRain,
                       #col = plot_colour,
                       do.plot = TRUE)
dev.off()

model <- glm(Rain ~ SOI,
             data = qRain,
             family = tweedie(var.power = 1.4,
                              link.power = 0) )




png("Images/Quilpie_dataplot.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

plot(Rain ~ SOI, 
     data = qRain,
     ylab = "Rainfall (in mm)",
     col = plot_colour,
     lwd = 2,
     cex = 1.25,
     las = 1)
points(Rain ~ SOI,
       col = plot_colour,
       data = subset(qRain,
                     Rain == 0),
       cex = 1.25,
       pch = 19)
newSOI <- data.frame(SOI = seq(min(qRain$SOI), max(qRain$SOI),
                               length = 100) )
newRain <- predict(model,
                   type = "response",
                   newdata = newSOI)
lines(newRain ~ newSOI$SOI,
      col = plot_colour,
      lwd = 3)

dev.off()




png("Images/Quilpie_qresiduals.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

qr <- qresid(model)
qqnorm(qr,
       main = "QQ plot of quantile residuals",
       col = plot_colour,
       las = 1,
       pch = 19)
qqline(qr)

dev.off()






#########################
### INTEGRANDS
#########################
par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults

library(tweedie)

tA = seq(0, 45, 
         length = 5000) 
out <- tweedie_integrand(y = 1,
                         mu = 1,
                         power = 1.9, 
                         phi = 1,
                         t = tA,
                         whichPlots = NA,
                         plot_args = list(plot_colour,
                                          lwd = 2) )

png("Images/Integration_integrand.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

plot(x = tA,
     y = out$IG,
     type = "l",
     lwd = 2,
     las = 1,
     col = plot_colour,
     main = expression("Integrand:"~mu==1*";"~phi==1*";"~xi==1.9),
     xlab = expression(italic(t)),
     ylab = expression(Expression) )
abline(h = 0, 
       col = "grey")

dev.off()


### Shading movie: each individual region
# Find the zeros


Integrand_Zeros <- which(diff(sign( out$IG )) != 0)
Integrand_Zeros <- c(1, Integrand_Zeros)

Number_Zeros <- length(Integrand_Zeros)

Bounds <- c(Integrand_Zeros, 
            length(tA))

for (i in 1:(length(Bounds) - 2)) {
  
  png(paste0("Integration_integrand_Shaded",i,".png"),
      width = 8, 
      height = 5,
      units = "in",
      res = 300)

  plot(x = tA,
       y = out$IG,
       type = "l",
       lwd = 2,
       las = 1,
       col = plot_colour,
       main = expression("Integrand:"~mu==1*";"~phi==1*";"~xi==1.9),
       xlab = expression(italic(t)),
       ylab = expression(Expression) )
  abline(h = 0, 
         col = "grey")
  
  
  Left  <- tA[Bounds[i    ]]
  Right <- tA[Bounds[i + 1]]
  Selection <- (tA >= Left) & (tA <= Right)
  
  if ( i == 1){
    polygon(
      x = c(0, tA[Selection]),
      y = c(0, out$IG[Selection]),
      col = plot_colour_light
    )
    
  } else {
    polygon(
      x = tA[Selection],
      y = out$IG[Selection],
      col = plot_colour_light
    )
  }
  
  dev.off()
}


# Package as a movie
library(gifski)
png_files <- c("Integration_integrand_Shaded1.png",
               "Integration_integrand_Shaded2.png",
               "Integration_integrand_Shaded3.png",
               "Integration_integrand_Shaded4.png",
               "Integration_integrand_Shaded5.png",
               "Integration_integrand_Shaded6.png",
               "Integration_integrand_Shaded7.png",
               "Integration_integrand_Shaded8.png",
               "Integration_integrand_Shaded9.png",
               "Integration_integrand_Shaded10.png",
               "Integration_integrand_Shaded11.png",
               "Integration_integrand_Shaded12.png",
               "Integration_integrand_Shaded13.png",
               "Integration_integrand_Shaded14.png")
gifski(png_files, 
       "Images/Integration_animation.gif", 
       width = 2400,
       height = 1500,
       delay = 0.75)

# Delete those pdf files now
file.remove(list.files(pattern = "\\.png$"))

# Restore figure defaults
par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults





### Shading movie: cumulative regions
# Find the zeros

AreaR <- 0
AreaTotal <- 0

for (i in 1:(length(Bounds) - 2)) {
  
  png(paste0("Integration_CumIntegrand_Shaded",i,".png"),
      width = 8, 
      height = 5,
      units = "in",
      res = 300)
  
  plot(x = tA,
       y = out$IG,
       type = "l",
       lwd = 2,
       las = 1,
       col = plot_colour,
       main = expression("Integration:"~mu==1*";"~phi==1*";"~xi==1.9),
       xlab = expression(italic(t)),
       ylab = expression(Expression) )
  abline(h = 0, 
         col = "grey")

  # Envelope
  lines( x = tA,
         y = exp(out$Real),
         lwd = 2,
         col = "grey",
         lty = 2)
  lines( x = tA,
         y = -exp(out$Real),
         lwd = 2,
         col = "grey",
         lty = 2)
  
  Left  <- tA[Bounds[i    ]]
  Right <- tA[Bounds[i + 1]]
  Selection <- (tA >= Left) & (tA <= Right)
  
  LeftR  <- 0
  RightR <- tA[Bounds[i + 1]]
  SelectionR <- (tA >= LeftR) & (tA <= RightR)
  
  
  # Region areas
  f <- splinefun(tA[Selection],
                 out$IG[Selection] )
  AreaR <- integrate(f, 
                     Left, 
                     Right)$value
  AreaTotal <- AreaTotal + AreaR
cat("This area:", AreaR, " (b/t ", Left, Right, "); Total area:", AreaTotal, "\n")
  # Plots  
  if ( i > 0){
    polygon(
      x = c(0, tA[SelectionR]),
      y = c(0, out$IG[SelectionR]),
      col = plot_colour_light
    )
    
    text(x = 30,
         y = 0.5,
         paste("This area:", sprintf("% .3f", AreaR), 
               "; Total area:", sprintf("% .3f", AreaTotal) )
    )

  } 
  
  dev.off()
}


# Package as a movie
library(gifski)
png_files <- c("Integration_CumIntegrand_Shaded1.png",
               "Integration_CumIntegrand_Shaded2.png",
               "Integration_CumIntegrand_Shaded3.png",
               "Integration_CumIntegrand_Shaded4.png",
               "Integration_CumIntegrand_Shaded5.png",
               "Integration_CumIntegrand_Shaded6.png",
               "Integration_CumIntegrand_Shaded7.png",
               "Integration_CumIntegrand_Shaded8.png",
               "Integration_CumIntegrand_Shaded9.png",
               "Integration_CumIntegrand_Shaded10.png",
               "Integration_CumIntegrand_Shaded11.png",
               "Integration_CumIntegrand_Shaded12.png",
               "Integration_CumIntegrand_Shaded13.png",
               "Integration_CumIntegrand_Shaded14.png")
gifski(png_files, 
       "Images/IntegrationCumulative_animation.gif", 
       width = 2400,
       height = 1500,
       delay = 0.75)

# Delete those pdf files now
file.remove(list.files(pattern = "\\.png$"))

# Restore figure defaults
par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults










#########################
### INTEGRANDS
#########################

par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults

tC <- seq(0, 20, 
         length = 5000) 

png(paste0("Images/Integration_Integrand_Imk.png"),
    width = 8, 
    height = 5,
    units = "in",
    res = 300)
out <- tweedie_integrand(y = 1,
                         mu = 1,
                         power = 1.9, 
                         phi = 1,
                         t = tC,
                         type = "PDF",
                         whichPlots = 1,
                         plot_args = list(lwd = 3,
                                          col = plot_colour))
dev.off()

png(paste0("Images/Integration_Integrand_cosImk.png"),
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

out <- tweedie_integrand(y = 1,
                         mu = 1,
                         power = 1.9, 
                         phi = 1,
                         t = tC,
                         whichPlots = 2,
                         plot_args = list(lwd = 3,
                                          col = plot_colour))
dev.off()






#########################
### INTEGRANDS: tricky
#########################

xi <- 1.003
y  <- 0.25
mu <- 1
phi <- 1


par( mar = c(5, 4, 4, 2) + 0.1) # Restore defaults

tB <- seq(0, 40, 
          length = 500)
png("Images/Integration_Integrand_Imk_A.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)
out <- tweedie_integrand(y = y,
                         mu = mu,
                         power = xi, 
                         phi = phi,
                         t = tB, 
                         whichPlots = 1,
                         plot_args = list(lwd = 3,
                                          col = plot_colour))
dev.off()

png("Images/Integration_Integrand_cosImk_A.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

out <- tweedie_integrand(y = y,
                         mu = mu,
                         power = xi, 
                         phi = phi,
                         t = tB,
                         whichPlots = 2,
                         plot_args = list(lwd = 3,
                                          col = plot_colour))

dev.off()








#########################
### SERIES
#########################

# Need to source some of the tweedie packages files to get access to the required functions

W <- function(xi, phi, j){
  alpha <- (2 - xi) / (1 - xi)
#  W_numerator <- y ^ (-j * alpha) * (xi - 1) ^ (alpha * j)
#  W_denominator <- phi^(j * (1 - alpha)) * (2 - xi)^j * factorial(j) * gamma(-j * alpha)
  
  
  log_W_numerator   <- (-j * alpha) * log(y) + 
                       (j * alpha) * log(xi - 1)
  log_W_denominator <- (j * (1 - alpha)) * log(phi) +
                       (j * log(2 - xi)) + 
                       lfactorial(j) + 
                       lgamma(-j * alpha)
  cat("num, denom=", log_W_numerator, log_W_denominator, "\n")
  exp(log_W_numerator - log_W_denominator)
}

xi <- 1.5
y  <- 0.25
mu <- 1
phi <- 1

out <- dtweedie_series(y = y, 
                       power = xi, 
                       mu = mu, 
                       phi = phi, 
                       details = TRUE)
j_A <- 1 : (out$hi + 2)
terms <- W(xi = xi, 
           phi = phi,
           j = j_A)



png("Images/Series_Easy.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

plot(x = j_A,
     y = terms,
     type = "h",
     main = "(Normalized) terms in the infinite series",
     xlab = expression(Index~italic(j)),
     ylab = "Value of terms in series",
     col = plot_colour,
     lwd = 2,
     las = 1)
points(x = j_A,
       y = terms,
       col = plot_colour,
       pch = 19)
abline(h = 0,
       lwd = 2,
       col = "grey")
dev.off()     
     








xi <- 1.999
y  <- 1
mu <- 1
phi <- 0.01

out <- dtweedie_series(y = y, 
                       power = xi, 
                       mu = mu, 
                       phi = phi, 
                       details = TRUE)
j_B <-  (out$lo - 2) : (out$hi + 2)
terms <- W(xi = xi, 
           phi = phi,
           j = j_B)




y <- 1
phi <- 0.01
mu <- 1
xi_vec <- c(1.001,
            seq(1.1, 1.9, by = 0.05),
            1.95,
            1.96,
            1.97,
            1.98,
            1.99,
            1.999)
num_terms <- lo <- hi <- numeric( length(xi_vec))

for (i in 1:length(xi_vec)){
  out <- dtweedie_series(y = y, 
                         power = xi_vec[i], 
                         mu = mu, 
                         phi = phi, 
                         details = TRUE)
  lo[i] <- out$lo
  hi[i] <- out$hi
  num_terms[i] <- hi[i] - lo[i]
    
}


png("Images/Series_Number_Terms.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)
plot(x = xi_vec,
     y = num_terms,
     pch = 19,
     las = 1,
     type = "l",
     lwd = 3,
     col = plot_colour,
     main = "The number of terms needed",
     xlab = expression(xi),
     ylab = "Number of terms")
abline(h = 0,
       col = "grey")
dev.off() 
     
     
     
png("Images/Series_Highest_Index.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

par(mar = c(5, 5, 4, 2) + 0.1,  # DEFAULT: 5.1 4.1 4.1 2.1
    mgp = c(4, 1, 0)  ) # DEFAULT: c(3, 1, 0) (axis title, tick labels, axis line)
options(scipen = 999)
plot(x = xi_vec,
     y = hi,
     pch = 19,
     las = 1,
     type = "l",
     lwd = 3,
     col = plot_colour,
     main = "Largest index needed",
     xlab = expression(xi),
     ylab = "Largest index")
abline(h = 0,
       col = "grey")
dev.off() 




png("Images/Range_Index.png",
    width = 8, 
    height = 5,
    units = "in",
    res = 300)

par(mar = c(5, 5, 4, 2) + 0.1,  # DEFAULT: 5.1 4.1 4.1 2.1
    mgp = c(4, 1, 0)  ) # DEFAULT: c(3, 1, 0) (axis title, tick labels, axis line)
options(scipen = 999)
plot(x = range(xi_vec),
     y = range( c(lo, hi) ),
     type = "n",
     las = 1,
     log = "y",
     main = "Range of indicies (log scale)",
     xlab = expression(xi),
     ylab = "Index number")
polygon(x = c( xi_vec,
            rev(xi_vec) ),
     y = c( (hi),
              rev((lo)) ),
     ylog = TRUE,
     pch = 19,
     las = 1,
#     type = "l",
     lwd = 3,
     col = plot_colour_light
)
text(x = 1.6,
     y = 80,
     "Lower index limit")
text(x = 1.6,
     y = 800,
     "Upper index limit")

xi_star <- 1.98
abline(v = xi_star,
       col = "grey")
text(x = xi_star,
     y = 50000,
     pos = 2,
     paste("Index range: 4490 to 5616" ))

dev.off() 










# RESTORE
par( mar = c(5, 5, 4, 2) + 0.1,  # DEFAULT: 5.1 4.1 4.1 2.1
     mgp = c(3, 1, 0) )
options(scipen = 0) # DEFAULT: 0

