options(bitmapType = "cairo")
library(ggplot2)
library(tweedie)
library(magick)

## ---- Generate a stylised Tweedie density ----
set.seed(1)
x  <- seq(0.001, 4, length.out = 400)
mu <- 1.2; phi <- 1; p <- 1.05

y  <- dtweedie(x, mu = mu, phi = phi, power = p)
y0 <- dtweedie(0, mu = mu, phi = phi, power = p)
df <- data.frame(x = x, y = y)

density_plot <- ggplot(df, aes(x, y)) +
  geom_line(linewidth = 1.2, colour = "#2C3E50") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  annotate("point", x = 0, y = y0, color = "#2C3E50", size = 1) +
  theme_void()

## ---- Create hex sticker ----
options(bitmapType = "cairo")
library(ggplot2)
library(tweedie)
library(magick)

## ---- Generate a stylised Tweedie density ----
set.seed(1)
x  <- seq(0.001, 4, length.out = 400)
mu <- 1.2; phi <- 1; p <- 1.05

y  <- dtweedie(x, mu = mu, phi = phi, power = p)
y0 <- dtweedie(0, mu = mu, phi = phi, power = p)
df <- data.frame(x = x, y = y)

density_plot <- ggplot(df, aes(x, y)) +
  geom_line(linewidth = 2, colour = "#2C3E50") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  annotate("point", x = 0, y = y0, color = "#2C3E50", size = 3) +
  theme_void()


## ---- Fix left border clipping with wider canvas ----
ggsave(
  "./Icons/tweedie_icon.png",
  plot   = density_plot,
  width  = 55,
  height = 55,
  units  = "mm",
  dpi    = 300,
  bg     = "white"
)

