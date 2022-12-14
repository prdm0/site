library(ggplot2)
library(tidyr)
library(dplyr)
library(latex2exp)

set.seed(0)

dados <- rweibull(n = 500L, shape = 2.5, scale = 1.5)

pdf_weibull <- function(x, alpha, beta)
  alpha/beta * (x/beta)^(alpha-1) * exp(-(x/beta)^alpha)

# log-verossimilhança
log_likelihood <- function(x, alpha, beta)
  prod(pdf_weibull(x, alpha, beta))

vec_log_likelihood <- 
  Vectorize(
    FUN = log_likelihood,
    vectorize.args = c("alpha", "beta")
  )

alpha <- seq(2.4, 2.87, length.out = length(dados))
beta <- seq(1.42, 1.56, length.out = length(dados))
df_contour <- expand_grid(alpha, beta)

df_contour <- 
  df_contour |> 
  mutate(z = vec_log_likelihood(x = dados, alpha, beta))
  
df_contour |> 
  ggplot() + 
  geom_contour_filled(aes(x = alpha, y = beta, z = z), show.legend = FALSE) +
  ggtitle(
    label = "Curvas de níveis da fução de Verossimilhança", 
  ) +
  xlab(TeX(r'(\alpha)')) +
  ylab(TeX(r'(\beta)')) +
  labs(fill = "Níveis") +
  theme(
    plot.title  = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  # R
  geom_point(
    x = 2.64276,
    y = 1.488277 ,
    color = "blue",
    size = 3
  ) +
  # Python
  geom_point(
    x = 2.642850353244295,
    y = 1.488271661049081,
    color = "green",
    size = 2.5
  ) + 
  # Julia
  geom_point(
    x = 2.642850346755736,
    y = 1.4882716649411558 ,
    color = "red",
    size = 2
  )
