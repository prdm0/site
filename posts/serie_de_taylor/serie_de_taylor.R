library(ggplot2)
library(purrr)
library(glue)
library(gganimate)

# Série de Taylor ---------------------------------------------------------
taylor <- function(n = 1L, func, x, a = 0,...){
  an <- function(n, ...){
    Deriv::Deriv(f = func, nderiv = n, ...)
  }
  
  if(n == 1L)
    return(func(a))
  
  sapply(
    X = 1L:n,
    FUN = \(n, x) do.call(an(n, ...), list(x = x))/factorial(n) * (x - a)^n,
    x = x
  ) |> sum() + func(a) 
}

plot_taylor <- function(n = 1L, func, a, lower = -1, upper = 1, ...){
  x <- seq(lower, upper, length.out = 100L)
  y <- func(x)
  
  y_taylor <- 
    sapply(
      X = x,
      FUN = \(x) taylor(n = n, func = func, x = x, a = a, ...)
    )
  
  data.frame(
    x = c(x, x), 
    y = c(y, y_taylor),
    classe = c(rep("f(x)", 100L), rep("Taylor", 100L))
  ) |> 
    ggplot() +
    geom_line(aes(x = x, y = y, color = classe), size = 0.9) +
    geom_point(x = a, y = func(a), color = "blue", size = 2) +
    ggtitle(
      label = "Série de Taylor", 
      subtitle = glue("Aproximação de uma função f(x), com n = {n}") 
    ) +
    ylab("f(x)") +
    scale_color_manual(values = c("black", "tomato")) + 
    theme(
      text = element_text(face = "bold")
    ) + 
    labs(color = "") 
}

# Aproximando func em torno de "a" ----------------------------------------
plot_taylor(n = 3, func = \(x) exp(x), lower = -1, upper = 1, a = 0.5)

taylor_animacao <- function(n = 1L, func, x, num_a = 5L, lower = -1, upper = 1, ...){

  x <- seq(lower, upper, length.out = 100L)
  y <- func(x)
  
  vec_taylor <- Vectorize(FUN = taylor, vectorize.args = "x")
  
  y_taylor <- 
    lapply(
      X = seq(lower, upper, length.out = num_a),
      FUN = \(a) vec_taylor(n = n, func = func, x = x, a = a)
    ) |> unlist()
  
  a <- rep(LETTERS[1:num_a], each = 100L)
  
  x <- rep(x, num_a)
  
  data.frame(x = x, y_taylor = y_taylor, classes = a) |> 
    ggplot(aes(x = x, y = y_taylor)) +
    geom_line(aes(x = x, y = y), data = data.frame(x = x, y = y), size = 2) +
    geom_line(color = "tomato", size = 2) +
    ggtitle(
      label = "Série de Taylor", 
      subtitle = glue("Aproximação de uma função f(x), com n = {n}") 
    ) +
    ylab("f(x)") +
    scale_color_manual(values = c("black", "tomato")) + 
    theme(
      text = element_text(face = "bold")
    ) + 
    labs(color = "") +
    transition_states(
      classes
    ) 
}

# taylor_animacao(n = 2L, func = \(x) exp(x), num_a = 3L, lower = -1, upper = 1)
