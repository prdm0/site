# Quantidade de observações
n <- 250L

# Parâmetros que especificam a distribuição verdadeira, i.e., distribuição
# da variável aleatória cujo os dados são observações.
alpha <- 2.5
beta <- 1.5

# Fixando uma semente para o gerador de números pseudo-aleatórios. Assim, conseguimos,
# toda vez que rodamos o código, reproduzir os mesmos dados.
set.seed(3)

# Gerando as observações. Esse será o conjunto de dados que você tera para 
# modelar.
dados <- rweibull(n = n, shape = alpha, scale = beta)

pdf_weibull <- function(x, par){
  alpha <- par[1]
  beta <- par[2]
  alpha/beta * (x/beta)^(alpha-1) * exp(-(x/beta)^alpha)
}

# Checando se a densidade de pdf_weibull integra em 1
integrate(f = pdf_weibull, lower = 0, upper = Inf, par = c(2.5, 1.5))

# Implementando uma função genérica que implementa a função objetivo
# (função de log-verossimilhança) que iremos maximizar. Essa função 
# irá receber como argumento uma função densidade de probabilidade.
# Não é preciso destrinchar (obter de forma exata) a função de 
# log-verossimilhança!
# A função de log-verossimilhança encontra-se multiplicada por -1
# devido ao fato da função que iremos fazer otimização minimizar 
# uma função função objetivo. Minimizar -f equivale a maximizar f.
# Lembre-se disso!
log_likelihood <- function(x, pdf, par)
  -sum(log(pdf(x, par)))

result <- optim(
  fn = log_likelihood,
  par = c(0.5, 0.5),
  method = "BFGS",
  x = dados,
  pdf = pdf_weibull
)

# Imprimindo os valores das estimativas de máxima verossimilhança
cat("Valores estimados de alpha e beta\n")
cat("--> alpha: ", result$par[1], "\n")
cat("--> beta: ", result$par[2], "\n")
