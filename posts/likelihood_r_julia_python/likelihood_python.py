import numpy as np
import scipy.stats as stat
import scipy.integrate as inte
import scipy.optimize as opt
import csv

# Valores da distribuição verdadeira
alpha = 2.5
beta = 1.5

# Número de observações que irão compor nossos dados
n = 250 

# Implementando a função random_weibull, em que os parâmetros
# que indexam a distribuição são argumentos da função. Tem mais
# sentido ser assim, não?
def random_weibull(n, alpha, beta):
  return beta * np.random.weibull(alpha,n)

# Escrevendo a funçãp densidade de probabilidade da Weibull
# na reparametrização correta.
def pdf_weibull(x, param):
  alpha = param[0]
  beta = param[1]
  return alpha/beta * (x/beta)**(alpha-1) * np.exp(-(x/beta)**alpha)

# Testando se a densidade integra em 1
round(inte.quad(lambda x, alpha, beta: pdf_weibull(x, param = [alpha, beta]),
          0, np.inf, args = (1,1))[0],2)

# Implementando uma função genérica que implementa a função objetivo
# (função de log-verossimilhança) que iremos maximizar. Essa função 
# irá receber como argumento uma função densidade de probabilidade.
# Não é preciso destrinchar (obter de forma exata) a função de 
# log-verossimilhança!
# A função de log-verossimilhança encontra-se multiplicada por -1
# devido ao fato da função que iremos fazer otimização minimizar 
# uma função função objetivo. Minimizar -f equivale a maximizar f.
# Lembre-se disso!
def log_likelihood(x, pdf, *args):
  return -np.sum(np.log(pdf(np.array(x), *args)))

# Gerando um conjunto de dados com alpha = 2.5 e beta = 1.5. Essa 
# é nossa distribuição verdadeira, i.e., é a distribuição que gera
# que gerou os dados que desejamos ajustar.
# Precisamos fixar uma semente, uma vez que queremos os mesmos dados
# toda vez que rodamos esse código. 
#np.random.seed(0)
#dados = random_weibull(n = n, alpha = alpha, beta = beta)

dados = pandas.read_csv("posts/likelihood_r_julia_python/dados.csv")["dados"]

# Miminimizando a função -1 * log_likelihood, i.e., maximizando
# a função log_likelihood.
alpha, beta = opt.minimize(
  fun = lambda *args: log_likelihood(dados, pdf_weibull, *args),
  x0=[0.5, 0.5]
).x

# Imprimindo os valores das estimativas de máxima verossimilhança
print("Valores estimados de alpha e beta\n")
print("--> alpha: ", alpha, "\n")
print("--> beta: ", beta, "\n")
