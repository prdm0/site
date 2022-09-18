using Distributions
using Random
using Optim
using QuadGK

# Quantidade de observações
n = 250;

# Parâmetros que especificam a distribuição verdadeira, i.e., 
# da fdp da v.a. cujos os dados são observações.
α = 2.5;
β = 1.5;

# Fixando um valor de semente
Random.seed!(0);

# Gerando um array de dados com distribuição Weibull(α, β)
dados = rand(Weibull(α,β), n);

# Função densidade de probaiblidade de uma v.a. 
# X ∼ Weibull(α, β)
function pdf_weibull(x, par = (α, β))
    α, β = par.α, par.β   
    @. α/β * (x/β)^(α-1) * exp(-(x/β)^α)
end;

# Checando se a integral no suporte de pdf_weibull integra em 1
area, error = quadgk(x -> pdf_weibull(x, (α = α, β = β)), 0, Inf);

# Escrevendo a função log_likelihood que em julia denotarei por
# ℓ, tendo em vista que podemos fazer uso de caracteres UTF-8 
# nessa linguagem. 

function ℓ(x, pdf, par...)
    -sum(log.(pdf(x, par...)))
end;

# Encontrando as estimativas de máxima verossimilhança usando a biblioteca Optim

emv = optimize(
        x -> ℓ(dados, pdf_weibull, (α = x[1], β = x[2])),
        [0.5, 0.5],
        LBFGS()
); 

emv_α, emv_β  = emv.minimizer;

# Imprimindo o resultado

print("Valores estimados para α e β\n")
print("--> α: ", emv_α, "\n")
print("--> β: ", emv_β, "\n")
