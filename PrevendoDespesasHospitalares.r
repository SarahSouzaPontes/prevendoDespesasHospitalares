# Machine Learning - Regressão 
getwd()

# Problema de Negócio: Previsão de Despesas Hospitalares
# 1.338 observações e 7 variáveis.

# Carregando Dataset
despesas <- read.csv("despesas.csv")

#Visualizando dados
View(despesas)

# Tipos de Variáveis
str(despesas)

#idade, sexo, fumantes regiao (...) variável independente.

# Resumo estatístico da variável gastos no banco despesas
summary(despesas$gastos)
#Media e Mediana distintas --> confirmar com as demais maneiras de visualizações de distribuição das variáveis
#TARGET = DEPENDENTE --> GASTOS --> previsao

# Construindo um histograma (analisando distribuicao da variável gastos)
hist(despesas$gastos, main = 'Histograma', xlab = 'Gastos')
#Variável não parece uma distribuição normal, não se dispões em curva de sino.

# Tabela de contingência das regiões
#Variáveis qualitativas
#Tipo Fator
table(despesas$regiao)
# Frequencia por região nordeste= 325    norte = 324  sudeste= 325     sul=364

# Explorando relacionamento entre as variáveis: Matriz de Correlação
cor(despesas[c("idade", "bmi", "filhos", "gastos")])
#Variáveis numéricas.Inserido em vetor, correlação. De -1 a +1, 0 sem correlação.
#Diagonal 1, pois se relaciona a variável com ela mesma.
#BMI X Idade 0.10 acima de 0.


# Nenhuma das correlações na matriz é considerada forte, mas existem algumas associações interessantes. 
# Por exemplo, a idade e o bmi (IMC) parecem ter uma correlação positiva fraca, o que significa que 
# com o aumento da idade, a massa corporal tende a aumentar. Há também uma correlação positiva 
# moderada entre a idade e os gastos, além do número de filhos e os gastos. Estas associações implicam 
# que, à media que idade, massa corporal e número de filhos aumenta, o custo esperado do seguro saúde sobe. 

#RESULTADO DA CORRELAÇÃO
#idade        bmi     filhos     gastos
#idade  1.0000000 0.10934101 0.04246900 0.29900819
#bmi    0.1093410 1.00000000 0.01264471 0.19857626
#filhos 0.0424690 0.01264471 1.00000000 0.06799823
#gastos 0.2990082 0.19857626 0.06799823 1.00000000

#SCARTERPLOT NA MESMA ÁREA DE PLOTAGEM
# Visualizando relacionamento entre as variáveis: Scatterplot
# Perceba que não existe um claro relacionamento entre as variáveis
pairs(despesas[c("idade", "bmi", "filhos", "gastos")])
#Não há correlação forte.

# Instalando e carregando pacote --> Scatterplot Matrix
install.packages("psych")
library(psych)

# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])

# Treinando o Modelo (usando os dados de treino)
#Modelo Linear --> Função LM
?lm
#Lado direito variávels preditoras e lado esquerdo do ~ variável preditora
modelo <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao, data = despesas)


# Similar ao item anterior
# MESMO COMANDO ACIMA, PORÉM P PONTO SIGNIFICA TODAS VAIRÁVEIS. REGRESSÃO MÚLTIPLA --> modelo <- lm(gastos ~ ., data = despesas)

# Visualizando os coeficientes
modelo


#RESULTADO DO MODELO--------------------------
#Call:
#  lm(formula = gastos ~ idade + filhos + bmi + sexo + fumante + 
#       regiao, data = despesas)

#Coefficients:
#  (Intercept)          idade         filhos  
#-12425.7          256.8          475.7  
#bmi     sexomulher     fumantesim  
#339.3          131.4        23847.5  
#regiaonorte  regiaosudeste      regiaosul  
#352.8         -606.5         -682.8  

#-------------------------------PREVISÃO C/ MODELO TREINADO
# Prevendo despesas médicas 
?predict

# Aqui verificamos os gastos previstos pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)

# Prevendo os gastos com Dados de teste APRESENTANDO OS DADOS DE TESTE
despesasteste <- read.csv("despesas-teste.csv")

#DADOS DE ENTRADA SEM DE SAÍDA, ASSIM NÃO HÁ COLUNA GASTOS
View(despesasteste)

previsao2 <- predict(modelo, despesasteste)
View(previsao2)

#DADOS HISTÓRICOS ENSINA E DADOS PARA PREVER TEM TAXA DE ERROS
#REDUZIR TAXA DE ERROS

# Avaliando a Performance do Modelo
summary(modelo)

#RESULTADO DO MODELO --- R COEFICINTE DE REGRESSAO
# Y A PREVISÃO --> COLUNA GASTOS
# A É O COEFICIENTE E O  B INCLINAÇÃO DA RETA E X REPRSENTA AS VARIÁVEIS PREDITORAS;
#Erro é a diferença entre a previsão.

#Intercept - reta de regressao corta o eixo y
# beta sao as variaveis preditoras;
# * é a significância, quanto mais, melhor;
# Erro padrao ideal que seja menor que o coeficiente
# valor t define o valor p e nível de significância;
#Valor p representa notação científica.
#última LINHA verificar relevância na construção do modelo;
# a ausência de *, intercepto, erro --> análise exploratória;
#Idade é relevante para o modelo;
#Grau de liberdade;
#R ao quadrado nível de significancia quanto maior melhor;
#Ideal de R = 1;
#Estatistica F;


#Call:
#  lm(formula = gastos ~ idade + filhos + bmi + sexo + fumante + 
#       regiao, data = despesas)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11302.7  -2850.9   -979.6   1383.9  29981.7 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -12425.7     1000.7 -12.418  < 2e-16 ***
 # idade            256.8       11.9  21.586  < 2e-16 ***
#  filhos           475.7      137.8   3.452 0.000574 ***
#  bmi              339.3       28.6  11.864  < 2e-16 ***
#  sexomulher       131.3      332.9   0.395 0.693255    
#fumantesim     23847.5      413.1  57.723  < 2e-16 ***
#  regiaonorte      352.8      476.3   0.741 0.458976    
#regiaosudeste   -606.5      477.2  -1.271 0.203940    
#regiaosul       -682.8      478.9  -1.426 0.154211    

#  Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 6062 on 1329 degrees of freedom
#Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7494 




# ****************************************************
# *** Estas informações abaixo é que farão de você ***
# *** um verdadeiro conhecedor de Machine Learning ***
# ****************************************************

# Equação de Regressão
# y = a + bx (simples)
# y = a + b0x0 + b1x1 (múltipla)

# Resíduos
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribuição normal, o que indica
# que a média entre os valores previstos e os valores observados é próximo de 0 (o que é bom)

# Coeficiente - Intercept - a (alfa)
# Valor de a na equação de regressão

# Coeficientes - Nomes das variáveis - b (beta)
# Valor de b na equação de regressão

# Obs: A questão é que lm() ou summary() têm diferentes convenções de 
# rotulagem para cada variável explicativa. 
# Em vez de escrever slope_1, slope_2, .... 
# Eles simplesmente usam o nome da variável em qualquer saída para 
# indicar quais coeficientes pertencem a qual variável.

# Erro Padrão
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal é que este valor 
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os níveis de significância de acordo com o p-value.
# Quanto mais estrelas, maior a significância.
# Atenção --> Muitos astericos indicam que é improvável que não exista 
# relacionamento entre as variáveis.

# Valor t
# Define se coeficiente da variável é significativo ou não para o modelo. 
# Ele é usado para calcular o p-value e os níveis de significância.

# p-value
# O p-value representa a probabilidade que a variável não seja relevante. 
# Deve ser o menor valor possível. 
# Se este valor for realmente pequeno, o R irá mostrar o valor 
# como notação científica

# Significância
# São aquelas legendas próximas as suas variáveis
# Espaço em branco - ruim
# Pontos - razoável
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standar Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento 
# e o número de variáveis no seu modelo

# R-squared (coeficiente de determinação - R^2)
# Ajuda a avaliar o nível de precisão do nosso modelo. 
# Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# É o teste F do modelo. Esse teste obtém os parâmetros do nosso modelo 
# e compara com um modelo que tenha menos parâmetros.
# Em teoria, um modelo com mais parâmetros tem um desempenho melhor. 

# Se o seu modelo com mais parâmetros NÃO tiver perfomance
# melhor que um modelo com menos parâmetros, o valor do p-value será bem alto. 

# Se o modelo com mais parâmetros tiver performance
# melhor que um modelo com menos parâmetros, o valor do p-value será mais baixo.

# Lembre-se que correlação não implica causalidade



# Otimizando a Performance do Modelo
#Verificar o dobro da idade;

# Adicionando uma variável com o dobro do valor das idades
despesas$idade2 <- despesas$idade ^ 2

# Adicionando um indicador para BMI >= 30
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0)

#Visualizando com as 2 variáveis;
View(despesas)

# Criando o modelo final
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                   bmi30 * fumante + regiao, data = despesas)

#Visualizar o modelo
summary(modelo_v2)

# Dados de teste
despesasteste <- read.csv("despesas-teste.csv")
View(despesasteste)
previsao <- predict(modelo, despesasteste)
class(previsao)
View(previsao)
#Melhorou o modelo: R-squared:  0.8664
