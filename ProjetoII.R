#Projeto II
#Grupo:
# Andre Luis Vasconcelos (alpvj)
# Joao Vitor Valadares (jvvm)
# Marcos Vinicius Phryston (mvpn)

# Questao 1:
  got = read.csv("GOTFinal.csv", header = TRUE)
  
  print(got)

# Questao 2:
  notas = got[, 3];
  #Pegando a media
    mediaNotas = mean(notas)
  #Pegando o desvio padrao
    desvio_padraoNotas = sd(notas)
  #Pegando a moda
    notas = sort(notas);
    resposta = c(0,0);
    auxNota = 0;
    auxQtd = 0;
    for (x in notas){
      if (auxNota != x){
          if (resposta[2] < auxQtd){
            resposta[1] = auxNota;
            resposta[2] = auxQtd;
          }
          auxNota = x;
          auxQtd = 1;
      }else{
        auxQtd = auxQtd + 1;
      }
    }
    modaNotas = resposta[1]
  #Printando a resposta
    print(sprintf("A media das notas e: %f", mediaNotas))
    print(sprintf("O desvio padrao das notas e: %f", desvio_padraoNotas))
    print(sprintf("A moda das notas e: %.1f", modaNotas))
    
#Questao 3:
  audiencia = got[, 5];
  #Pegando a media
    mediaAud = mean(audiencia)
  #Pegando o desvio padrao
    desvio_padraoAud = sd(audiencia)
  #Pegando a mediana
    medianaAud = median(audiencia)
  #Printando a resposta
    print(sprintf("A media das audiencias e: %f", mediaAud))
    print(sprintf("O desvio padrao das audiencias e: %f", desvio_padraoAud))
    print(sprintf("A mediana das audiencias e: %.2f", medianaAud))

#Questao 4:
      episodios = got[, 2]
      #Criando a funcao que printa os episodios com nota >= 9
      funcaoPrint = function(){
        index_x = 0 #um auxiliar para poder pegar o index de x em notas para poder printar
        for (x in notas){
          index_x = index_x + 1
          if (x >= 9)
            print(factor(episodios[index_x]))
        }
      }
      funcaoPrint()
#Questao 5:
      