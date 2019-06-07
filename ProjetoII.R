#Projeto II
#Grupo:
# Andre Luis Vasconcelos (alpvj)
# Joao Vitor Valadares (jvvm)
# Marcos Vinicius Phryston (mvpn)

# Questao 1:
  got = read.csv("GOTFinal.csv", header = TRUE)#getwd()
  
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
            print(as.character(episodios[index_x]))
        }
      }
      funcaoPrint()
      
#Questao 5:
      #Funcao que pega o maior e o menor episodio de uma certa temporada (de acordo com a nota)
      funcaoEpiPorTemp = function(){
        TITULO = c()
        NOTA = c()
        TEMPORADA = c()
        for (temporada_atual in 1:8){
          indiceInit = 0
          indiceFinal = 0
          indexAtual = 0
          jaAchou = FALSE
          for (x in got$Temporada){
            indexAtual = indexAtual + 1
            if (x == temporada_atual)
              if (jaAchou == FALSE){
                indiceInit = indexAtual
                jaAchou = TRUE
              }
              else
                indiceFinal = indexAtual
          }
          arrayEp = episodios[indiceInit:indiceFinal]
          arrayNotas = got[, 3]
          arrayNotas = arrayNotas[indiceInit:indiceFinal]
          #BubbleSort
            indiceX = 0
            indiceJ = 0
            for (i in arrayNotas){
              indiceX = indiceX + 1
              indiceJ = 0
              for (j in arrayNotas){
                indiceJ = indiceJ + 1
                
                if (arrayNotas[indiceX] < arrayNotas[indiceJ]){
                  aux = arrayNotas[indiceX]
                  arrayNotas[indiceX] = arrayNotas[indiceJ]
                  arrayNotas[indiceJ] = aux
                  aux2 = arrayEp[indiceX]
                  arrayEp[indiceX] = arrayEp[indiceJ]
                  arrayEp[indiceJ] = aux2
                }
              }
            }
          #Adicionando nos arrays para criar o dataframe
            TITULO[(temporada_atual*2)-1] = as.character(arrayEp[1])
            TITULO[(temporada_atual*2)] = as.character(arrayEp[length(arrayEp)])
            
            NOTA[(temporada_atual*2)-1] = arrayNotas[1]
            NOTA[(temporada_atual*2)] = arrayNotas[length(arrayNotas)]
            
            TEMPORADA[(temporada_atual*2)-1] = temporada_atual
            TEMPORADA[(temporada_atual*2)] = temporada_atual
        
        }
        
        df = data.frame(TITULO, NOTA, TEMPORADA)
        print(df)
      }
      
      funcaoEpiPorTemp()
      
#Questao 6:
      menorDP = function(){
        arrayDosDP = c()
        for (temporada_atual in 1:8){
          indiceInit = 0
          indiceFinal = 0
          indexAtual = 0
          jaAchou = FALSE
          for (x in got$Temporada){
            indexAtual = indexAtual + 1
            if (x == temporada_atual)
              if (jaAchou == FALSE){
                indiceInit = indexAtual
                jaAchou = TRUE
              }
            else
              indiceFinal = indexAtual
          }
          arrayAudiencia = got[, 5]
          arrayAudiencia = arrayAudiencia[indiceInit:indiceFinal]
          desvPad = sd(arrayAudiencia)
          arrayDosDP[temporada_atual] = desvPad
        }
        melhorTemp = 0
        indexAtual = 0
        menorDP = Inf
        for(x in arrayDosDP){
          indexAtual = indexAtual + 1
          if (x < menorDP){
            menorDP = x
            melhorTemp = indexAtual
          }
        }
        return (melhorTemp)
      }
      print(sprintf("A temporada com menor desvio padrao e: %d", menorDP()))
      
#Questao 7:
      arrayPers = got[, 4]
      arrayNotas = got[, 3]
      contador = 0
      somaNotas = 0
      indexAtual = 0
      for (x in arrayPers){
        indexAtual = indexAtual + 1
        if (grepl("Brienne of Tarth", as.character(x))){
          contador = contador + 1 
          somaNotas = somaNotas + arrayNotas[indexAtual]
        }
      }
      mediaBrienne = somaNotas/contador
      print(sprintf("A media das notas dos episodios em que Brienne of Tarth aparece e: %.1f",mediaBrienne))

#Questao 8:
      funcaoDeRepetidos4Temp  = function(){
        stringComTodoMundo = ""
        for (x in 31:40){
          stringComTodoMundo = paste0(stringComTodoMundo, as.character(arrayPers[x]))
          stringComTodoMundo = paste0(stringComTodoMundo, ",")
        }
        arrayTodosOsPersonagens = unlist(strsplit(stringComTodoMundo, ","))
        arrayPersonagens4 = arrayTodosOsPersonagens
        arrayTodosOsPersonagens = unique(arrayTodosOsPersonagens)
  
        arrayRepetido = c()
        for (x in 1:41){
          arrayRepetido[x] = 0
        }
        indexAtual = 0
        for (x in arrayTodosOsPersonagens){
          indexAtual = indexAtual + 1
          for (y in arrayPersonagens4){
            if (x == y){
              arrayRepetido[indexAtual] = arrayRepetido[indexAtual] + 1
            }
          }
        }
        arrayDoRetorno = c()
        contador = 0
        indexAtual = 0
        for (x in arrayRepetido){
          indexAtual = indexAtual + 1
          if (x == 1){
            contador = contador + 1
            arrayDoRetorno[contador] = arrayTodosOsPersonagens[indexAtual]
          }
        }
        return (arrayDoRetorno)
      }
      print(funcaoDeRepetidos4Temp())
#Questao 9:
      histogramaPersonagem = function(personagem){
        aparicoesPorTemp = c(0, 0, 0, 0, 0, 0, 0, 0)
        indexAtual = 0
        for (x in got$Temporada){
          indexAtual = indexAtual +1
          if (grepl(personagem, as.character(got$Personagens[indexAtual]))){
            aparicoesPorTemp[x] = aparicoesPorTemp[x] + 1
          }
        }
        return (aparicoesPorTemp)
      }
      nome = "Bran Stark"
      #nome <- readline(prompt = "Digite o nome do personagem")
      arrayParaHisto = histogramaPersonagem(nome)
      print(arrayParaHisto)
      novoArrayFreq = c()
      indexAtual = 0
      contador = 0
      for (x in arrayParaHisto){
        contador = contador +1
        if (x > 0){
          for (y in 1:x){
            indexAtual = indexAtual +1
            novoArrayFreq[indexAtual] = contador
          }
        }
      }
      hist(novoArrayFreq,
           main = nome,
           ylab = "Ocorrencia",
           xlab = "Temporada",
           border = "black",
           col = "blue",
           breaks = c(0,1,2,3,4,5,6,7,8),
           xlim = c(0, 8),
           ylim = c(0,max(arrayParaHisto))
          )