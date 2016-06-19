#lang racket

;; Este programa encontra horários disponíveis que sejam comuns entre vários
;; horários especificados e que tenham um tamanho mínimo especificado.
;;
;; ** Conceitos **
;;  Horário
;;    Um momento no tempo, definido em termos da hora e minutos
;;  Intervalo (abreviado inter)
;;    Um intervalo no tempo, tem um horário de início e um horário de fim
;;  Disponibilidade do dia (abreviado dispo)
;;    Uma lista de intervalos que estão disponíveis em um determinado dia
;;  Disponibilidade semanal (abreviado dispo-semana)
;;    Uma lista com as disponibilidades de cada dia
;;  Lista de associações
;;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;;    elemento do par é chamado de chave e o segundo elemento é chamado de
;;    valor. Uma lista de associações é uma maneira simples de implementar uma
;;    tabela associativa (dicionário).  Ex: o dicionário
;;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado pela lista associativa
;;    (list (list 1 4) (list 20 12) (list 6 70)).
;;    A função assoc é utilizada para consultar uma lista associativa.
;;
;; ** Formatação de entrada e saída **
;; Toda operação de entrada e saída deve ser feita respeitando essas
;; formatações. A sua implementação não precisa validar as entradas. Para os
;; testes automatizados as entradas sempre serão válidas.
;;
;;  Horário (HH:MM) (sempre 5 dígitos)
;;  Exemplos
;;     08:30 =  8 horas e 30 minutos
;;     12:07 = 12 horas e  7 minutos
;;
;;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;;  Exemplos
;;     08:30-12:07 = o intervalo tem início às 8 horas e 30 minutos e tem
;;                   o fim às 12 horas e 7 minutos
;;
;;  Dias da semana
;;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;;
;;  Disponibilidade semanal
;;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;;    intervalos disponíveis naquele dia
;;  Exemplo
;;    ter 10:20-12:00 16:10-17:30
;;    sex 08:30-11:30
;;  Observe que nem todos os dias devem estar especificados. Os dias
;;  que não têm disponibilidades não devem ser especificados.

;; exporta as funções que podem ser utilizadas em outros arquivos
(provide horario
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

(struct horario (h m) #:transparent)
;; Horário representa um momento no tempo, definido em termos da hora e minutos
;;    h : Número - horas
;;    m : Número - minutos

(struct intervalo (inicio fim) #:transparent)
;; Intervalo representa um intervalo no tempo, tem um horário de início e um
;; horário de fim
;;    inicio : Horário - horário de início
;;       fim : Horário - horário de fim

;; Constante que define um intervalo vazio
(define intervalo-vazio (void))

;; Constante que define os dias da semana
(define dias-semana  '("seg" "ter" "qua" "qui" "sex" "sab" "dom"))

;; Intervalo -> bool
;; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

;; Horario -> Inteiro
;; Converte um horario para minutos.
(define (horario-em-minutos hora)
  (let ([minutos-hora (* (horario-h hora) 60)])
    (+ minutos-hora (horario-m hora))
  )
)

;; Horario, Horario -> Horario
;; Retorna o maior Horario entre dois
(define (maior-horario horario-a horario-b)
  (retorna-horario horario-a horario-b >)
)

;; Horario, Horario -> Horario
;; Retorna o menor Horario entre dois
(define (menor-horario horario-a horario-b)
  (retorna-horario horario-a horario-b <)
)

;; Horario, Horario, Operador -> Horario
;; Retorna o Horario, entre dois, que satisfaça o operador ('>' ou '<')
(define (retorna-horario horario-a horario-b operador)
  (let ([minutos-horario-a (horario-em-minutos horario-a)]
        [minutos-horario-b (horario-em-minutos horario-b)])
    (cond
      [(operador minutos-horario-a minutos-horario-b) horario-a]
      [else horario-b]    
    )                       
  )
)

;; Horario, Horario -> Boolean
;; Verifica se um intervalo de tempo é válido, e devolve #t ou #f. Por exemplo, o intervalo tem inicio e fim,
;; no caso de o fim ser menor que o inicio a função deve retornar #f.
(define (intervalo-valido? inicio fim)
  (let ([minutos-inicio (horario-em-minutos inicio)]
        [minutos-fim    (horario-em-minutos fim)])
    (not (< minutos-fim minutos-inicio))
  )
)

;; Intervalo -> Inteiro
;; Encontra o tempo, em minutos, de um intervalo.
(define (intervalo-em-minutos inter)
  (let ([minutos-inicio (horario-em-minutos (intervalo-inicio inter))]
        [minutos-fim    (horario-em-minutos (intervalo-fim    inter))])
    (- minutos-fim minutos-inicio)
  )
)

;; Intervalo, Inteiro -> Boolean
;; Verifica se o tempo comparado é menor que o intervalo.
(define (tempo-intervalo-valido? inter tempo)
  (let ([minutos-intervalo (intervalo-em-minutos inter)]
        [minutos-tempo     (horario-em-minutos tempo)])
   (not (> minutos-tempo minutos-intervalo))
  )
)

;; String -> Horario
;; Converte um valor, referente a um horário, na estrutura Horario
(define (string-em-horario horario-string)
  (let([hora    (string->number (substring horario-string 0 2))]
       [minutos (string->number (substring horario-string 3 5))])
  (horario hora minutos)
  )
)

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (cond
    [(not (intervalo-valido? (intervalo-inicio b) (intervalo-fim a))) intervalo-vazio]
    [(not (intervalo-valido? (intervalo-inicio a) (intervalo-fim b))) intervalo-vazio]
    [(let ([inicial (maior-horario (intervalo-inicio a) (intervalo-inicio b))]
           [final (menor-horario (intervalo-fim a) (intervalo-fim b))])
     (intervalo-valido? inicial final)
        (intervalo inicial final))]
    [else intervalo-vazio]
  )
)

;; Intervalo, Lista de Intervalos -> Lista de Intervalos
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao-lista intervalo-a lista-b)
  (cond
    [(empty? lista-b) empty]
    [else
     (let ([inter (intervalo-intersecao intervalo-a (first lista-b))])
       (if (intervalo-vazio? inter)
           (intervalo-intersecao-lista intervalo-a (rest lista-b))
           (cons inter (intervalo-intersecao-lista intervalo-a (rest lista-b)))))]))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (cond
    [(or (empty? dispo-a) (empty? dispo-b)) empty]
    [else (append (intervalo-intersecao-lista (first dispo-a) dispo-b) (encontrar-dispo-em-comum (rest dispo-a) dispo-b))]
  )
)

;; Lista Intervalos, String -> Boolean
;; Retorna se a lista possui disponibilidade na semana.
(define (disponibilidade-dia? dispos dia)
  (assoc dia dispos)
)

;; Lista Intervalos, Lista Intervalos, String -> Boolean
;; Retorna se as listas possuem disponibilidade na semana.
(define (disponibilidade-dispos-dia? dispo-a dispo-b dia)
  (and (disponibilidade-dia? dispo-a dia) (disponibilidade-dia? dispo-b dia))
)

;; Lista Intervalos, Lista Intervalos, String, Inteiro -> Par dia e Intervalos
;; Retorna o par de disponibilidades entre duas listas, no dia e de acordo com o tempo estabelecido
(define (disponibilidade-por-dia-e-tempo dispo-a dispo-b dia-semana tempo)
  (if (and (disponibilidade-dispos-dia? dispo-a dispo-b dia-semana))
      (let ([dispos (encontrar-dispo-em-comum (first (cdr (assoc dia-semana dispo-a)))
                                              (first (cdr (assoc dia-semana dispo-b))))])
        (let ([disponibilidades (filter (λ (inter) (tempo-intervalo-valido? inter tempo)) dispos)])
          (if (not (null? disponibilidades))
              (list dia-semana disponibilidades)
              empty)
        )
      )
      empty
  )
)

;;Lista Dispos, Lista Dispos, Horario -> disponiilidade-por-dia-e-tempo
;;Mapeia as disponibilidades dos dias da semana por dia e tempo
(define (mapeia-dias-semana dispo-a dispo-b tempo)
  (map (λ (dia) (disponibilidade-por-dia-e-tempo dispo-a dispo-b dia tempo)) dias-semana)
)

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
;;
;; dispo-semana é uma lista de associações entre um dia (string) e a
;; disponibilidade naquele dia. Veja a definição de lista de associações no
;; início deste arquivo.
;;
;; Por exemplo, a disponibilidade semanal (dispo-semana):
;; ter 10:20-12:00 16:10-17:30
;; sex 08:30-11:30
;; é representada da seguinte maneira:
;; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;;                         (intervalo (hora 16 10) (hora 17 30))))
;;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;;
;; Observe que esta função recebe como parâmetro uma lista de disponibilidades
;; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
;; Veja os testes de unidade para exemplos de entrada e saída desta função

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra a disponibilidade semanal que duas pessoas teriam caso quisessem marcar um reunião de X minutos.
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (define (filtra-dispos dispo-a dispo-b)
    (filter pair? (mapeia-dias-semana dispo-a dispo-b tempo))
  )
  
  (filter pair? (foldr filtra-dispos (first dispos)
                                     (cdr   dispos))
  )
)

;; String -> intervalo-string
;; Converte as informações de horário lidas do arquivo na forma de String e transforma em intervalo.
(define (string-em-intervalo intervalo-string)
  (let ([string-inicio (substring intervalo-string 0 5)]
        [string-fim    (substring intervalo-string 6 11)])
    (let ([horario-inicio (string-em-horario string-inicio)]
          [horario-fim    (string-em-horario string-fim)])
      (intervalo horario-inicio horario-fim)
    )
  )
)

;; Linha de String -> Intervalo
;; Mapeia apenas uma linha do arquivo de string para intervalo.
(define (linha-em-intervalos linha)
  (map string-em-intervalo (cdr linha))
)

;; String -> dispo-a ou dispo-b ou dispo-c 
;; Controi uma lista de disponibilidade a partir de cada linha do arquivo.
(define (string-em-dispo linha)
  (cons (first linha) (list (linha-em-intervalos linha)))
)

;; Arquivo -> Linha
;; Divide as informações do arquivo por linha.
(define (arquivos-em-linhas arquivo)
  (map string-split (file->lines arquivo))
)

;; Arquivo -> lista-dispos
;; Transforma as informações do arquivo em uma lista de disponibilidade.
(define (arquivo-em-lista-dispos arquivo)
  (map string-em-dispo (arquivos-em-linhas arquivo))
)

;; Lista de arquivos -> Lista de Dispos
;; Mapeia de todos os arquivos de listas para uma lista de dispos, por exemplo: list '(dispo-a, dispo-b).
(define (mapeia-dispos-arquivos lista-de-arquivos)
  (map arquivo-em-lista-dispos lista-de-arquivos)
)

;; Inteiro -> String 
;; Faz a conversão de um inteiro para String no formato correto, com o valor tendo sempre 2 dígitos.
(define (inteiro-em-string-formato valor)
  (let ([valor-string (number->string valor)])
    (cond
      [(< (string-length valor-string) 2) (string-append "0" valor-string)]
      [else valor-string]
    )
  )
)

;; Horario -> String
;; Transforma o horário em uma String no formato HH:MM.
(define (horario-em-string hora)
  (string-append (inteiro-em-string-formato (horario-h hora)) ":" (inteiro-em-string-formato (horario-m hora)))
)

;; Intervalos -> Intervalos
;; Imprime na tela os intervalos.
(define (imprime-intervalos intervalos)
  (cond
    [(empty? intervalos) (display "\n")]
    [(display (string-append " " (horario-em-string (intervalo-inicio (first intervalos))) "-" (horario-em-string (intervalo-fim (first intervalos))))) (imprime-intervalos (rest intervalos))]
  )
)

;; Lista de Dispos -> Dispo
;; Imprime a disponibiliade na tela.
(define (imprime-dispos dispo)
  (display (first dispo))
  (map imprime-intervalos (cdr dispo))
)

;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
;; devem ter. O tempo mínimo é especificado usando a formatação de horário.
;;
;; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
;; contêm uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
;; testes.
;;
;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.

;; list string -> void
;; Função main recebe o tempo que a pessoa deseja que a reunião tenha e as listas com as disponibilidades de todos que devem comparecer na reunião.
;; Faz a verificação se todos os envolvidos possuem disponibilidade em comum e devolve os dias e horários possiveis para se marcar a reunião.
(define (main args)
  (let ([tempo (string-em-horario (first args))]
        [lista-de-arquivos (cdr args)])
    (let ([disponibilidades (mapeia-dispos-arquivos lista-de-arquivos)])
      (map imprime-dispos (encontrar-dispo-semana-em-comum tempo disponibilidades))
    )
  )
)
