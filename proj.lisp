;;;    Projecto de Inteligencia Artificial     ;;;
;;;              1a Entrega                    ;;;
;;;               Grupo 54                     ;;;
;;;             Ana Galvao 75312               ;;;
;;;       Jose Diogo Oliveira 75255            ;;;
;;;             Andre Silva 75455              ;;;
(defconstant T-NLINHAS 18)  ; 0 a 17
(defconstant T-NCOLUNAS 10) ; 0 a 9

;;; cria-accao: inteiro x array -> accao
(defun cria-accao (pos_esq config)
    ;(setf accao (make-array 2))
    ;(setf (aref accao 0) pos_esq)
    ;(setf (aref accao 1) config)
    (cons pos_esq config)

    ;(make-accao :pos_esq pos :config config)
    ;(make-array 2 :initial-contents '(pos_esq config))
    ;(setq accao (pos_esq (append 'config accao))
)

;;; accao-coluna: accao-coluna: accao -> inteiro
(defun accao-coluna (accao)
    (car accao)
)

;;; accao-peca: accao -> array
(defun accao-peca(accao)
    (cdr accao)
)

;;; cria-tabuleiro {} -> tabuleiro
(defun cria-tabuleiro ()
    ; criar o tabuleiro
    ; devolve tabuleiro vazio
    (make-array (list T-NLINHAS T-NCOLUNAS) :initial-element nil)
)

;;; Aux function copy-arrays
(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;; copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro (tabuleiro)
    ;funcao para copiar o antigo tabuleiro para um novo

    ;(copy-array(tabuleiro))
    (make-array (array-total-size tabuleiro)
                :displaced-to tabuleiro
                :element-type (array-element-type tabuleiro))
)

;;; tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tabuleiro nlinha ncoluna)
    ; devolve true se tiver preenchida
    ; nil caso contrario
    (not (null (aref tabuleiro nlinha ncoluna)))
)

;;; tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tabuleiro ncoluna)
    ; devolve a posicao mais alta que esteja preenchida
    ; da coluna em questao
    ; devolve zero caso nao esteja preenchida
    ;METI altura iniciar a ZERO no let, para devolver 0 e nao nil quando a coluna nao tiver nenhuma peca

    (let ((altura 0))
        (dotimes (lin T-NLINHAS altura)  ; vou percorrer as linhas todas e returnar a altura
            (if (not (null (aref tabuleiro lin ncoluna)))  ; se a current altura nao for nil
                (setf altura (+ lin 1))  ; actualizo a var altura para a mais actual

            )
        )
    )
)

;;; tabuleiro-linha-completa-p: tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tabuleiro nlinha)
    ; devolve true se todas as posicoes da linha inteira
    ; estiverem preenchidas
    ; false, otherwise
    (dotimes (col T-NCOLUNAS t)
        (if (null (aref tabuleiro nlinha col)) (return nil))
    )
)

;;; tabuleiro-preenche!: tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tabuleiro nlinha ncoluna)
    ; altera o tabuleiro recebido na pos nlinha ncoluna
    ; para ficar preenchido
    ; valida de os valores da nlinha e ncoluna sao validos
    ; (se estao dentro dos limites do campo)
    ; nao interessa o valor devolvido (deve devolver nada)??
    ;(if (AND (AND (>= nlinha 0) (< nlinha T-NLINHAS)) (AND (>= ncoluna 0) (< ncoluna T-NCOLUNAS))) (setf (aref tabuleiro nlinha ncoluna) T) )

    (cond
        ((OR (< nlinha 0) (>= nlinha T-NLINHAS)) ()) ;(format t "Posicao invalida. Apenas [0, 17] linhas"))
        ((OR (< ncoluna 0) (>= ncoluna T-NCOLUNAS)) ()) ;(format t "Posicao invalida. Apenas [0, 9] colunas"))
        (t (setf (aref tabuleiro nlinha ncoluna) t))
    )
)

;;; tabuleiro-remove-linha!: tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tabuleiro nlinha)
    ; remove a nlinha do tabuleiro
    ; fazendo com que as linhas consecutivas, descam
    ; nao interessa o valor devolvido (deve devolver nada)??
    (let ((linha-de-cima nil))
    (dotimes (col T-NCOLUNAS t)
        (loop for lin from nlinha to (- T-NLINHAS 1)
           do (progn
                (cond
                    ((< lin (- T-NLINHAS 1)) (setf linha-de-cima (aref tabuleiro (+ lin 1) col)))
                    (t (setf linha-de-cima nil))
                )
                (setf (aref tabuleiro lin col) linha-de-cima)
            )
        )
    ))
)

;;; tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tabuleiro)
    ; devolve true se existir uma coluna preenchida
    ; na linha 17 do tabuleiro
    ;ALTEREI PARA DEVOLVER T QUANDO ENCONTRA POSICAO PREENCHIDA E DEVOLVER NIL CASO NAO ENCONTRE
    
    (dotimes (col T-NCOLUNAS nil)
        (if (not (null (aref tabuleiro (1- T-NLINHAS) col))) (return T))
    )
    
)

;;; tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
    ; devolve true se os 2 tabuleiros tiverem valores iguais
    (equalp tabuleiro1 tabuleiro2)
)

;;; tabuleiro->array: tabuleiro -> array
(defun tabuleiro->array (tabuleiro)
    ; recebe um tabuleiro e devolve um novo array com 18
    ; linhas e 10 colunas que em cada linha e coluna dever conter
    ; o valor logico
    ; o tabuleiro retornado e um novo objecto ( nao o mesmo que o tabuleiro)
    ; http://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
    (return-from tabuleiro->array tabuleiro)
)

;;; array->tabuleiro: array->tabuleiro
(defun array->tabuleiro (array)
    ; da a entrada do array com 18 linhas e 10 colunas
    ; devolve um tabuleiro object
    ; novo objecto (nao o mesmo que o array)
    ; http://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
    (return-from array->tabuleiro array)
)

;;; creates type Estado
(defstruct estado
    pontos              ; #num de pontos conseguidos ate ao momento
    pecas-por-colocar   ; lista de pecas ainda por colocar, com ordem de colocacao, letras da peca
    pecas-colocadas     ; lista de pecas ja colocadas, repr pelo simbolo, ordenada pela peca mais antiga
    tabuleiro           ; tabuleiro com as posicoes actualmente preechindas
)

;;; copia-estado: estado->estado
(defun copia-estado (estado)
    ; devolve novo estado cujo conteudo sera copiado do arg
    ; devolve novo objecto, nao destrutivo do antigo
    (make-estado
        :pontos (estado-pontos estado)  ;BUG: e preciso fazer uma copia deste valor
        :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado)) ;usar o copy-list para nao alterar o estado original
        :pecas-colocadas (copy-list (estado-pecas-colocadas estado))
        ;:tabuleiro (copia-tabuleiro (estado-tabuleiro estado))
        :tabuleiro (copy-array (estado-tabuleiro estado))   ;usar o copy-array para alterar estado inicial
    )
    ;(estado-copy estado)
)

;;; estados-iguais-p: estado x estado -> logico
(defun estados-iguais-p (estado1 estado2)
    ; devolve true se os 2 estados forem iguais
    (equalp estado1 estado2)
)

;;; estado-final-p: estado -> logico
(defun estado-final-p (estado)
    ; devolve true caso seja um estado final
    ; (jogador nao pode fazer mais jogadas) (pecas por colocar zerop)
    ; false caso contrario
    ; tiver atingido o topo ou nao tiver mais pecas por colocar
    (OR (zerop (length (estado-pecas-por-colocar estado)))  ; se nao tiver pecas por colocar
        (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))  ; se tiver o topo preenchido
)

;;;  creates type Problema
(defstruct problema
    estado-inicial  ; estado do problema de procura
    solucao         ; funcao recebe estado e devolve true se for a solucao, nil cc
    accoes          ; funcao recebe estado e devolve lista com acoes que sao possiveis para esse estado
    resultado       ; funcao que dado um estado devolve o estado sucessor  ?? ainda nao percebo ??
    custo-caminho   ; funcao que dado um estado devolve o custo do caminho desde o estado inicial
)

;;; solucao: estado -> logico
(defun solucao (estado)
    ; recebe um estado e devolve true se o estado
    ; recebido for solucao, nil otherwise
    ; solucao (true) se o topo nao tiver preenchido e se nao
    ; existirem pecas por colocar.
    ; (ter pontos nao interessa)
    (AND (zerop (length (estado-pecas-por-colocar estado)))  ;se nao tiver pecas por colocar
        (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))) ;se o topo nao estiver preenchido
)

;;; peca-i: {} -> lista de accoes
(defun peca-i ()
    ; devolve uma lista de accoes correspondentes a peca i
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (lista (list ()))
        (dotimes (n T-NCOLUNAS)
            (setf lista (append lista (list (cria-accao n peca-i0))))
        )
        (dotimes (n (- T-NCOLUNAS 3) (rest lista))
            (setf lista (append lista (list (cria-accao n peca-i1))))
        )
    )
)

;;; peca-l: {} -> lista de accoes
(defun peca-l ()
    ; devolve uma lista de accoes correspondentes a peca l
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em l0 e passar para l3, l2 e l1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para l3 -> l2 -> l1
    (let (lista (list ()))
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-l0))))
        )
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-l3))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-l2))))
        )
        (dotimes (n (- T-NCOLUNAS 2) (rest lista))
           (setf lista (append lista (list (cria-accao n peca-l1))))
        )
    )
)

;;; peca-j: {} -> lista de accoes
(defun peca-j ()
    ; devolve uma lista de accoes correspondentes a peca j
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em j0 e passar para j3, j2 e j1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para j3 -> j2 -> j1
    (let (lista '())
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-j0))))
        )
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-j3))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-j2))))
        )
        (dotimes (n (- T-NCOLUNAS 2) lista)
           (setf lista (append lista (list (cria-accao n peca-j1))))
        )
    )
)

;;; peca-o: {} -> lista de accoes
(defun peca-o ()
    ; devolve uma lista de accoes correspondentes a peca o
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (lista '())
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-o0))))
        )
    )
)

;;; peca-s: {} -> lista de accoes
(defun peca-s ()
    ; devolve uma lista de accoes correspondentes a peca s
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (lista '())
        (dotimes (n (- T-NCOLUNAS 2))
            (setf lista (append lista (list (cria-accao n peca-s0))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-s1))))
        )
    )
)

;;; peca-z: {} -> lista de accoes
(defun peca-z ()
    ; devolve uma lista de accoes correspondentes a peca z
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (lista '())
        (dotimes (n (- T-NCOLUNAS 2))
            (setf lista (append lista (list (cria-accao n peca-z0))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-z1))))
        )
    )
)

;;; peca-t: {} -> lista de accoes
(defun peca-t ()
    ; devolve uma lista de accoes correspondentes a peca t
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em t0 e passar para t3, t2 e t1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para t3 -> t2 -> t1
    (let (lista '())
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-t0))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-t3))))
        )
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-t2))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
           (setf lista (append lista (list (cria-accao n peca-t1))))
        )
    )
)

;;; accoes: estado -> lista de acoes
(defun accoes (estado)
    ; recebe estado devolve lista de accoes validas
    ; acao valida mesmo que faca o jogador perder
    ; acao invalida se nao for fisicamente possivel (< 0 > 10)
    ; !! ordem e importante frente na lista deve estar a order
    ; com que a peca deve estar virada, (orientacao)
    ; **** LER COM MAIS ATENCAO ****
    (cond
        ((eq (first (estado-pecas-por-colocar estado)) 'i) (peca-i))
        ((eq (first (estado-pecas-por-colocar estado)) 'l) (peca-l))
        ((eq (first (estado-pecas-por-colocar estado)) 'j) (peca-j))
        ((eq (first (estado-pecas-por-colocar estado)) 'o) (peca-o))
        ((eq (first (estado-pecas-por-colocar estado)) 's) (peca-s))
        ((eq (first (estado-pecas-por-colocar estado)) 'z) (peca-z))
        ((eq (first (estado-pecas-por-colocar estado)) 't) (peca-t))
        (T (nil))
    )
)

;;; dentro-limites: nlinha x ncoluna -> logico
(defun dentro-limites (nlinha ncoluna)
    ; recebe numero de linha e coluna 
    ; devolve True se posicao dentro dos limites
    ; limite das linhas [0, 17]
    ; limite das colunas [0, 9]
    (AND 
        (AND
            (>= nlinha 0)
            (< nlinha T-NLINHAS)
        )
        (AND
            (>= ncoluna 0)
            (< ncoluna T-NCOLUNAS)
        )
    )
)

;;; detecta-colisao: accao x tabuleiro -> logico
(defun detecta-colisao (accao tabuleiro nlinha)
    ; recebe accao, estado e numero da linha
    ; devolve
    ; true caso a peca esteja a coincidir com alguma posicao do tabuleiro(True)
    ; false caso a peca nao coincida com nenhuma posicao preenchida do tabuleiro
    (let (
        (numlinhaspeca (first (array-dimensions (cdr accao)))) 
        (numcolunaspeca (last (array-dimensions (cdr accao))))
        )
        
        (dotimes (l numlinhaspeca)
            (dotimes (c numcolunaspeca)
                (lambda
                (if (dentro-limites (+ nlinha l) (+ ncoluna c)) ( ;verifica se a posicao que se ira comparar esta dentro dos limites do tabuleiro
                    ;verifica se alguma posicao da peca (a True) coincide com alguma posicao do tabuleiro (a True)
                    (if (AND (tabuleiro-preenchido-p (cdr accao) l c) (tabuleiro-preenchido-p tabuleiro (+ nlinha l) (+ (car accao) c))) T))
                )
                )
            )
        )
    )
)

;;; insere-peca: peca x tabuleiro x nlinha x ncoluna -> {}
(defun insere-peca (peca tabuleiro nlinha ncoluna)
    ; recebe linha e coluna a partir das quais se insere a peca no tabuleiro
    ; nao devolve nada
    (let (
        (numlinhaspeca (first (array-dimensions (cdr accao)))) ;numero de linhas da peca
        (numcolunaspeca (last (array-dimensions (cdr accao)))) ;numero de colunas da peca
        )
        
        (dotimes (l numlinhaspeca)
            (dotimes (c numcolunaspeca)
                (lambda
                (if (tabuleiro-preenchido-p peca l c) (    ;verifica se a posicao na peca e true (se for nil nao faz sentido inserir)
                    (if (dentro-limites (+ nlinha l) (+ ncoluna c)) ( ;validar se a nova posicao a inserir no tabuleiro esta dentro dos limites
                        tabuleiro-preenche! tabuleiro (+ nlinha l) (+ ncoluna c)))) ;ibserir nova posicao no tabuleiro
                )
                )
            )
        )
    )
)

;;; resultado: estado x accao -> estado
(defun resultado (estado accao)
    ; recebe estado e acao e devolve o novo estado que
    ; resultda de aplica a acao ao estado original
    ; NAO e destrutivo, ou seja, novo obejcto e gerado
    ; pseudo algo:
    ; deve actualizar as listas de pecas,
    ; colocar a peca especificada pela accao na posicao correcta
    ; depois de colocada a peca,
    ; verifica se o topo do tabuleiro esta preenchido;
    ; caso sim: nao se removem linhas e devolve-se o estado
    ; case no: removem-se as linhas e calculam-se os pontos obtidos
    (let (
        (new (copia-estado estado))   
        (numlinhaspeca (first (array-dimensions (cdr accao)))) 
        (numcolunaspeca (last (array-dimensions (cdr accao)))) 
        (colunamaior 0)
        (nlinhasremovidas 0)
        )

        ;CICLO DESCOBRIR COLUNA MAIOR DO TABULEIRO (ONDE A PECA PUDERA COLIDIR)
        (dotimes (c numcolunaspeca)
            (if (> (tabuleiro-altura-coluna (estado-tabuleiro new) c) colunamaior) (
                setf colunamaior (tabuleiro-altura-coluna (estado-tabuleiro new) c)))
        )

        ;CICLO DE DECREMENTO DAS POSICOES DA PECA NA TABELA ATE COLISAO
        (loop for lin from colunamaior downto 0
            do (
                (lambda             
                (if (detecta-colisao accao (estado-tabuleiro new) lin) ( ;detecta se houve colisao da peca com o tabuleiro
                    insere-peca (cdr accao) (estado-tabuleiro new) (1- lin) (car accao))) ;caso existe colisao, metemos a peca na LINHA ANTERIOR
                )
            )
        )

        ;ACTUALIZAR A LISTA DE PECAS COLOCADAS PELA LISTA DE PECAS POR COLOCAR
        (setf (estado-pecas-colocadas new) (append (estado-pecas-colocadas new) (list (first (estado-pecas-por-colocar new)))))

        ;ACTUALIZAR A LISTA DE PECAS POR COLOCAR (RETIRANDO O PRIMEIRO ELEMENTO DA LISTA)
        (setf (estado-pecas-por-colocar new) (rest (estado-pecas-por-colocar new)))

        ;VERIFICA SE ACABOU O JOGO (TOPO PREENCHIDO)
        (if (tabuleiro-topo-preenchido-p (estado-tabuleiro new)) 
            ;Se true:
            (return new) ;devolve o estado
            ;Se nil:
            (dotimes (l T-NLINHAS)
                (lambda
                (if (tabuleiro-linha-completa-p (estado-tabuleiro new) l) (
                (tabuleiro-remove-linha! l) ;remover as linhas (BUG: nao e bug mas podemos arranjar forma de nao procurar o tabuleiro todo por uma linha preenchida)
                (setf nlinhasremovidas (1+ nlinhasremovidas)))) ; incrementa contador de linhas removidas
                )
            )
            (cond  ;atribuicao da respectiva pontuacao consoante o numero de linhas removidas
                ((eq nlinhasremovidas 1) (setf (estado-pontos new) (+ (estado-pontos new) 100)))
                ((eq nlinhasremovidas 2) (setf (estado-pontos new) (+ (estado-pontos new) 300)))
                ((eq nlinhasremovidas 3) (setf (estado-pontos new) (+ (estado-pontos new) 500)))
                ((eq nlinhasremovidas 4) (setf (estado-pontos new) (+ (estado-pontos new) 800)))
            )
        )




       ;(setf (estado-pontos new) 10)
    )
    ;(declare (ignore estado accao))
)

;;; qualidade: estado -> inteiro
(defun qualidade (estado)
    ; recebe estado e devolve inteiro que corresponde
    ; ao valor de pontos ganhos ate ao momento em valor negativo.
    ; **** LER COM MAIS ATENCAO ****
    (* -1 (estado-pontos estado))
)

;;; custo-oportunidade: estado -> inteiro
(defun custo-oportunidade (estado)
    ; recebe estado e devolve inteiro que corresponde
    ; nao percebo
    ; **** LER COM MAIS ATENCAO ****
    (declare (ignore estado))
)


#|
Algoritmos de Procura (2' parte do projecto)
|#
;;; procura-pp: problema -> lista de acoes
(defun procura-pp (problema)
    ; usa procura em profundidade primeiro em arvore
    ; para obter solucao para o problema
    ; devolve lsita de acoes que se executa pela ordem especifica
    ; leva de um estado inicial ao objectivo
    ; deve utilizar LIFO (last in first out)
    ; ultimo n a ser colocado na fronteira dever ser o primeiro a ser explorado.
    ; generico.. nao so para o tetris mas para qualquer problema
    (declare (ignore problema))
)

;;; procura-A*: problema x heuristica -> lista de acoes
(defun procura-A* (problema heuristica)
    ; usa o algo procura A* em arvore para determinar a seq de acoes
    ; de modo a maximizar os pontos obtidos
    ; a funcao euristica corresponde a uma func que recebe um estado
    ; e devolve um numero que corresponde a uma estimativa do custo/qualidade
    ; apartir desse estado ate ao melhor objectivo
    ; em caso de empate entre dois nos com igual valor de f
    ; deve ser escolhido o ultimo a ser colocado
    ; generico.. nao so para o tetris mas para qualuer problema
    (declare (ignore problema heuristica))
)

;;; procura-best: array x listapecas -> lista de acoes
(defun procura-best (array lista-pecas)
    ;;;;;; este e o avaliado
    (declare (ignore array lista-pecas))
)

;(load "utils.fas")
(load (compile-file "utils.lisp"))
