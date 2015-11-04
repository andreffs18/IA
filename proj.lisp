;;;    Projecto de Inteligencia Artificial     ;;;
;;;              1a Entrega                    ;;;
;;;               Grupo 54                     ;;;
;;;          	Ana Galvao 75312	           ;;;
;;;       Jose Diogo Oliveira 75255            ;;;
;;;    			Andre Silva 75455		       ;;;
(defconstant T-NLINHAS 18)  ; 0 a 17
(defconstant T-NCOLUNAS 10) ; 0 a 9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                                                                                   ;;;;;;;;;;;
;;;;;;;;;;;;                                  ACCOES                                        ;;;;;;;;;;;
;;;;;;;;;;;;                                                                                   ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cria-accao: inteiro x array -> accao
(defun cria-accao (pos_esq config)
	(cons pos_esq config)
)

;;; accao-coluna: accao-coluna: accao -> inteiro
(defun accao-coluna (accao)
	(car accao)
)

;;; accao-peca: accao -> array
(defun accao-peca(accao)
	(cdr accao)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                                                                                   ;;;;;;;;;;;
;;;;;;;;;;;;                                  TABULEIRO                                        ;;;;;;;;;;;
;;;;;;;;;;;;                                                                                   ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cria-tabuleiro {} -> tabuleiro
(defun cria-tabuleiro ()
    ; criar o tabuleiro
    ; devolve tabuleiro vazio
    (make-array (list T-NLINHAS T-NCOLUNAS) :initial-element nil)
)

;;; copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro (tabuleiro)
	(make-tabuleiro :linhas (tabuleiro-linhas t1)
                    :colunas (tabuleiro-colunas t1)
    (declare (ignore tabuleiro))
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
    (let ((altura nil))
        (dotimes (lin T-NLINHAS altura)
            (if (not (null (aref tabuleiro lin ncoluna)))
                (setf altura (+ lin 1))
            )
        )
    )
)

;;; tabuleiro-linha-completa-p: tabuleiro x inteiro -> lógico
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
    (cond
        ((OR (< nlinha 0) (>= nlinha T-NLINHAS)) (format t "Posicao invalida. Apenas [0, 17] linhas"))
        ((OR (< ncoluna 0) (>= ncoluna T-NCOLUNAS)) (format t "Posicao invalida. Apenas [0, 9] colunas"))
        (t (setf (aref tabuleiro nlinha ncoluna) t))
    )
)

;;; tabuleiro-remove-linha!: tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tabuleiro nlinha)
    ; remove a nlinha do tabuleiro
    ; fazendo com que as linhas consecutivas, descam
    ; nao interessa o valor devolvido (deve devolver nada)??
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
    )
)

;;; tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tabuleiro)
    ; devolve true se existir uma coluna preenchida
    ; na linha 17 do tabuleiro
    (dotimes (col T-NCOLUNAS t)
        (if (null (aref tabuleiro (1- T-NLINHAS) col)) (return nil))
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
    ; linhas e 10 colunas que em cada linha e coluna deverá conter
    ; o valor logico
    ; o tabuleiro retornado e um novo objecto ( nao o mesmo que o tabuleiro)
    ; http://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
    (declare (ignore tabuleiro))
)

;;; array->tabuleiro: array->tabuleiro
(defun array->tabuleiro (array)
    ; da a entrada do array com 18 linhas e 10 colunas
    ; devolve um tabuleiro object
    ; novo objecto (nao o mesmo que o array)
    ; http://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
    (declare (ignore array))

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
    (declare (ignore estado))
)

;;; estados-iguais-p: estado x estado -> logico
(defun estados-iguais-p (estado1 estado2)
    ; devolve true se os 2 estados forem iguais
    (declare (ignore estado1 estado2))
)

;;; estado-final-p: estado -> logico
(defun estado-final-p (estado)
    ; devolve true caso seja um estado final
    ; (jogador nao pode fazer mais jogadas)
    ; false caso contrario
    ; tiver atingido o topo ou nao tiver mais pecas por colocar
    (declare (ignore estado))
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
    (declare (ignore estado))
)

;;; accoes: estado -> lista de acoes
(defun accoes (estado)
    ; recebe estado devove lista de accoes validas
    ; acao valida mesmo que faca o jogador perder
    ; acao invalida se nao for fisicamente possivel (< 0 > 10)
    ; !! ordem e importante frente na lista deve estar a order
    ; com que a peca deve estar virada, (orientacao)
    ; **** LER COM MAIS ATENCAO ****
    (declare (ignore estado))
)

;;; resultado: estado x accao -> estado
(defun resultado (estado accao)
    ; recebe estado e acao e devolve o novo estado que
    ; resultda de aplica a cao ao estado original
    ; NAO e destrutivo, ou seja, novo obejcto e gerado
    ; pseudo algo:
    ; deve actualizar as listas de peças,
    ; colocar a peça especificada pela acção na posição correcta
    ; depois de colocada a peça,
    ; verifica se o topo do tabuleiro está preenchido;
    ; caso sim:não se removem linhas e devolve-se o estado
    ; case não:removem-se as linhas e calculam-se os pontos obtidos
    (declare (ignore estado accao))
)

;;; qualidade: estado -> inteiro
(defun qualidade (estado)
    ; recebe estado e devolve inteiro que corresponde
    ; ao valor de pontos ganhos ate ao momento em valor negativo.
    ; **** LER COM MAIS ATENCAO ****
    (declare (ignore estado))
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
    ; ultimo nó a ser colocado na fronteira deverá ser o primeiro a ser explorado.
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



(setf tab (cria-tabuleiro))
(dotimes (n T-NCOLUNAS)
    (tabuleiro-preenche! tab 5 n)
    (tabuleiro-preenche! tab 6 n)
    (tabuleiro-preenche! tab 7 n)
    (tabuleiro-preenche! tab 15 n)
    (if (eq (mod n 2) 0)
        (tabuleiro-preenche! tab 16 n)
    )
    (tabuleiro-preenche! tab 17 n)

)
(format t "~d ~%" tab)

; replace with this one when submiting on mooshak
; (load "utils.fas")
; (load (compile-file "utils.lisp"))
