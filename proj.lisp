
;;;    Projecto de Inteligencia Artificial     ;;;
;;;              1a Entrega                    ;;;
;;;               Grupo 54                     ;;;
;;;          	Ana Galvao 75312	           ;;;
;;;       Jose Diogo Oliveira 75255            ;;;
;;;    			Andre Silva 75455		       ;;;


; cria-accao: inteiro x array -> accao
(defun cria-accao (pos_esq config)
	(cons pos_esq config))

; accao-coluna: accao-coluna: accao -> inteiro
(defun accao-coluna (accao)
	(car accao))

; accao-peca: accao -> array 
(defun accao-peca(accao)
	(cdr accao))


; cria-tabuleiro {} -> tabuleiro
(defun cria-tabuleiro()
	;; criar o tabuleiro?
	)

; copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro (tabuleiro)
	;funcao para copiar
	)

; tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tabuleiro nlinha ncoluna)
	) ; nth

; tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tabuleiro ncoluna)
	)

; tabuleiro-linha-completa-p: tabuleiro x inteiro -> lógico
(defun tabuleiro-linha-completa-p)

; tabuleiro-preenche!: tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche)

; tabuleiro-remove-linha!: tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha!)

; tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p)

; tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p)

; tabuleiro->array: tabuleiro -> array
(defun tabuleiro->array)

; array->tabuleiro: array->tabuleiro
(defun array->tabuleiro)


; creates type Estado

(defstruct Estado)

; copia-estado: estado->estado
(defun copia-estado)

; estados-iguais-p: estado x estado -> logico
(defun estados-iguais-p)

; estado-final-p: estado -> logico
(defun estado-final-p)


; ; creates type Problema

(defstruct Problema)