;;; peca-i: {} -> lista de accoes
; (
;     (0 . #2A((T) (T) (T) (T)))
;     (1 . #2A((T) (T) (T) (T)))
;     (2 . #2A((T) (T) (T) (T)))
;     (3 . #2A((T) (T) (T) (T)))
;     (4 . #2A((T) (T) (T) (T)))
;     (5 . #2A((T) (T) (T) (T)))
;     (6 . #2A((T) (T) (T) (T)))
;     (7 . #2A((T) (T) (T) (T)))
;     (8 . #2A((T) (T) (T) (T)))
;     (9 . #2A((T) (T) (T) (T)))
;     (0 . #2A((T T T T)))
;     (1 . #2A((T T T T)))
;     (2 . #2A((T T T T)))
;     (3 . #2A((T T T T)))
;     (4 . #2A((T T T T)))
;     (5 . #2A((T T T T)))
;     (6 . #2A((T T T T)))
; )
(defun peca-i ()
    ; devolve uma lista de accoes correspondentes a peca i
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (( lista (list) ))
        (dotimes (n T-NCOLUNAS)
            (setf lista (append lista (list (cria-accao n peca-i0))))
        )
        (dotimes (n (- T-NCOLUNAS 3) lista)
            (setf lista (append lista (list (cria-accao n peca-i1))))
        )
    )
)

;;; peca-l: {} -> lista de accoes
; (
;     (0 . #2A((T T) (T NIL) (T NIL)))
;     (1 . #2A((T T) (T NIL) (T NIL)))
;     (2 . #2A((T T) (T NIL) (T NIL)))  ; # Nil
;     (3 . #2A((T T) (T NIL) (T NIL)))  ; # Nil
;     (4 . #2A((T T) (T NIL) (T NIL)))  ; # #
;     (5 . #2A((T T) (T NIL) (T NIL)))
;     (6 . #2A((T T) (T NIL) (T NIL)))
;     (7 . #2A((T T) (T NIL) (T NIL)))
;     (8 . #2A((T T) (T NIL) (T NIL)))
;     (0 . #2A((T NIL NIL) (T T T)))
;     (1 . #2A((T NIL NIL) (T T T)))
;     (2 . #2A((T NIL NIL) (T T T)))
;     (3 . #2A((T NIL NIL) (T T T)))   ; #  #  #
;     (4 . #2A((T NIL NIL) (T T T)))   ; # nil nil
;     (5 . #2A((T NIL NIL) (T T T)))
;     (6 . #2A((T NIL NIL) (T T T)))
;     (7 . #2A((T NIL NIL) (T T T)))
;     (0 . #2A((NIL T) (NIL T) (T T)))
;     (1 . #2A((NIL T) (NIL T) (T T)))
;     (2 . #2A((NIL T) (NIL T) (T T)))  ;   # #
;     (3 . #2A((NIL T) (NIL T) (T T)))  ; nil #
;     (4 . #2A((NIL T) (NIL T) (T T)))  ; nil #
;     (5 . #2A((NIL T) (NIL T) (T T)))
;     (6 . #2A((NIL T) (NIL T) (T T)))
;     (7 . #2A((NIL T) (NIL T) (T T)))
;     (8 . #2A((NIL T) (NIL T) (T T)))
;     (0 . #2A((T T T) (NIL NIL T)))
;     (1 . #2A((T T T) (NIL NIL T)))
;     (2 . #2A((T T T) (NIL NIL T)))
;     (3 . #2A((T T T) (NIL NIL T)))   ; nil nil #
;     (4 . #2A((T T T) (NIL NIL T)))   ; #   #   #
;     (5 . #2A((T T T) (NIL NIL T)))
;     (6 . #2A((T T T) (NIL NIL T)))
;     (7 . #2A((T T T) (NIL NIL T)))
; )
(defun peca-l ()
    ; devolve uma lista de accoes correspondentes a peca l
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em l0 e passar para l3, l2 e l1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para l3 -> l2 -> l1
    (let (( lista (list) ))
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-l0))))
        )
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-l1))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-l2))))
        )
        (dotimes (n (- T-NCOLUNAS 2) lista)
           (setf lista (append lista (list (cria-accao n peca-l3))))
        )

    )
)

;;; peca-j: {} -> lista de accoes
; (
;     (0 . #2A((T T) (NIL T) (NIL T)))
;     (1 . #2A((T T) (NIL T) (NIL T)))
;     (2 . #2A((T T) (NIL T) (NIL T)))
;     (3 . #2A((T T) (NIL T) (NIL T)))  ; Nil #
;     (4 . #2A((T T) (NIL T) (NIL T)))  ; nil #
;     (5 . #2A((T T) (NIL T) (NIL T)))  ;   # #
;     (6 . #2A((T T) (NIL T) (NIL T)))
;     (7 . #2A((T T) (NIL T) (NIL T)))
;     (8 . #2A((T T) (NIL T) (NIL T)))
;     (0 . #2A((T T T) (T NIL NIL)))
;     (1 . #2A((T T T) (T NIL NIL)))
;     (2 . #2A((T T T) (T NIL NIL)))   ; # nil nil
;     (3 . #2A((T T T) (T NIL NIL)))   ; #  #  #
;     (4 . #2A((T T T) (T NIL NIL)))
;     (5 . #2A((T T T) (T NIL NIL)))
;     (6 . #2A((T T T) (T NIL NIL)))
;     (7 . #2A((T T T) (T NIL NIL)))
;     (0 . #2A((T NIL) (T NIL) (T T)))
;     (1 . #2A((T NIL) (T NIL) (T T)))
;     (2 . #2A((T NIL) (T NIL) (T T)))  ; # #
;     (3 . #2A((T NIL) (T NIL) (T T)))  ; # nil
;     (4 . #2A((T NIL) (T NIL) (T T)))  ; # nil
;     (5 . #2A((T NIL) (T NIL) (T T)))
;     (6 . #2A((T NIL) (T NIL) (T T)))
;     (7 . #2A((T NIL) (T NIL) (T T)))
;     (8 . #2A((T NIL) (T NIL) (T T)))
;     (0 . #2A((NIL NIL T) (T T T)))
;     (1 . #2A((NIL NIL T) (T T T)))
;     (2 . #2A((NIL NIL T) (T T T)))   ;   #  #  #
;     (3 . #2A((NIL NIL T) (T T T)))   ; nil nil #
;     (4 . #2A((NIL NIL T) (T T T)))
;     (5 . #2A((NIL NIL T) (T T T)))
;     (6 . #2A((NIL NIL T) (T T T)))
;     (7 . #2A((NIL NIL T) (T T T)))
; )
(defun peca-j ()
    ; devolve uma lista de accoes correspondentes a peca j
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em j0 e passar para j3, j2 e j1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para j3 -> j2 -> j1
    (let (( lista (list) ))
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-j0))))
        )
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-j1))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-j2))))
        )
        (dotimes (n (- T-NCOLUNAS 2) lista)
           (setf lista (append lista (list (cria-accao n peca-j3))))
        )
    )
)

;;; peca-o: {} -> lista de accoes
; (
;     (0 . #2A((T T) (T T)))
;     (1 . #2A((T T) (T T)))
;     (2 . #2A((T T) (T T)))
;     (3 . #2A((T T) (T T))) ; # #
;     (4 . #2A((T T) (T T))) ; # #
;     (5 . #2A((T T) (T T)))
;     (6 . #2A((T T) (T T)))
;     (7 . #2A((T T) (T T)))
;     (8 . #2A((T T) (T T)))
; )
(defun peca-o ()
    ; devolve uma lista de accoes correspondentes a peca o
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (( lista (list) ))
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-o0))))
        )
    )
)

;;; peca-s: {} -> lista de accoes
; (
;     (0 . #2A((T T NIL) (NIL T T)))
;     (1 . #2A((T T NIL) (NIL T T)))
;     (2 . #2A((T T NIL) (NIL T T)))
;     (3 . #2A((T T NIL) (NIL T T)))  ; nil # #
;     (4 . #2A((T T NIL) (NIL T T)))  ;   # # Nil
;     (5 . #2A((T T NIL) (NIL T T)))
;     (6 . #2A((T T NIL) (NIL T T)))
;     (7 . #2A((T T NIL) (NIL T T)))
;     (0 . #2A((NIL T) (T T) (T NIL)))
;     (1 . #2A((NIL T) (T T) (T NIL)))
;     (2 . #2A((NIL T) (T T) (T NIL)))
;     (3 . #2A((NIL T) (T T) (T NIL))) ;   # nil
;     (4 . #2A((NIL T) (T T) (T NIL))) ;   # #
;     (5 . #2A((NIL T) (T T) (T NIL))) ; nil #
;     (6 . #2A((NIL T) (T T) (T NIL)))
;     (7 . #2A((NIL T) (T T) (T NIL)))
;     (8 . #2A((NIL T) (T T) (T NIL)))
; )
(defun peca-s ()
    ; devolve uma lista de accoes correspondentes a peca s
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (( lista (list) ))
        (dotimes (n (- T-NCOLUNAS 2))
            (setf lista (append lista (list (cria-accao n peca-s0))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-s1))))
        )
    )
)

;;; peca-z: {} -> lista de accoes
; (
;     (0 . #2A((NIL T T) (T T NIL)))
;     (1 . #2A((NIL T T) (T T NIL)))
;     (2 . #2A((NIL T T) (T T NIL)))
;     (3 . #2A((NIL T T) (T T NIL)))  ;   # # nil
;     (4 . #2A((NIL T T) (T T NIL)))  ; nil # #
;     (5 . #2A((NIL T T) (T T NIL)))
;     (6 . #2A((NIL T T) (T T NIL)))
;     (7 . #2A((NIL T T) (T T NIL)))
;     (0 . #2A((T NIL) (T T) (NIL T)))
;     (1 . #2A((T NIL) (T T) (NIL T)))
;     (2 . #2A((T NIL) (T T) (NIL T)))
;     (3 . #2A((T NIL) (T T) (NIL T))) ; nil #
;     (4 . #2A((T NIL) (T T) (NIL T))) ;   # #
;     (5 . #2A((T NIL) (T T) (NIL T))) ;   # nil
;     (6 . #2A((T NIL) (T T) (NIL T)))
;     (7 . #2A((T NIL) (T T) (NIL T)))
;     (8 . #2A((T NIL) (T T) (NIL T)))
; )
(defun peca-z ()
    ; devolve uma lista de accoes correspondentes a peca z
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    (let (( lista (list) ))
        (dotimes (n (- T-NCOLUNAS 2))
            (setf lista (append lista (list (cria-accao n peca-z0))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
            (setf lista (append lista (list (cria-accao n peca-z1))))
        )
    )
)

;;; peca-t: {} -> lista de accoes
; (
;     (0 . #2A((T T T) (NIL T NIL)))
;     (1 . #2A((T T T) (NIL T NIL)))
;     (2 . #2A((T T T) (NIL T NIL)))
;     (3 . #2A((T T T) (NIL T NIL))) ; nil # nil
;     (4 . #2A((T T T) (NIL T NIL))) ;   # # #
;     (5 . #2A((T T T) (NIL T NIL)))
;     (6 . #2A((T T T) (NIL T NIL)))
;     (7 . #2A((T T T) (NIL T NIL)))
;     (0 . #2A((T NIL) (T T) (T NIL)))
;     (1 . #2A((T NIL) (T T) (T NIL)))
;     (2 . #2A((T NIL) (T T) (T NIL)))
;     (3 . #2A((T NIL) (T T) (T NIL))) ; # nil
;     (4 . #2A((T NIL) (T T) (T NIL))) ; # #
;     (5 . #2A((T NIL) (T T) (T NIL))) ; # nil
;     (6 . #2A((T NIL) (T T) (T NIL)))
;     (7 . #2A((T NIL) (T T) (T NIL)))
;     (8 . #2A((T NIL) (T T) (T NIL)))
;     (0 . #2A((NIL T NIL) (T T T)))
;     (1 . #2A((NIL T NIL) (T T T)))
;     (2 . #2A((NIL T NIL) (T T T)))
;     (3 . #2A((NIL T NIL) (T T T)))  ;   # # #
;     (4 . #2A((NIL T NIL) (T T T)))  ; nil # nil
;     (5 . #2A((NIL T NIL) (T T T)))
;     (6 . #2A((NIL T NIL) (T T T)))
;     (7 . #2A((NIL T NIL) (T T T)))
;     (0 . #2A((NIL T) (T T) (NIL T)))
;     (1 . #2A((NIL T) (T T) (NIL T)))
;     (2 . #2A((NIL T) (T T) (NIL T)))
;     (3 . #2A((NIL T) (T T) (NIL T))) ; nil #
;     (4 . #2A((NIL T) (T T) (NIL T))) ;   # #
;     (5 . #2A((NIL T) (T T) (NIL T))) ; nil #
;     (6 . #2A((NIL T) (T T) (NIL T)))
;     (7 . #2A((NIL T) (T T) (NIL T)))
;     (8 . #2A((NIL T) (T T) (NIL T)))
;)
(defun peca-t ()
    ; devolve uma lista de accoes correspondentes a peca t
    ; cria uma lista vazia e vai adicionando accoes com as colunas
    ; possiveis para esta peca especifica e com a sua configuracao
    ; A escolha da orientacao comecar em t0 e passar para t3, t2 e t1,
    ; esta descrito no enunciado que a lista deve comecar pela orientacao inicial da peca
    ; e ir alterando a orientacao rodando a peca 90 graus no sentido horario,
    ; passando para t3 -> t2 -> t1
    (let (( lista (list) ))
        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-t0))))
        )
        (dotimes (n (1- T-NCOLUNAS))
           (setf lista (append lista (list (cria-accao n peca-t1))))
        )

        (dotimes (n (- T-NCOLUNAS 2))
           (setf lista (append lista (list (cria-accao n peca-t2))))
        )
        (dotimes (n (1- T-NCOLUNAS) lista)
           (setf lista (append lista (list (cria-accao n peca-t3))))
        )

    )
)
