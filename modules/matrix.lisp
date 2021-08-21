
;; Estrutura de uma celula do tabuleiro
;; Possue valor da celula, valor do bloco e posiçao no tabuleiro
(defstruct Cell
    value
    block
    position
)

;; Retorna tamanho de uma matriz quadrada
(defun size (matrix)
    (sqrt (length (flatten matrix))))

;; Retorna as colunas de uma matriz
(defun cols (matrix)
    (apply #'mapcar #'list matrix))

;; Retorna as linhas de uma matriz
(defun rows (matrix)
    (return-from rows matrix))

;; Retorna todas as celulas de uma matriz em uma unica lista
(defun flatten (matrix)
    (apply #'append matrix))

;; Retorna a matriz em uma unica lista removendo as ocorrências de NIL
(defun full-flatten (lista)
    (if lista
        (if (atom lista)
            (list lista)
            (append (full-flatten (car lista)) (full-flatten (cdr lista))))))

;; Formata a matriz para exibição no terminal
(defun format-matrix(file)
    (setq grid (read-puzzle file))
    (setq matrix (solve grid))
    (setq ffm (full-flatten matrix))
    (setq ffm-value (mapcar #'Cell-value ffm))
    (setq matrix-size (sqrt (length ffm-value)))
    (setq printed-matrix (chunks-of ffm-value matrix-size))
)

;; retorna uma lista dos blocos ordenados
(defun blocks (matrix)
    (setq flattened (flatten matrix))
    (setq blocks (mapcar #'Cell-block flattened))
    (setq num-of-blocks (apply #'max blocks))
    (loop
        for b to num-of-blocks
        collect (remove-if-not (lambda (el) (= (cell-block el) b)) flattened)))

;; Retorna numero de celulas em um bloco
(defun length-of-block (block-number matrix)
    (length (nth block-number (blocks matrix))))

;; Retorna valores das celulas de um bloco escolhido
(defun vals-of-block (block-number matrix)
    (mapcar #'Cell-value (nth block-number (blocks matrix))))

;; Retorna uma celula a partir de sua posiçao
(defun cell-from-pos (pos matrix)
    (find-if (lambda (c) (equal (cell-position c) pos)) (flatten matrix)))

;; Retorna os valores contidos nos campos ortogonais da célula informada
(defun vals-of-neighbors (c matrix)
    (setq row-c (first (cell-position c)))
    (setq col-c (second (cell-position c)))
    (mapcar (lambda (el) (if el (cell-value el) 0))
        (flatten (loop
            for i from -1 to 1 by 2
            for neighbor-row = (cell-from-pos (list (+ row-c i) col-c) matrix)
            for neighbor-col = (cell-from-pos (list row-c (+ col-c i)) matrix)
            collect (list neighbor-row neighbor-col)))))

;; Lista de blocos presentes em colunas
(defun blocks-by-cols (matrix)
    (loop for col in (cols matrix)
        append (group-by col :key #'Cell-block))
)

;; Divide uma coluna em grupos
(defun group-by (list &key (test #'eql) (key #'identity))
    (labels 
        ((travel (tail group groups)
            (cond ((endp tail) (mapcar #'nreverse (cons group groups)))
                ((funcall test
                    (funcall key (car tail))
                    (funcall key (car group)))
                    (travel (cdr tail) (cons (car tail) group) groups))
            (t (travel (cdr tail) (list (car tail)) (cons group groups))))))
        ;; do
        (nreverse (travel (cdr list) (list (car list)) nil))))

;; Retorna matriz desfazendo a divisao do blocks-by-cols
(defun undo-blocks-by-cols (blocks-by-cols)
    (chunks-of (flatten blocks-by-cols) (size blocks-by-cols)))

;; Retorna matriz de tamanho n a partir de uma lista de celulas 
(defun chunks-of (lst n)
    (labels 
        ((take (lst n) (subseq lst 0 n))
         (drop (lst n) (subseq lst n))
         (split-parts-inner (lst n acc)
             (if (null lst)
                acc
                (split-parts-inner (drop lst n) n (append acc (list (take lst n)))))))
        ;; do
        (split-parts-inner lst n '())))