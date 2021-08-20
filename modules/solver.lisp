;; Retorna true se a lista passada só possui um elemento
(defun single (list)
    (if (= (length list) 1) T NIL))

;; se a lst1 eh single, retorna lst1, se nao, retorna lst1 \\ lst2
(defun minus (lst1 lst2)
    (if (single lst1) lst1 (set-difference lst1 lst2)))

(defun fill-values-with-choices (matrix)
    (setq local-matrix (loop for row in matrix collect (mapcar #'copy-structure row)))
    (labels (
        ;; [1..n]
        (one-to-n (n) (loop for i from 1 to n collect i))
        (gen-choices (el)
            (if (= (cell-value el) 0)
                ;; then
                (progn (setq choices (one-to-n (length-of-block (cell-block el) matrix)))
                (setq choices (minus choices (vals-of-block (cell-block el) matrix)))
                (minus choices (vals-of-neighbors el matrix)))
                ;; else
                (list (cell-value el)))))
        ;; do
        (loop
            for el in (flatten matrix)
            do (progn 
                (setq local-cell (cell-from-pos (cell-position el) local-matrix))
                (setf (cell-value local-cell) (gen-choices el)))))
    (return-from fill-values-with-choices local-matrix))

;; Retorna true se a matriz passada nunca pode fornecer uma solução.
(defun blocked (matrix)
    (not (safe matrix)))

;; Retorna true se as matrizes passam nos seguintes testes:
;; ;; Cada célula não possui vizinhos com o mesmo valor;
;; ;; Cada bloco não possui valores duplicados;
;; ;; Cada bloco respeita a ordem de valores decrescentes na vertical.
(defun safe (matrix)
    (and
        (notevery #'void (rows matrix))
        (every #'valid-neighborhood (cols matrix))
        (every #'valid-neighborhood (rows matrix))
        (every #'nodups (blocks matrix))
        (every #'is-decreasing (blocks-by-cols matrix))))

;; recebe uma lista de celulas
;; Verifica se não há valores unitarios duplicados na linha passada
(defun nodups (row)
    (labels (
        (is-uniques-list (l)
            (or (null l)
                (and (not (member (car l) (cdr l)))
                (is-uniques-list (cdr l))))))
        ;; do
        (is-uniques-list (flatten
            (loop
                for el in row 
                for v = (cell-value el)
                collect (if (single v) v NIL) into raw
                finally (return (remove NIL raw)))))))

;; Verifica se nao ha valores unitarios iguais na linha passada
(defun valid-neighborhood (row)
    (if (<= (length row) 1)
        T
        (progn (setq a (cell-value (car row)))
        (setq b (cell-value (car (cdr row))))
        (if (and (<= (length a) 1) 
                 (<= (length b) 1))
            ;; then
            (if (equal a b) NIL (valid-neighborhood (cdr row)))
            ;; else
            (valid-neighborhood (cdr row))))))

;; Verifica se os valores unitários da linha passada estão em ordem decrescente
(defun is-decreasing (row)
    (if (<= (length row) 1)
        T
        (progn (setq a (cell-value (car row)))
        (setq b (cell-value (car (cdr row))))
        (if (and (<= (length a) 1) 
                 (<= (length b) 1))
            ;; then
            (if (< (car a) (car b)) NIL (is-decreasing (cdr row)))
            ;; else
            (is-decreasing (cdr row))))))

;; Verifica se ha alguma celula vazia na linha passada
(defun void (row)
    (if (member NIL (loop for el in (list c1 c2 c3 c4) collect (cell-value el))) T NIL))

;; para a linha passada, retorna os valores unitarios
(defun singles (row)
    (flatten (remove-if-not #'single (loop for el in row collect (cell-value el)))))

;; -- De uma linha contendo escolhas, reduz as escolhas com base em elementos unitários
;; -- ex: ["1 2 3 4", "1", "3 4", "3"] -> ["2 4", "1", "4", "3"]
(defun reduce-choices (row)
    (setq s (singles row))
    (loop for el in row do (setf (cell-value el) (minus (cell-value el) s))))

;; Aplica a função reduce para as celulas de cada coluna dividida por blocos.
(defun prune (matrix)
    (cols (undo-blocks-by-cols (mapcar #'reduce-choices (blocks-by-cols matrix)))))

;; -- Recebe uma matriz de escolhas e a matriz de posições.
;; -- Retorna uma lista que contém soluções válidas para o tabuleiro.
;; -- A ideia desse algoritmo é de que filtre todas as escolhas possíveis, uma célula por vez,
;; -- e retorne somente matrizes que contém escolhas válidas.
;; search :: Matrix Choices -> Grid -> [Grid]
;; search vals pos
;;     -- nao retorna nada se a matriz de escolhas passada não pode fornecer uma solução
;;     | blocked vals pos = []
;;     -- se a matriz de escolhas passada é válida e só contém valores unitários, é uma solução,
;;     -- portanto, extrai o valor de cada lista de escolhas e retorna.
;;     | all (all single) vals = [map concat vals]
;;     -- se a matriz de escolhas passada é valida e contém mais de uma escolha para pelo menos uma célula,
;;     -- expande a matriz, reduz o número de escolhas restantes e continua o processo de busca sobre ela.
;;     | otherwise = [g | vals' <- expand vals, g <- search (prune vals' pos) pos]

(defun search(matrix)
    (if (blocked matrix)
        ;then
        (list )
        ;else
        (if (every #'single (flatten matrix))
            ;then
            matrix
            ;else
        )
    )
)


;; -- Expand funciona de modo similar à collapse. A diferença é que faz o collapse
;; -- apenas para a primeira célula que contém mais de uma escolha.
;; expand :: Matrix Choices -> [Matrix Choices]
;; expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
;;     where
;;         (rows1,row:rows2) = break (any (not . single)) m   ->
;;         (row1,cs:row2) = break (not . single) row

(defun expand (matrix)
    (setq )
)


;; -- Recebe a matriz de valores e de posições lida do documento de texto.
;; -- Retorna a primeira solução encontrada para o tabuleiro.
;; solve :: Grid -> Grid -> Grid
;; solve vals pos = (search (prune (choices vals pos) pos) pos)!!0

;; (defun solve (matrix)
;;     (searchs (prune (choices (matrix))))
;; )