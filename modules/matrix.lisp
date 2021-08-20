(defstruct Cell
    value
    block
    position
)

(defun size (matrix)
    (sqrt (length (flatten matrix))))

(defun cols (matrix)
  (apply #'mapcar #'list matrix))

(defun rows (matrix)
    (return-from rows matrix))

(defun flatten (matrix)
    (apply #'append matrix))

;; retorna uma lista dos blocos ordenados
(defun blocks (matrix)
    (setq flattened (flatten matrix))
    (setq blocks (mapcar #'Cell-block flattened))
    (setq num-of-blocks (apply #'max blocks))
    (loop
        for b to num-of-blocks
        collect (remove-if-not (lambda (el) (= (cell-block el) b)) flattened)))

(defun length-of-block (block-number matrix)
    (length (nth block-number (blocks matrix))))

(defun vals-of-block (block-number matrix)
    (mapcar #'Cell-value (nth block-number (blocks matrix))))

(defun cell-from-pos (pos matrix)
    (find-if (lambda (c) (equal (cell-position c) pos)) (flatten matrix)))

(defun vals-of-neighbors (c matrix)
    (setq row-c (first (cell-position c)))
    (setq col-c (second (cell-position c)))
    (mapcar (lambda (el) (if el (cell-value el) 0))
        (flatten (loop
            for i from -1 to 1 by 2
            for neighbor-row = (cell-from-pos (list (+ row-c i) col-c) matrix)
            for neighbor-col = (cell-from-pos (list row-c (+ col-c i)) matrix)
            collect (list neighbor-row neighbor-col)))))

;; lista de blocos
(defun blocks-by-cols (matrix)
    (loop for col in (cols matrix)
        append (group-by col :key #'Cell-block))
)

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

(defun undo-blocks-by-cols (blocks-by-cols)
    (chunks-of (flatten blocks-by-cols) (size blocks-by-cols)))

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