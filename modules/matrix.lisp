(defstruct Cell
    value
    block
    position
)

(defun size (matrix)
    (length matrix))

(defun cols (matrix)
  (apply #'mapcar #'list matrix))

(defun rows (matrix)
    (return-from rows matrix))

(defun flatten (matrix)
    (apply #'append matrix))

;; retorna uma lista dos blocos em ordem crescente
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

;; TODO
(defun blocks-by-cols (matrix)
    )

;; TODO
(defun undo-blocks-by-cols (blocks-by-cols)
    )