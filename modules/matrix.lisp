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
    (loop for col in (cols matrix)
        append (group-blocks col)
    )
    ;; (loop 
    ;;     for col in (cols matrix)
    ;;     do (loop 
    ;;                 for (a b) on col by #'cdr
    ;;                 for block-a = (cell-block a)
    ;;                 for block-b = (cell-block b)
    ;;                 while b
    ;;                 do (print block-a)
    ;;                     (print block-b)
    ;;                 collect (if (= block-a block-b) (list a b))
    ;;             )
    ;; )
)

(defun group-blocks (row)
    
)

;; TODO
(defun undo-blocks-by-cols (blocks-by-cols)
    ;; chunks-of (flatten blocks-by-cols) (size matrix)
    )

;; TODO
(defun chunks-of ( l n / a b )
    (while l
        (repeat n
            (setq a (cons (car l) a)
                  l (cdr l)))
        (setq b (cons (reverse a) b)
              a nil)
    )
    (reverse b))