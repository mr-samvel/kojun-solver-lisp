(load "modules/matrix.lisp")
(load "modules/reader.lisp")
(load "modules/solver.lisp")

(defun main()
    (setq grid (read-puzzle "tabuleiros/tabuleiro3x3.txt"))
    
    ;             coluna   linha
    (setq celula (nth 1 (nth 2 grid)))
    ;; (setq celula (nth 0 (nth 1 grid)))
    
    (setq c1 (make-Cell :value (list 4) :block 0 :position (list 0 1)))
    (setq c2 (make-Cell :value (list 3) :block 0 :position (list 0 2)))
    (setq c3 (make-Cell :value (list 1) :block 0 :position (list 1 1)))
    (setq c4 (make-Cell :value (list 4) :block 0 :position (list 1 2)))

    (setq test-matrix (list (list c1 c2) (list c3 c4)))
    
    ;; (write-line (write-to-string (fill-values-with-choices grid)))

    ;; (write-line (write-to-string (blocked test-matrix)))

    ;;(write-line (write-to-string (void grid)))
    ;; (print (void (reduce-choices (list c1 c2 c3 c4))))
    ;; (print (list c1 c2 c3 c4))
)

(main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; estacionamento de cursores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor do Samuel  |        |  Cursor do Eduardo  |        |  Cursor do Leonardo  |        |  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
