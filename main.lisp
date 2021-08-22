(load "modules/matrix.lisp")
(load "modules/reader.lisp")
(load "modules/solver.lisp")

;; Função principal
(defun main()
    (print "Insira o documento que contém o tabuleiro (ex.: tabuleiros/tabuleiro10x10.txt):")
    (setq path (read))
    (setq grid (solve-and-print path))
    (format t "~{~a~^~%~}" grid)
)

(main)