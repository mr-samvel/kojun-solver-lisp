(load "modules/matrix.lisp")
(load "modules/reader.lisp")
(load "modules/solver.lisp")

;; Função principal
(defun main()
    (print "Insira o documento que contém o tabuleiro (ex.: tabuleiros/tabuleiro10x10.txt):")
    (setq file (read))
    (setq matrix (format-matrix file))
    (format t "~{~a~^~%~}" matrix)
)

(main)