(load "modules/matrix.lisp")
(load "modules/reader.lisp")

(defun main()
    (setq grid (read-puzzle "tabuleiros/tabuleiro3x3.txt"))
    ;; (write-line (write-to-string (size grid)))
    ;; (write-line (write-to-string (rows grid)))
    ;; (write-line (write-to-string (cols grid)))
    ;; (write-line (write-to-string (blocks grid)))
    ;; (write-line (write-to-string (length-of-block 0 grid)))
    ;; (write-line (write-to-string (vals-of-block 2 grid)))
    
    ;             coluna   linha
    ;;(setq celula (nth 1 (nth 2 grid)))
    
    ;; (vals-of-neighbors celula11 grid)
    ;;(write-line (write-to-string (vals-of-neighbors celula grid)))
    (write-line (write-to-string (blocks-by-cols grid)))
)

(main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; estacionamento de cursores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor do Samuel  |        |  Cursor do Eduardo  |        |  Cursor do Leonardo  |        |  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
