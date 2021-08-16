(defstruct cell
    value
    block
    position
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; READER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; split "A,B,C,D" ","
;; => ("A" "B" "C" "D")
(defun split (str del)
    (loop for i = 0 then (1+ j)
        as j = (position del str :start i :test #'string=)
        collect (subseq str i j)
        while j))

;; create-cells ("0,1" "1,1") 0
;; => ((CELL :VALUE 0 :BLOCK 1 :POSITION (0 0)) (CELL :VALUE 1 :BLOCK 1 :POSITION (0 1)))
(defun create-cells (raw-elements row)
    (loop 
        for raw in raw-elements
        for col from 0
        for el = (split raw ",")
        collect (make-cell :value (parse-integer (first el)) :block (parse-integer (second el)) :position (list row col))))

;; le o tabuleiro no path indicado e retorna uma lista de cells
(defun read-puzzle (path)
    (with-open-file (file path)
        (loop 
            for line = (read-line file nil)
            for i from 0
            while line
            collect (create-cells (split line " ") i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; AUXILIARES/MANIPULAÇÃO DE DADOS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
(defun row (cells))

;; TODO
(defun cols (cells))

;; TODO
(defun by-blocks (cells))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main()
    (write-line (write-to-string (read-puzzle "tabuleiros/tabuleiro3x3.txt")))
)

(main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; estacionamento de cursores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor do Samuel  |        |  Cursor do Eduardo  |        |  Cursor do Leonardo  |        |  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
