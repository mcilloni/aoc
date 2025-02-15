(require :asdf)

(defun middle (list)
    (assert (oddp (length list)))
    (nth (floor (/ (length list) 2)) list)
)

(defun parse-file (path)
    (let 
        ((result 0))
        (loop
            with section = 1
                and lines = (uiop:read-file-lines path)
                and should-come-before-map = (make-hash-table)
            for line in lines do
            (cond
                ((= section 1) (
                    let ((entry (parse-mapping line)))
                    (cond
                        (entry (let
                            (
                                (page (first entry))
                                (always-after (second entry))
                            )
                            (push always-after (gethash page should-come-before-map))
                        ))

                        ((string= line "") (setf section 2))

                        (t (error "malformed first section"))
                    )
                ))

                ((= section 2) (if
                    (validate-update should-come-before-map (parse-update line))
                    (setf result (+ result (middle (parse-update line))))
                ))

                (t (error "unknown section"))
            )    
        )
        result
    )
)

(defun parse-mapping (line)
    (let
        ((parts (uiop:split-string line :separator "|")))
        (cond 
            ((= (length parts) 2) (mapcar #'parse-integer parts))
            (t nil)
        )
    )
)

(defun parse-update (line) (mapcar #'parse-integer (uiop:split-string line :separator ",")))

(defun validate-update (after-map after-values)
    (if after-values
        (and
            (validate-value after-map (first after-values) (rest after-values))
            (validate-update after-map (rest after-values))
        )
        t
    )
)

(defun validate-value (after-map value after-values)
    ; in this function, we verify that none of the following values are required to come before the current value
    (notany (lambda (n) (member value (gethash n after-map))) after-values)
)

(let
    ((args (uiop:command-line-arguments)))

    (if (= (length args) 1)
        (print (parse-file (first args)))
        (write-line "Usage: 05.lsp INPUT")
    )
  )