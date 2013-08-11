(defun read-lines (file)
  (with-temp-buffer 
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun go (file)
  (let ((output-buffer (generate-new-buffer "pprinted")))
    (mapcar (lambda (line)
              (with-current-buffer output-buffer
                (goto-char (point-max))
                (insert (pp-to-string (read line)))
                (newline)))
            (read-lines file))))

(go "~/src/icfp2013/bonus/even_more_bonus")
