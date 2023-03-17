;; -*- lexical-binding: t -*-

(defmacro /init/measure-load (target expr)
  "Measures library load times."
  (declare (indent defun))
  `(let ((elapsed)
         (start (current-time)))
     (prog1
         (apply (car ,expr) (cdr ,expr))
       (with-current-buffer (get-buffer-create "*Load Times*")
         (when (= 0 (buffer-size))
           (insert
            (format "| %-30s | %-23s | elapsed  |\n" "feature" "timestamp"))
           (insert "|--------------------------------+-------------------------+----------|\n"))
         (goto-char (point-max))
         (setq elapsed (float-time (time-subtract (current-time) start)))
         (insert (format "| %-30s | %s | %f |\n"
                         ,target
                         (format-time-string "%Y-%m-%d %H:%M:%S.%3N"
                                             (current-time))
                         elapsed))))))

(advice-add 'load :around
            (lambda (&rest args)
              (/init/measure-load (nth 1 args) args)))

(advice-add 'require :around
            (lambda (&rest args)
              (if (memq (nth 1 args) features)
                  ;; If already loaded, don't count time.
                  (apply (car args) (cdr args))
                (/init/measure-load (nth 1 args) args))))
