;; -*- lexical-binding: t -*-

(require 'verilog3-mode)

(add-to-list
   'auto-mode-alist '("\\.\\(sv\\|v\\|svh\\|vh\\|vinc\\)\\'"
                      . verilog3-mode))
