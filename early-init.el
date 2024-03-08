;;;; early-init-config.el --- todo  -*- lexical-binding: t; -*-

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun se-initialize-packages ()
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(add-hook 'before-init-hook #'se-initialize-packages)
