;;; gruvbox-dark-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Greduan
;; Copyright (c) 2016-2017 Jason Milkins
;; Copyright (c) 2017 Martijn Terpstra

;; Author: Jason Milkins <jasonm23@gmail.com>
;; (current maintainer)
;;
;; Author-list: Lee Machin <ljmachin@gmail.com>,
;;              Greduan <me@greduan.com>
;;
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 1.12.0

;; Package-Requires: ((autothemer "0.2"))

;;; Commentary:

;; Using autothemer since 1.00

;; A port of the Gruvbox colorscheme for Vim, built on top of the new built-in
;; theme support in Emacs 24.
;;
;; This theme contains my own modifications and it's a bit opinionated
;; sometimes, deviating from the original because of it. I try to stay
;; true to the original as much as possible, however. I only make
;; changes where I would have made the changes on the original.
;;
;; Since there is no direct equivalent in syntax highlighting from Vim to Emacs
;; some stuff may look different, especially in stuff like JS2-mode, where it
;; adds stuff that Vim doesn't have, in terms of syntax.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim, on which this port
;; is based.

;; Lee Machin created the first port of the original theme, which
;; Greduan developed further adding support for several major modes.
;;
;; Jason Milkins (ocodo) has maintained the theme since 2015 and is
;; working with the community to add further mode support and align
;; the project more closely with Vim Gruvbox.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defgroup gruvbox nil
  "Customizations for gruvbox color theme"
  :group 'faces)

(defcustom gruvbox-contrast 'medium
  "Contrast level for the theme background.
Valid options are \"soft\", \"medium\" and \"hard\""
  :options '(soft medium hard)
  :group   'gruvbox)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-dark-theme.el ends here


(provide 'gruvbox)
