;;; gruvbox-dark-theme.el --- A retro-groove colour theme for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Eduardo Lavaque
;; Copyright (c) 2016-2022 Jason Milkins
;; Copyright (c) 2017 Martijn Terpstra

;; Author: Jason Milkins <jasonm23@gmail.com>
;; (current maintainer)
;;
;; Author-list: Jason Milkins <jasonm23@gmail.com>,
;;              Martijn Terpstra,
;;              Eduardo Lavaque <me@greduan.com>,
;;              Lee Machin <ljmachin@gmail.com>
;;
;; URL: https://github.com/greduan/emacs-theme-gruvbox
;; Version: 1.30.1

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
;;
;; Martijn Terpstra has been a major contributor since mid 2017 and
;; helped to re-implement Gruvbox with autothemer so we can have
;; multiple variants of Gruvbox (as we do on Vim).  Martijn has also
;; provided a large number mode support enhancements.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(defvar gruvbox-bold-constructs nil
  "If non-nil, bold constructs")

(defvar gruvbox-screenshot-command "scrot -u %s%s.png"
  "Command used to take automated screenshots for gruvbox.
Should contain 2 %s constructs to allow for theme name and directory/prefix")

(defun gruvbox-screenshot (prefix)
  "Take a screenshot of all versions of the gruvbox theme"
  (interactive "sScreenshot Prefix: ")
  (dolist (theme '(gruvbox-light-soft
                   gruvbox-light-medium
                   gruvbox-light-hard
                   gruvbox-dark-soft
                   gruvbox-dark-medium
                   gruvbox-dark-hard))
    (load-theme theme t)
    (redisplay t)
    (shell-command (format gruvbox-screenshot-command
                           prefix theme))))

(defmacro gruvbox-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette
    ((default                                   (:background gruvbox-bg :foreground gruvbox-light1))
     (cursor                                    (:background gruvbox-light1))
     (mode-line                                 (:background gruvbox-dark3 :foreground gruvbox-light2 :box nil))
     (mode-line-inactive                        (:background gruvbox-dark1 :foreground gruvbox-light4 :box nil))
     (fringe                                    (:background gruvbox-bg))
     (hl-line                                   (:background gruvbox-dark1))
     (region                                    (:background gruvbox-dark2)) ;;selection
     (secondary-selection                       (:background gruvbox-dark1))
     (minibuffer-prompt                         (:foreground gruvbox-bright_green :bold t))
     (vertical-border                           (:foreground gruvbox-dark2))
     (internal-border                           (:background gruvbox-dark2))
     (window-divider                            (:foreground gruvbox-dark2))
     (link                                      (:foreground gruvbox-faded_blue :underline t))
     (shadow                                    (:foreground gruvbox-dark4))

     ;;; Built-in syntax

     (font-lock-builtin-face                            (:foreground gruvbox-bright_orange))
     (font-lock-constant-face                           (:foreground gruvbox-bright_purple))
     (font-lock-comment-face                            (:foreground gruvbox-dark4))
     (font-lock-function-name-face                      (:foreground gruvbox-bright_yellow))
     (font-lock-keyword-face                            (:foreground gruvbox-bright_red :bold ,(if gruvbox-bold-constructs t nil)))
     (font-lock-string-face                             (:foreground gruvbox-bright_green))
     (font-lock-number-face                             (:foreground gruvbox-bright_purple))
     (font-lock-variable-name-face                      (:foreground gruvbox-bright_blue))
     (font-lock-type-face                               (:foreground gruvbox-bright_purple))
     (font-lock-property-face                           (:foreground gruvbox-bright_blue))
     (font-lock-warning-face                            (:foreground gruvbox-bright_red :bold t))

     ;;; Basic faces
     (error                                             (:foreground gruvbox-bright_red :bold t))
     (success                                           (:foreground gruvbox-bright_green :bold t))
     (warning                                           (:foreground gruvbox-bright_yellow :bold t))
     (alert-low-face                                    (:foreground gruvbox-bright_blue))
     (trailing-whitespace                               (:background gruvbox-bright_red))
     (escape-glyph                                      (:foreground gruvbox-bright_aqua))
     (header-line                                       (:background gruvbox-dark0 :foreground gruvbox-light3 :box nil :inherit nil))
     (highlight                                         (:background gruvbox-dark4 :foreground gruvbox-light0))
     (homoglyph                                         (:foreground gruvbox-bright_yellow))
     (match                                             (:foreground gruvbox-dark0 :background gruvbox-bright_blue))

     ;;; Customize faces

     (widget-field                                      (:background gruvbox-dark3))
     (custom-group-tag                                  (:foreground gruvbox-bright_blue :weight 'bold))
     (custom-variable-tag                               (:foreground gruvbox-bright_blue :weight 'bold))

     ;;; whitespace-mode

     (whitespace-space                          (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-hspace                         (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-tab                            (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-newline                        (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-trailing                       (:background gruvbox-dark1 :foreground gruvbox-bright_red))
     (whitespace-line                           (:background gruvbox-dark1 :foreground gruvbox-bright_red))
     (whitespace-space-before-tab               (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-indentation                    (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-empty                          (:background 'unspecified :foreground 'unspecified))
     (whitespace-space-after-tab                (:background gruvbox-bg :foreground gruvbox-dark4))

     ;;; RainbowDelimiters

     (rainbow-delimiters-depth-1-face           (:foreground gruvbox-delimiter-one))
     (rainbow-delimiters-depth-2-face           (:foreground gruvbox-delimiter-two))
     (rainbow-delimiters-depth-3-face           (:foreground gruvbox-delimiter-three))
     (rainbow-delimiters-depth-4-face           (:foreground gruvbox-delimiter-four))
     (rainbow-delimiters-depth-5-face           (:foreground gruvbox-delimiter-one))
     (rainbow-delimiters-depth-6-face           (:foreground gruvbox-delimiter-two))
     (rainbow-delimiters-depth-7-face           (:foreground gruvbox-delimiter-three))
     (rainbow-delimiters-depth-8-face           (:foreground gruvbox-delimiter-four))
     (rainbow-delimiters-depth-9-face           (:foreground gruvbox-delimiter-one))
     (rainbow-delimiters-depth-10-face          (:foreground gruvbox-delimiter-two))
     (rainbow-delimiters-depth-11-face          (:foreground gruvbox-delimiter-three))
     (rainbow-delimiters-depth-12-face          (:foreground gruvbox-delimiter-four))
     (rainbow-delimiters-unmatched-face         (:background 'unspecified  :foreground gruvbox-light0))

     ;;; line numbers

     (line-number                               (:foreground gruvbox-dark4 :background gruvbox-dark1))
     (line-number-current-line                  (:foreground gruvbox-bright_orange :background gruvbox-dark2))
     (linum                                     (:foreground gruvbox-dark4 :background gruvbox-dark1))
     (linum-highlight-face                      (:foreground gruvbox-bright_orange :background gruvbox-dark2))
     (linum-relative-current-face               (:foreground gruvbox-bright_orange :background gruvbox-dark2))

     ;;; Highlight indentation mode

     (highlight-indentation-current-column-face (:background gruvbox-dark2))
     (highlight-indentation-face                (:background gruvbox-dark1))

     ;;; smartparens

     (sp-pair-overlay-face                      (:background gruvbox-dark2))
     (sp-show-pair-match-face                   (:background gruvbox-dark2)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face                (:background gruvbox-bright_red)) ;; Highlight for bracket without pair

     ;;; elscreen

     (elscreen-tab-background-face              (:background gruvbox-bg :box nil)) ;; Tab bar, not the tabs
     (elscreen-tab-control-face                 (:background gruvbox-dark2 :foreground gruvbox-bright_red :underline nil :box nil)) ;; The controls
     (elscreen-tab-current-screen-face          (:background gruvbox-dark4 :foreground gruvbox-dark0 :box nil)) ;; Current tab
     (elscreen-tab-other-screen-face            (:background gruvbox-dark2 :foreground gruvbox-light4 :underline nil :box nil)) ;; Inactive tab

     ;;; ag (The Silver Searcher)

     (ag-hit-face                               (:foreground gruvbox-bright_blue))
     (ag-match-face                             (:foreground gruvbox-bright_red))

     ;;; Diffs

     (diff-header                               (:background gruvbox-dark1))
     (diff-file-header                          (:background gruvbox-dark2))
     (diff-hunk-header                          (:background gruvbox-dark2))
     (diff-context                              (:background gruvbox-dark1 :foreground gruvbox-light1))
     (diff-added                                (:background 'unspecified  :foreground gruvbox-neutral_green))
     (diff-refine-added                         (:background 'unspecified  :foreground gruvbox-bright_green))
     (diff-removed                              (:background 'unspecified  :foreground gruvbox-neutral_red))
     (diff-refine-removed                       (:background 'unspecified  :foreground gruvbox-bright_red))
     (diff-indicator-changed                    (:inherit 'diff-changed))
     (diff-indicator-added                      (:inherit 'diff-added))
     (diff-indicator-removed                    (:inherit 'diff-removed))

     ;;; Ediff

     (ediff-even-diff-A                         (:background gruvbox-dark1))
     (ediff-even-diff-B                         (:background gruvbox-dark1))
     (ediff-even-diff-C                         (:background gruvbox-dark1))
     (ediff-even-diff-Ancestor                  (:background gruvbox-dark1))
     (ediff-odd-diff-A                          (:background gruvbox-dark2))
     (ediff-odd-diff-B                          (:background gruvbox-dark2))
     (ediff-odd-diff-C                          (:background gruvbox-dark2))
     (ediff-odd-diff-Ancestor                   (:background gruvbox-dark2))

     (ediff-fine-diff-A                         (:background gruvbox-ediff-fine-diff-A))
     (ediff-fine-diff-Ancestor                  (:background gruvbox-ediff-fine-diff-Ancestor))
     (ediff-fine-diff-B                         (:background gruvbox-ediff-fine-diff-B))
     (ediff-fine-diff-C                         (:background gruvbox-ediff-fine-diff-C))
     (ediff-current-diff-A                      (:background gruvbox-ediff-current-diff-A))
     (ediff-current-diff-Ancestor               (:background gruvbox-ediff-current-diff-Ancestor))
     (ediff-current-diff-B                      (:background gruvbox-ediff-current-diff-B))
     (ediff-current-diff-C                      (:background gruvbox-ediff-current-diff-C))

     (js2-warning                               (:underline (:color gruvbox-bright_yellow :style 'wave)))
     (js2-error                                 (:underline (:color gruvbox-bright_red :style 'wave)))
     (js2-external-variable                     (:underline (:color gruvbox-bright_aqua :style 'wave)))
     (js2-jsdoc-tag                             (:background 'unspecified  :foreground gruvbox-gray))
     (js2-jsdoc-type                            (:background 'unspecified  :foreground gruvbox-light4))
     (js2-jsdoc-value                           (:background 'unspecified  :foreground gruvbox-light3))
     (js2-function-param                        (:background 'unspecified  :foreground gruvbox-bright_aqua))
     (js2-function-call                         (:background 'unspecified  :foreground gruvbox-bright_blue))
     (js2-instance-member                       (:background 'unspecified  :foreground gruvbox-bright_orange))
     (js2-private-member                        (:background 'unspecified  :foreground gruvbox-faded_yellow))
     (js2-private-function-call                 (:background 'unspecified  :foreground gruvbox-faded_aqua))
     (js2-jsdoc-html-tag-name                   (:background 'unspecified  :foreground gruvbox-light4))
     (js2-jsdoc-html-tag-delimiter              (:background 'unspecified  :foreground gruvbox-light3))

     ;;; popup

     (popup-face                                (:underline nil :foreground gruvbox-light1 :background gruvbox-dark1))
     (popup-menu-mouse-face                     (:underline nil :foreground gruvbox-light0 :background gruvbox-faded_green))
     (popup-menu-selection-face                 (:underline nil :foreground gruvbox-light0 :background gruvbox-faded_green))
     (popup-tip-face                            (:underline nil :foreground gruvbox-light2 :background gruvbox-dark2))

     ;;; helm

     (helm-M-x-key                              (:foreground gruvbox-bright_orange))
     (helm-action                               (:foreground gruvbox-light0_hard :underline t))
     (helm-bookmark-addressbook                 (:foreground gruvbox-bright_red))
     (helm-bookmark-directory                   (:foreground gruvbox-bright_purple))
     (helm-bookmark-file                        (:foreground gruvbox-faded_blue))
     (helm-bookmark-gnus                        (:foreground gruvbox-faded_purple))
     (helm-bookmark-info                        (:foreground gruvbox-turquoise4))
     (helm-bookmark-man                         (:foreground gruvbox-sienna))
     (helm-bookmark-w3m                         (:foreground gruvbox-bright_yellow))
     (helm-buffer-directory                     (:foreground gruvbox-white :background gruvbox-bright_blue))
     (helm-buffer-not-saved                     (:foreground gruvbox-faded_red))
     (helm-buffer-process                       (:foreground gruvbox-burlywood4))
     (helm-buffer-saved-out                     (:foreground gruvbox-bright_red))
     (helm-buffer-size                          (:foreground gruvbox-bright_purple))
     (helm-candidate-number                     (:foreground gruvbox-bright_green))
     (helm-eshell-prompts-buffer-name           (:foreground gruvbox-bright_green))
     (helm-eshell-prompts-promptidx             (:foreground gruvbox-turquoise4))
     (helm-ff-directory                         (:foreground gruvbox-bright_purple))
     (helm-ff-executable                        (:foreground gruvbox-turquoise4))
     (helm-ff-file                              (:foreground gruvbox-sienna))
     (helm-ff-invalid-symlink                   (:foreground gruvbox-white :background gruvbox-bright_red))
     (helm-ff-prefix                            (:foreground gruvbox-black :background gruvbox-bright_yellow))
     (helm-ff-symlink                           (:foreground gruvbox-bright_orange))
     (helm-grep-cmd-line                        (:foreground gruvbox-bright_green))
     (helm-grep-file                            (:foreground gruvbox-faded_purple))
     (helm-grep-finish                          (:foreground gruvbox-turquoise4))
     (helm-grep-lineno                          (:foreground gruvbox-bright_orange))
     (helm-grep-match                           (:foreground gruvbox-bright_yellow))
     (helm-grep-running                         (:foreground gruvbox-bright_red))
     (helm-header                               (:foreground gruvbox-aquamarine4))
     (helm-helper                               (:foreground gruvbox-aquamarine4))
     (helm-history-deleted                      (:foreground gruvbox-black :background gruvbox-bright_red))
     (helm-history-remote                       (:foreground gruvbox-faded_red))
     (helm-lisp-completion-info                 (:foreground gruvbox-faded_orange))
     (helm-lisp-show-completion                 (:foreground gruvbox-bright_red))
     (helm-locate-finish                        (:foreground gruvbox-white :background gruvbox-aquamarine4))
     (helm-match                                (:foreground gruvbox-bright_orange))
     (helm-moccur-buffer                        (:foreground gruvbox-bright_aqua :underline t))
     (helm-prefarg                              (:foreground gruvbox-turquoise4))
     (helm-selection                            (:foreground gruvbox-white :background gruvbox-dark2))
     (helm-selection-line                       (:foreground gruvbox-white :background gruvbox-dark2))
     (helm-separator                            (:foreground gruvbox-faded_red))
     (helm-source-header                        (:foreground gruvbox-light2))
     (helm-visible-mark                         (:foreground gruvbox-black :background gruvbox-light3))

     ;;; helm-rg

     (helm-rg-preview-line-highlight              (:foreground gruvbox-black :background gruvbox-bright_green))
     (helm-rg-base-rg-cmd-face                    (:foreground gruvbox-light2))
     (helm-rg-extra-arg-face                      (:foreground gruvbox-bright_yellow))
     (helm-rg-inactive-arg-face                   (:foreground gruvbox-faded_aqua))
     (helm-rg-active-arg-face                     (:foreground gruvbox-bright_green))
     (helm-rg-directory-cmd-face                  (:foreground gruvbox-burlywood4 :background gruvbox-black))
     (helm-rg-error-message                       (:foreground gruvbox-bright_red))
     (helm-rg-title-face                          (:foreground gruvbox-bright_purple))
     (helm-rg-directory-header-face               (:foreground gruvbox-white :background gruvbox-dark1))
     (helm-rg-file-match-face                     (:foreground gruvbox-turquoise4))
     (helm-rg-colon-separator-ripgrep-output-face (:foreground gruvbox-dark3 :background gruvbox-bg))
     (helm-rg-line-number-match-face              (:foreground gruvbox-faded_orange))
     (helm-rg-match-text-face                     (:foreground gruvbox-white :background gruvbox-bright_purple))

     ;;; hi-lock-mode

     (hi-black-b                                (:foreground gruvbox-black :weight 'bold))
     (hi-black-hb                               (:foreground gruvbox-black :weight 'bold :height 1.5))
     (hi-blue                                   (:foreground gruvbox-dark0 :background gruvbox-bright_blue))
     (hi-blue-b                                 (:foreground gruvbox-bright_blue :weight 'bold))
     (hi-green                                  (:foreground gruvbox-dark0 :background gruvbox-bright_green))
     (hi-green-b                                (:foreground gruvbox-bright_green :weight 'bold))
     (hi-pink                                   (:foreground gruvbox-dark0 :background gruvbox-bright_purple))
     (hi-red-b                                  (:foreground gruvbox-bright_red :weight 'bold))
     (hi-yellow                                 (:foreground gruvbox-dark0 :background gruvbox-faded_yellow))

     ;;; company-mode

     (company-scrollbar-bg                      (:background gruvbox-dark2))
     (company-scrollbar-fg                      (:background gruvbox-dark1))
     (company-tooltip                           (:background gruvbox-dark1))
     (company-tooltip-annotation                (:foreground gruvbox-bright_green))
     (company-tooltip-annotation-selection      (:inherit 'company-tooltip-annotation))
     (company-tooltip-selection                 (:foreground gruvbox-bright_purple :background gruvbox-dark2))
     (company-tooltip-common                    (:foreground gruvbox-bright_blue :underline t))
     (company-tooltip-common-selection          (:foreground gruvbox-bright_blue :underline t))
     (company-preview-common                    (:foreground gruvbox-light0))
     (company-preview                           (:background gruvbox-lightblue4))
     (company-preview-search                    (:background gruvbox-turquoise4))
     (company-template-field                    (:foreground gruvbox-black :background gruvbox-bright_yellow))
     (company-echo-common                       (:foreground gruvbox-faded_red))

     ;;; tool tips

     (tooltip                                   (:foreground gruvbox-light1 :background gruvbox-dark1))

     ;;; marginalia

     (marginalia-documentation                  (:italic t :foreground gruvbox-light3))

     ;;; corfu

     (corfu-default                             (:inherit 'tooltip))
     (corfu-current                             (:foreground gruvbox-bright_purple :background gruvbox-dark2))
     (corfu-bar                                 (:background gruvbox-dark2))
     (corfu-border                              (:background gruvbox-dark1))

     ;;; term

     (term-color-black                          (:foreground gruvbox-dark2 :background gruvbox-dark1))
     (term-color-blue                           (:foreground gruvbox-bright_blue :background gruvbox-bright_blue))
     (term-color-cyan                           (:foreground gruvbox-bright_aqua :background gruvbox-bright_aqua))
     (term-color-green                          (:foreground gruvbox-bright_green :background gruvbox-bright_green))
     (term-color-magenta                        (:foreground gruvbox-bright_purple :background gruvbox-bright_purple))
     (term-color-red                            (:foreground gruvbox-bright_red :background gruvbox-bright_red))
     (term-color-white                          (:foreground gruvbox-light1 :background gruvbox-light1))
     (term-color-yellow                         (:foreground gruvbox-bright_yellow :background gruvbox-bright_yellow))
     (term-default-fg-color                     (:foreground gruvbox-light0))
     (term-default-bg-color                     (:background gruvbox-bg))

     ;;; message-mode

     (message-header-to                         (:inherit 'font-lock-variable-name-face))
     (message-header-cc                         (:inherit 'font-lock-variable-name-face))
     (message-header-subject                    (:foreground gruvbox-bright_orange :weight 'bold))
     (message-header-newsgroups                 (:foreground gruvbox-bright_yellow :weight 'bold))
     (message-header-other                      (:inherit 'font-lock-variable-name-face))
     (message-header-name                       (:inherit 'font-lock-keyword-face))
     (message-header-xheader                    (:foreground gruvbox-faded_blue))
     (message-separator                         (:inherit 'font-lock-comment-face))
     (message-cited-text                        (:inherit 'font-lock-comment-face))
     (message-mml                               (:foreground gruvbox-faded_green :weight 'bold))

     ;;; org-mode

     (org-hide                                  (:foreground gruvbox-dark0))
     (org-level-1                               (:foreground gruvbox-bright_blue))
     (org-level-2                               (:foreground gruvbox-bright_yellow))
     (org-level-3                               (:foreground gruvbox-bright_purple))
     (org-level-4                               (:foreground gruvbox-bright_red))
     (org-level-5                               (:foreground gruvbox-bright_green))
     (org-level-6                               (:foreground gruvbox-bright_aqua))
     (org-level-7                               (:foreground gruvbox-faded_blue))
     (org-level-8                               (:foreground gruvbox-bright_orange))
     (org-special-keyword                       (:inherit 'font-lock-comment-face))
     (org-drawer                                (:inherit 'font-lock-function-name-face))
     (org-column                                (:background gruvbox-dark0))
     (org-column-title                          (:background gruvbox-dark0 :underline t :weight 'bold))
     (org-warning                               (:foreground gruvbox-bright_red :weight 'bold :underline nil :bold t))
     (org-archived                              (:foreground gruvbox-light0 :weight 'bold))
     (org-link                                  (:foreground gruvbox-faded_aqua :underline t))
     (org-footnote                              (:foreground gruvbox-bright_aqua :underline t))
     (org-ellipsis                              (:foreground gruvbox-light4))
     (org-date                                  (:foreground gruvbox-bright_blue :underline t))
     (org-sexp-date                             (:foreground gruvbox-faded_blue :underline t))
     (org-tag                                   (:bold t :weight 'bold))
     (org-list-dt                               (:bold t :weight 'bold))
     (org-todo                                  (:foreground gruvbox-bright_red :weight 'bold :bold t))
     (org-done                                  (:foreground gruvbox-bright_aqua :weight 'bold :bold t))
     (org-agenda-done                           (:foreground gruvbox-bright_aqua))
     (org-headline-done                         (:foreground gruvbox-bright_aqua))
     (org-table                                 (:foreground gruvbox-bright_blue))
     (org-block                                 (:background gruvbox-dark0_soft))
     (org-block-begin-line                      (:background gruvbox-dark1))
     (org-block-end-line                        (:background gruvbox-dark1))
     (org-formula                               (:foreground gruvbox-bright_yellow))
     (org-document-title                        (:foreground gruvbox-faded_blue))
     (org-document-info                         (:foreground gruvbox-faded_blue))
     (org-agenda-structure                      (:inherit 'font-lock-comment-face))
     (org-agenda-date-today                     (:foreground gruvbox-light0 :weight 'bold :italic t))
     (org-scheduled                             (:foreground gruvbox-bright_yellow))
     (org-scheduled-today                       (:foreground gruvbox-bright_blue))
     (org-scheduled-previously                  (:foreground gruvbox-faded_red))
     (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
     (org-deadline-announce                     (:foreground gruvbox-faded_red))
     (org-time-grid                             (:foreground gruvbox-faded_orange))
     (org-latex-and-related                     (:foreground gruvbox-bright_blue))

     ;;; org-habit

     (org-habit-clear-face                      (:background gruvbox-faded_blue))
     (org-habit-clear-future-face               (:background gruvbox-bright_blue))
     (org-habit-ready-face                      (:background gruvbox-faded_green))
     (org-habit-ready-future-face               (:background gruvbox-bright_green))
     (org-habit-alert-face                      (:background gruvbox-faded_yellow))
     (org-habit-alert-future-face               (:background gruvbox-bright_yellow))
     (org-habit-overdue-face                    (:background gruvbox-faded_red))
     (org-habit-overdue-future-face             (:background gruvbox-bright_red))

      ;; elfeed

     (elfeed-search-title-face                  (:foreground gruvbox-gray))
     (elfeed-search-unread-title-face           (:foreground gruvbox-light1 :weight 'bold))
     (elfeed-search-tag-face                    (:foreground gruvbox-burlywood4))
     (elfeed-search-date-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
     (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
     (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

     ;;; smart-mode-line

     (sml/global                                (:foreground gruvbox-light4 :inverse-video nil))
     (sml/modes                                 (:foreground gruvbox-bright_green))
     (sml/filename                              (:foreground gruvbox-bright_red :weight 'bold))
     (sml/prefix                                (:foreground gruvbox-light1))
     (sml/read-only                             (:foreground gruvbox-bright_blue))
     (persp-selected-face                       (:foreground gruvbox-bright_orange))

     ;;; powerline

     (powerline-active0                         (:background gruvbox-dark4 :foreground gruvbox-light0))
     (powerline-active1                         (:background gruvbox-dark3 :foreground gruvbox-light0))
     (powerline-active2                         (:background gruvbox-dark2 :foreground gruvbox-light0))
     (powerline-inactive0                       (:background gruvbox-dark2 :foreground gruvbox-light4))
     (powerline-inactive1                       (:background gruvbox-dark1 :foreground gruvbox-light4))
     (powerline-inactive2                       (:background gruvbox-dark0 :foreground gruvbox-light4))

     ;;; isearch

     (isearch                                   (:foreground gruvbox-bg :background gruvbox-bright_orange))
     (lazy-highlight                            (:foreground gruvbox-bg :background gruvbox-bright_yellow))
     (isearch-fail                              (:foreground gruvbox-bg :background gruvbox-bright_red))

     ;;; markdown-mode

     (markdown-header-face-1                    (:foreground gruvbox-bright_blue))
     (markdown-header-face-2                    (:foreground gruvbox-bright_yellow))
     (markdown-header-face-3                    (:foreground gruvbox-bright_purple))
     (markdown-header-face-4                    (:foreground gruvbox-bright_red))
     (markdown-header-face-5                    (:foreground gruvbox-bright_green))
     (markdown-header-face-6                    (:foreground gruvbox-bright_aqua))

     ;;; anzu-mode

     (anzu-mode-line                            (:foreground gruvbox-bright_yellow :weight 'bold))
     (anzu-match-1                              (:background gruvbox-bright_green))
     (anzu-match-2                              (:background gruvbox-faded_yellow))
     (anzu-match-3                              (:background gruvbox-aquamarine4))
     (anzu-replace-to                           (:foreground gruvbox-bright_yellow))
     (anzu-replace-highlight                    (:inherit 'isearch))

     ;;; ace-jump-mode

     (ace-jump-face-background                  (:foreground gruvbox-light4 :background gruvbox-bg :inverse-video nil))
     (ace-jump-face-foreground                  (:foreground gruvbox-bright_red :background gruvbox-bg :inverse-video nil))

     ;;; ace-window

     (aw-background-face                        (:foreground gruvbox-light1 :background gruvbox-bg :inverse-video nil))
     (aw-leading-char-face                      (:foreground gruvbox-bright_red :background gruvbox-bg :height 4.0))

     ;;; show-paren

     (show-paren-match                          (:background gruvbox-dark3 :foreground gruvbox-bright_blue  :weight 'bold))
     (show-paren-mismatch                       (:background gruvbox-bright_red :foreground gruvbox-dark3 :weight 'bold))

     ;;; ivy

     (ivy-current-match                         (:foreground gruvbox-light0_hard :weight 'bold :underline t))
     (ivy-minibuffer-match-face-1               (:foreground gruvbox-bright_orange))
     (ivy-minibuffer-match-face-2               (:foreground gruvbox-bright_yellow))
     (ivy-minibuffer-match-face-3               (:foreground gruvbox-faded_orange))
     (ivy-minibuffer-match-face-4               (:foreground gruvbox-faded_yellow))

     ;;; ido

     (ido-only-match                            (:inherit 'success))
     (ido-first-match                           (:foreground gruvbox-light0_hard :weight 'bold :underline t))
     (ido-subdir                                (:inherit 'dired-directory))

     ;;; orderless

     (orderless-match-face-0                    (:foreground gruvbox-bright_yellow))
     (orderless-match-face-1                    (:foreground gruvbox-bright_orange))
     (orderless-match-face-2                    (:foreground gruvbox-bright_blue))
     (orderless-match-face-3                    (:foreground gruvbox-bright_purple))

     ;;; magit

     (magit-bisect-bad                          (:foreground gruvbox-faded_red))
     (magit-bisect-good                         (:foreground gruvbox-faded_green))
     (magit-bisect-skip                         (:foreground gruvbox-faded_yellow))
     (magit-blame-heading                       (:foreground gruvbox-light0 :background gruvbox-dark2))
     (magit-branch-local                        (:foreground gruvbox-bright_blue))
     (magit-branch-current                      (:underline gruvbox-bright_blue :inherit 'magit-branch-local))
     (magit-branch-remote                       (:foreground gruvbox-bright_green))
     (magit-cherry-equivalent                   (:foreground gruvbox-bright_purple))
     (magit-cherry-unmatched                    (:foreground gruvbox-bright_aqua))
     (magit-diff-added                          (:foreground gruvbox-bright_green))
     (magit-diff-added-highlight                (:foreground gruvbox-bright_green :inherit 'magit-diff-context-highlight))
     (magit-diff-base                           (:background gruvbox-faded_yellow :foreground gruvbox-light2))
     (magit-diff-base-highlight                 (:background gruvbox-faded_yellow :foreground gruvbox-light0))
     (magit-diff-context                        (:foreground gruvbox-dark1  :foreground gruvbox-light1))
     (magit-diff-context-highlight              (:background gruvbox-dark1 :foreground gruvbox-light0))
     (magit-diff-hunk-heading                   (:background gruvbox-dark3 :foreground gruvbox-light2))
     (magit-diff-hunk-heading-highlight         (:background gruvbox-dark2 :foreground gruvbox-light0))
     (magit-diff-hunk-heading-selection         (:background gruvbox-dark2 :foreground gruvbox-bright_orange))
     (magit-diff-lines-heading                  (:background gruvbox-faded_orange :foreground gruvbox-light0))
     (magit-diff-removed                        (:foreground gruvbox-bright_red))
     (magit-diff-removed-highlight              (:foreground gruvbox-bright_red :inherit 'magit-diff-context-highlight))
     (magit-diffstat-added                      (:foreground gruvbox-faded_green))
     (magit-diffstat-removed                    (:foreground gruvbox-faded_red))
     (magit-dimmed                              (:foreground gruvbox-dark4))
     (magit-hash                                (:foreground gruvbox-bright_blue))
     (magit-log-author                          (:foreground gruvbox-bright_red))
     (magit-log-date                            (:foreground gruvbox-bright_aqua))
     (magit-log-graph                           (:foreground gruvbox-dark4))
     (magit-process-ng                          (:foreground gruvbox-bright_red :weight 'bold))
     (magit-process-ok                          (:foreground gruvbox-bright_green :weight 'bold))
     (magit-reflog-amend                        (:foreground gruvbox-bright_purple))
     (magit-reflog-checkout                     (:foreground gruvbox-bright_blue))
     (magit-reflog-cherry-pick                  (:foreground gruvbox-bright_green))
     (magit-reflog-commit                       (:foreground gruvbox-bright_green))
     (magit-reflog-merge                        (:foreground gruvbox-bright_green))
     (magit-reflog-other                        (:foreground gruvbox-bright_aqua))
     (magit-reflog-rebase                       (:foreground gruvbox-bright_purple))
     (magit-reflog-remote                       (:foreground gruvbox-bright_blue))
     (magit-reflog-reset                        (:foreground gruvbox-bright_red))
     (magit-refname                             (:foreground gruvbox-light4))
     (magit-section-heading                     (:foreground gruvbox-bright_yellow :weight 'bold))
     (magit-section-heading-selection           (:foreground gruvbox-faded_yellow))
     (magit-section-highlight                   (:background gruvbox-dark1))
     (magit-sequence-drop                       (:foreground gruvbox-faded_yellow))
     (magit-sequence-head                       (:foreground gruvbox-bright_aqua))
     (magit-sequence-part                       (:foreground gruvbox-bright_yellow))
     (magit-sequence-stop                       (:foreground gruvbox-bright_green))
     (magit-signature-bad                       (:foreground gruvbox-bright_red :weight 'bold))
     (magit-signature-error                     (:foreground gruvbox-bright_red))
     (magit-signature-expired                   (:foreground gruvbox-bright_orange))
     (magit-signature-good                      (:foreground gruvbox-bright_green))
     (magit-signature-revoked                   (:foreground gruvbox-bright_purple))
     (magit-signature-untrusted                 (:foreground gruvbox-bright_blue))
     (magit-tag                                 (:foreground gruvbox-bright_yellow))

     ;;; cider

     (cider-debug-code-overlay-face             (:background gruvbox-dark2 :foreground gruvbox-light0))
     (cider-deprecated-face                     (:background gruvbox-dark2 :foreground gruvbox-bright_orange))
     (cider-enlightened-local-face              (:foreground gruvbox-bright_orange :weight 'bold))
     (cider-error-highlight-face                (:foreground gruvbox-bright_red :underline t :style 'wave))
     (cider-fringe-good-face                    (:foreground gruvbox-neutral_green))
     (cider-instrumented-face                   (:background gruvbox-dark1 :box (:line-width -1 :color gruvbox-bright_red)))
     (cider-result-overlay-face                 (:background gruvbox-dark2 :box (:line-width -1 :color gruvbox-bright_yellow)))
     (cider-test-error-face                     (:background gruvbox-faded_red))
     (cider-test-error-face                     (:background gruvbox-neutral_orange))
     (cider-test-success-face                   (:background gruvbox-bright_green))
     (cider-traced                              (:background gruvbox-bright_aqua))
     (cider-warning-highlight-face              (:foreground gruvbox-bright_yellow :underline t :style 'wave))

     ;;; git-gutter

     (git-gutter:modified                       (:background gruvbox-faded_blue :foreground gruvbox-faded_blue))
     (git-gutter:added                          (:background gruvbox-faded_green :foreground gruvbox-faded_green))
     (git-gutter:deleted                        (:background gruvbox-faded_red :foreground gruvbox-faded_red))

     ;;; git-gutter+

     (git-gutter+-modified                      (:foreground gruvbox-faded_blue :background gruvbox-faded_blue))
     (git-gutter+-added                         (:foreground gruvbox-faded_green :background gruvbox-faded_green))
     (git-gutter+-deleted                       (:foreground gruvbox-faded_red :background gruvbox-faded_red))

     ;;; git-gutter-fringe

     (git-gutter-fr:modified                    (:inherit 'git-gutter:modified))
     (git-gutter-fr:added                       (:inherit 'git-gutter:added))
     (git-gutter-fr:deleted                     (:inherit 'git-gutter:deleted))

     ;;; diff-hl

     (diff-hl-change (:background gruvbox-faded_blue :foreground gruvbox-faded_blue))
     (diff-hl-delete (:background gruvbox-faded_red :foreground gruvbox-faded_red))
     (diff-hl-insert (:background gruvbox-faded_green :foreground gruvbox-faded_green))

     ;;; flyspell

     (flyspell-duplicate                        (:underline (:color gruvbox-light4 :style 'line)))
     (flyspell-incorrect                        (:underline (:color gruvbox-bright_red :style 'line)))

     ;;; langtool

     (langtool-errline                          (:foreground gruvbox-dark0 :background gruvbox-bright_red))
     (langtool-correction-face                  (:foreground gruvbox-bright_yellow :weight 'bold))

     ;;; latex

     (font-latex-bold-face                      (:foreground gruvbox-faded_green :bold t))
     (font-latex-italic-face                    (:foreground gruvbox-bright_green :underline t))
     (font-latex-math-face                      (:foreground gruvbox-light3))
     (font-latex-script-char-face               (:foreground gruvbox-faded_aqua))
     (font-latex-sectioning-5-face              (:foreground gruvbox-bright_yellow :bold t))
     (font-latex-sedate-face                    (:foreground gruvbox-light4))
     (font-latex-string-face                    (:foreground gruvbox-bright_orange))
     (font-latex-verbatim-face                  (:foreground gruvbox-light4))
     (font-latex-warning-face                   (:foreground gruvbox-bright_red :weight 'bold))
     (preview-face                              (:background gruvbox-dark1))

     ;;; lsp

     (lsp-lsp-flycheck-warning-unnecessary-face (:underline (:color gruvbox-bright_orange :style 'wave)
                                                            :foreground gruvbox-burlywood4))
     (lsp-ui-doc-background                     (:background gruvbox-dark3))
     (lsp-ui-doc-header                         (:background gruvbox-faded_blue))
     (lsp-ui-peek-filename                      (:foreground gruvbox-bright_red))
     (lsp-ui-sideline-code-action               (:foreground gruvbox-bright_yellow))
     (lsp-ui-sideline-current-symbol            (:foreground gruvbox-faded_aqua))
     (lsp-ui-sideline-symbol                    (:foreground gruvbox-gray))

     ;;; mu4e

     (mu4e-header-key-face                      (:foreground gruvbox-bright_green :weight 'bold))
     (mu4e-unread-face                          (:foreground gruvbox-bright_blue :weight 'bold))
     (mu4e-highlight-face                       (:foreground gruvbox-bright_green))

     ;;; shell script

     (sh-quoted-exec                            (:foreground gruvbox-bright_purple))
     (sh-heredoc                                (:foreground gruvbox-bright_orange))

     ;;; undo-tree

     (undo-tree-visualizer-active-branch-face   (:foreground gruvbox-light0))
     (undo-tree-visualizer-current-face         (:foreground gruvbox-bright_red))
     (undo-tree-visualizer-default-face         (:foreground gruvbox-dark4))
     (undo-tree-visualizer-register-face        (:foreground gruvbox-bright_yellow))
     (undo-tree-visualizer-unmodified-face      (:foreground gruvbox-bright_aqua))

     ;;; widget faces

     (widget-button-pressed-face                (:foreground gruvbox-bright_red))
     (widget-documentation-face                 (:foreground gruvbox-faded_green))
     (widget-field                              (:foreground gruvbox-light0 :background gruvbox-dark2))
     (widget-single-line-field                  (:foreground gruvbox-light0 :background gruvbox-dark2))

     ;;; dired+

     (diredp-file-name                          (:foreground gruvbox-light2))
     (diredp-file-suffix                        (:foreground gruvbox-light4))
     (diredp-compressed-file-suffix             (:foreground gruvbox-faded_blue))
     (diredp-dir-name                           (:foreground gruvbox-faded_blue))
     (diredp-dir-heading                        (:foreground gruvbox-bright_blue))
     (diredp-symlink                            (:foreground gruvbox-bright_orange))
     (diredp-date-time                          (:foreground gruvbox-light3))
     (diredp-number                             (:foreground gruvbox-faded_blue))
     (diredp-no-priv                            (:foreground gruvbox-dark4))
     (diredp-other-priv                         (:foreground gruvbox-dark2))
     (diredp-rare-priv                          (:foreground gruvbox-dark4))
     (diredp-ignored-file-name                  (:foreground gruvbox-dark4))

     (diredp-dir-priv                           (:foreground gruvbox-faded_blue  :background gruvbox-dark_blue))
     (diredp-exec-priv                          (:foreground gruvbox-faded_blue  :background gruvbox-dark_blue))
     (diredp-link-priv                          (:foreground gruvbox-faded_aqua  :background gruvbox-dark_aqua))
     (diredp-read-priv                          (:foreground gruvbox-bright_red  :background gruvbox-dark_red))
     (diredp-write-priv                         (:foreground gruvbox-bright_aqua :background gruvbox-dark_aqua))

     ;;; diredfl
     (diredfl-autofile-name                     (:foreground gruvbox-light2))
     (diredfl-compressed-file-name              (:foreground gruvbox-light2))
     (diredfl-compressed-file-suffix            (:foreground gruvbox-faded_blue))
     (diredfl-date-time                         (:foreground gruvbox-bright_aqua))
     (diredfl-deletion                          (:foreground gruvbox-bright_red :bold t))
     (diredfl-deletion-file-name                (:foreground gruvbox-bright_red :bold t))
     (diredfl-dir-heading                       (:foreground gruvbox-bright_blue :bold t))
     (diredfl-dir-name                          (:foreground gruvbox-bright_blue))
     (diredfl-dir-priv                          (:foreground gruvbox-bright_blue :background gruvbox-dark_blue))
     (diredfl-exec-priv                         (:foreground gruvbox-bright_blue :background gruvbox-dark_blue))
     (diredfl-executable-tag                    (:foreground gruvbox-bright_green))
     (diredfl-file-name                         (:foreground gruvbox-light2))
     (diredfl-file-suffix                       (:foreground gruvbox-light4))
     (diredfl-symlink                           (:foreground gruvbox-bright_purple))
     (diredfl-flag-mark                         (:foreground gruvbox-bright_yellow :background gruvbox-dark3))
     (diredfl-flag-mark-line                    (:foreground gruvbox-bright_yellow :background gruvbox-dark2))
     (diredfl-ignored-file-name                 (:foreground gruvbox-dark4))
     (diredfl-link-priv                         (:foreground gruvbox-faded_purple))
     (diredfl-no-priv                           (:foreground gruvbox-light2))
     (diredfl-number                            (:foreground gruvbox-bright_yellow))
     (diredfl-other-priv                        (:foreground gruvbox-bright_purple))
     (diredfl-rare-priv                         (:foreground gruvbox-light2))
     (diredfl-read-priv                         (:foreground gruvbox-bright_yellow))
     (diredfl-write-priv                        (:foreground gruvbox-bright_red))
     (diredfl-tagged-autofile-name              (:foreground gruvbox-light4))

     ;;; neotree

     (neo-banner-face                           (:foreground gruvbox-bright_purple :bold t))
     (neo-dir-link-face                         (:foreground gruvbox-bright_yellow))
     (neo-expand-btn-face                       (:foreground gruvbox-bright_orange))
     (neo-file-link-face                        (:foreground gruvbox-light0))
     (neo-header-face                           (:foreground gruvbox-bright_purple))
     (neo-root-dir-face                         (:foreground gruvbox-bright_purple :bold t))

     ;;; eshell

     (eshell-prompt                              (:foreground gruvbox-bright_aqua))
     (eshell-ls-archive                          (:foreground gruvbox-light3))
     (eshell-ls-backup                           (:foreground gruvbox-light4))
     (eshell-ls-clutter                          (:foreground gruvbox-bright_orange :weight 'bold))
     (eshell-ls-directory                        (:foreground gruvbox-bright_yellow))
     (eshell-ls-executable                       (:weight 'bold))
     (eshell-ls-missing                          (:foreground gruvbox-bright_red :bold t))
     (eshell-ls-product                          (:foreground gruvbox-faded_red))
     (eshell-ls-readonly                         (:foreground gruvbox-light2))
     (eshell-ls-special                          (:foreground gruvbox-bright_yellow :bold t))
     (eshell-ls-symlink                          (:foreground gruvbox-bright_red))
     (eshell-ls-unreadable                       (:foreground gruvbox-bright_red :bold t))

     ;;; tabbar

     (tabbar-default                             (:foreground gruvbox-light0 :background gruvbox-dark3 :bold nil :height 1.0 :box (:line-width -5 :color gruvbox-dark3)))
     (tabbar-separator                           (:foreground gruvbox-light0 :background gruvbox-dark3))
     (tabbar-highlight                           (:inherit 'highlight))
     (tabbar-button                              (:foreground gruvbox-dark3 :background gruvbox-dark3 :box nil :line-width 0))
     (tabbar-button-highlight                    (:inherit 'tabbar-button :inverse-video t))
     (tabbar-modified                            (:foreground gruvbox-bright_green :background gruvbox-dark3 :box (:line-width -5 :color gruvbox-dark3)))
     (tabbar-unselected                          (:inherit 'tabbar-default))
     (tabbar-unselected-modified                 (:inherit 'tabbar-modified))
     (tabbar-selected                            (:inherit 'tabbar-default :foreground gruvbox-bright_yellow))
     (tabbar-selected-modified                   (:inherit 'tabbar-selected))

     ;;; wgrep

     (wgrep-delete-face                          (:strike-through gruvbox-bright_red))
     (wgrep-done-face                            (:foreground gruvbox-turquoise4))
     (wgrep-face                                 (:underline (:color gruvbox-bright_yellow :style 'line)))
     (wgrep-file-face                            (:inherit 'highlight))
     (wgrep-reject-face                          (:foreground gruvbox-bright_red :bold t))

     ;;; hydra

     (hydra-face-red (:foreground gruvbox-bright_red :weight 'bold))
     (hydra-face-blue (:foreground gruvbox-bright_blue :weight 'bold))
     (hydra-face-amaranth (:foreground gruvbox-bright_yellow :weight 'bold))
     (hydra-face-pink (:foreground gruvbox-bright_purple :weight 'bold))
     (hydra-face-teal (:foreground gruvbox-bright_aqua :weight 'bold))

     ;;; which-function-mode

     (which-func                                 (:foreground gruvbox-faded_blue))

     ;;; auto-dim-other-buffers

     (auto-dim-other-buffers-face                (:background gruvbox-bg_inactive))

     ;;; flycheck

     (flycheck-warning                          (:underline (:style 'wave :color gruvbox-bright_yellow)))
     (flycheck-error                            (:underline (:style 'wave :color gruvbox-bright_red)))
     (flycheck-info                             (:underline (:style 'wave :color gruvbox-bright_blue)))
     (flycheck-fringe-warning                   (:foreground gruvbox-bright_yellow))
     (flycheck-fringe-error                     (:foreground gruvbox-bright_red))
     (flycheck-fringe-info                      (:foreground gruvbox-bright_blue))
     (flycheck-error-list-warning               (:foreground gruvbox-bright_yellow :bold t))
     (flycheck-error-list-error                 (:foreground gruvbox-bright_red :bold t))
     (flycheck-error-list-info                  (:foreground gruvbox-bright_blue :bold t))

     ;;; tab-bar

     (tab-bar-tab-inactive (:background gruvbox-bg :foreground gruvbox-light0))
     (tab-bar-tab (:background gruvbox-dark2 :foreground gruvbox-light0))
     (tab-bar (:background gruvbox-bg :foreground gruvbox-light0))

     ;;; circe

     (circe-prompt-face               (:foreground gruvbox-turquoise4))
     (circe-fool                      (:foreground gruvbox-dark2))
     (circe-highlight-nick-face       (:foreground gruvbox-bright_yellow))
     (circe-server-face               (:foreground gruvbox-dark4))
     (circe-my-message-face           (:foreground gruvbox-bright_aqua))
     (lui-time-stamp-face             (:foreground gruvbox-bright_blue))

     ;;; erc

     (erc-action-face            (:inherit 'erc-default-face))
     (erc-bold-face              (:weight 'bold))
     (erc-current-nick-face      (:foreground gruvbox-aquamarine4))
     (erc-dangerous-host-face    (:inherit 'font-lock-warning-face))
     (erc-default-face           (:inherit 'default))
     (erc-direct-msg-face        (:inherit 'erc-default-face))
     (erc-error-face             (:inherit 'font-lock-warning-face))
     (erc-fool-face              (:inherit 'erc-default-face))
     (erc-input-face             (:foreground gruvbox-turquoise4))
     (erc-my-nick-face           (:foreground gruvbox-turquoise4))
     (erc-nick-msg-face          (:inherit 'erc-default-face))
     (erc-notice-face            (:foreground gruvbox-dark4))
     (erc-timestamp-face         (:foreground gruvbox-neutral_green))
     (erc-underline-face         (:underline t))
     (erc-prompt-face            (:foreground gruvbox-turquoise4))
     (erc-pal-face               (:foreground gruvbox-neutral_yellow :weight 'bold))
     (erc-keyword-face           (:foreground gruvbox-bright_orange :weight 'bold))
     (erc-nick-default-face      (:weight 'regular))
     (erc-button                 (:weight 'bold  :underline t))

     ;;; gnus

     (gnus-group-mail-1           (:weight 'bold :foreground gruvbox-light0))
     (gnus-group-mail-2           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-3           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-1-empty     (:foreground gruvbox-dark4))
     (gnus-group-mail-2-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-mail-3-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-1           (:inherit 'gnus-group-mail-1))
     (gnus-group-news-2           (:inherit 'gnus-group-news-1))
     (gnus-group-news-3           (:inherit 'gnus-group-news-1))
     (gnus-group-news-4           (:inherit 'gnus-group-news-1))
     (gnus-group-news-5           (:inherit 'gnus-group-news-1))
     (gnus-group-news-6           (:inherit 'gnus-group-news-1))
     (gnus-group-news-1-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-2-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-3-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-4-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-5-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-6-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-mail-low         (:inherit 'gnus-group-mail-1 :weight 'normal))
     (gnus-group-mail-low-empty   (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-low         (:inherit 'gnus-group-mail-1 :foreground gruvbox-dark4))
     (gnus-group-news-low-empty   (:inherit 'gnus-group-news-low :weight 'normal))
     (gnus-header-content         (:inherit 'message-header-other))
     (gnus-header-from            (:inherit 'message-header-other))
     (gnus-header-name            (:inherit 'message-header-name))
     (gnus-header-newsgroups      (:inherit 'message-header-other))
     (gnus-header-subject         (:inherit 'message-header-subject))
     (gnus-summary-cancelled      (:foreground gruvbox-bright_red :strike-through t))
     (gnus-summary-normal-ancient (:foreground gruvbox-dark4 :inherit 'italic))
     (gnus-summary-normal-read    (:foreground gruvbox-light0))
     (gnus-summary-normal-ticked  (:foreground gruvbox-bright_purple))
     (gnus-summary-normal-unread  (:foreground gruvbox-bright_green :inherit 'bold))
     (gnus-summary-selected       (:foreground gruvbox-bright_blue :weight 'bold))

     (gnus-cite-1                 (:foreground gruvbox-accent-00))
     (gnus-cite-2                 (:foreground gruvbox-accent-01))
     (gnus-cite-3                 (:foreground gruvbox-accent-02))
     (gnus-cite-4                 (:foreground gruvbox-accent-03))
     (gnus-cite-5                 (:foreground gruvbox-accent-04))
     (gnus-cite-6                 (:foreground gruvbox-accent-05))
     (gnus-cite-7                 (:foreground gruvbox-accent-06))
     (gnus-cite-8                 (:foreground gruvbox-accent-07))
     (gnus-cite-9                 (:foreground gruvbox-accent-08))
     (gnus-cite-10                (:foreground gruvbox-accent-09))
     (gnus-cite-11                (:foreground gruvbox-accent-10))

     (gnus-signature              (:foreground gruvbox-faded_orange))
     (gnus-x-face                 (:background gruvbox-dark4 :foreground gruvbox-light0))

     ;;; web-mode

     (web-mode-doctype-face          (:foreground gruvbox-bright_blue))
     (web-mode-html-tag-bracket-face (:foreground gruvbox-bright_blue))
     (web-mode-html-tag-face         (:foreground gruvbox-bright_blue))
     (web-mode-html-attr-name-face   (:foreground gruvbox-bright_yellow))
     (web-mode-html-attr-equal-face  (:foreground gruvbox-bright_yellow))
     (web-mode-html-attr-value-face  (:foreground gruvbox-bright_green))

     ;;; Coq

     (coq-solve-tactics-face      (:inherit 'font-lock-constant-face))
     (coq-cheat-face              (:box (:line-width -1 :color gruvbox-bright_red :style nil)
                                   :foreground gruvbox-bright_red))
     (coq-button-face             (:background gruvbox-bg_inactive))
     (coq-button-face-active      (:background gruvbox-dark1))
     (coq-button-face-pressed     (:background gruvbox-bg_inactive))

     ;;; Proof General

     (proof-active-area-face      (:underline t))
     (proof-tacticals-name-face   (:inherit 'font-lock-constant-face))
     (proof-tactics-name-face     (:inherit 'font-lock-constant-face))
     (proof-locked-face           (:background gruvbox-dark1))
     (proof-queue-face            (:background gruvbox-dark2))
     (proof-warning-face          (:background gruvbox-dark_red))
     (proof-error-face            (:background gruvbox-bg :foreground gruvbox-faded_red))

     ;;; ledger-mode

     (ledger-font-xact-highlight-face  (:background gruvbox-dark1))

     ;;; Solaire

     (solaire-default-face        (:background gruvbox-dark0_soft))
     (solaire-minibuffer-face     (:background gruvbox-dark0_soft))
     (solaire-hl-line-face        (:background gruvbox-dark0_soft))
     (solaire-org-hide-face       (:background gruvbox-dark0_soft))

     ;;; Vertico posframe

     (vertico-posframe            (:background gruvbox-dark0_hard))
     (vertico-posframe-border     (:background gruvbox-dark0_hard))
     (vertico-posframe-border-2   (:background gruvbox-dark0))
     (vertico-posframe-border-3   (:background gruvbox-dark1))
     (vertico-posframe-border-4   (:background gruvbox-dark2))

     ;;; avy

     (avy-background-face         (:foreground gruvbox-gray))
     (avy-lead-face               (:foreground gruvbox-dark0 :background gruvbox-neutral_red))
     (avy-lead-face-0             (:foreground gruvbox-dark0 :background gruvbox-neutral_blue))
     (avy-lead-face-1             (:foreground gruvbox-dark0 :background gruvbox-neutral_aqua))
     (avy-lead-face-2             (:foreground gruvbox-dark0 :background gruvbox-neutral_purple))

     ;;; ansi-color <built-in>

     (ansi-color-black          (:foreground gruvbox-dark2 :background gruvbox-dark1))
     (ansi-color-red            (:foreground gruvbox-bright_red :background gruvbox-bright_red))
     (ansi-color-green          (:foreground gruvbox-bright_green :background gruvbox-bright_green))
     (ansi-color-yellow         (:foreground gruvbox-bright_yellow :background gruvbox-bright_yellow))
     (ansi-color-blue           (:foreground gruvbox-bright_blue :background gruvbox-bright_blue))
     (ansi-color-magenta        (:foreground gruvbox-bright_purple :background gruvbox-bright_purple))
     (ansi-color-cyan           (:foreground gruvbox-bright_aqua :background gruvbox-bright_aqua))
     (ansi-color-white          (:foreground gruvbox-light1 :background gruvbox-light1))
     (ansi-color-bright-black   (:foreground gruvbox-dark2 :background gruvbox-dark1))
     (ansi-color-bright-red     (:foreground gruvbox-bright_red :background gruvbox-bright_red))
     (ansi-color-bright-green   (:foreground gruvbox-bright_green :background gruvbox-bright_green))
     (ansi-color-bright-yellow  (:foreground gruvbox-bright_yellow :background gruvbox-bright_yellow))
     (ansi-color-bright-blue    (:foreground gruvbox-bright_blue :background gruvbox-bright_blue))
     (ansi-color-bright-magenta (:foreground gruvbox-bright_purple :background gruvbox-bright_purple))
     (ansi-color-bright-cyan    (:foreground gruvbox-bright_aqua :background gruvbox-bright_aqua))
     (ansi-color-bright-white   (:foreground gruvbox-light1 :background gruvbox-light1))

     ;;; Elpaca

     (elpaca-finished           (:foreground gruvbox-bright_green :bold t))
     (elpaca-blocked            (:foreground gruvbox-bright_yellow :bold t))
     (elpaca-busy               (:foreground gruvbox-bright_orange :bold t))
     (elpaca-failed             (:foreground gruvbox-bright_red :bold t))

     (elpaca-ui-marked-delete   (:foreground gruvbox-bright_red :bold t))
     (elpaca-ui-marked-fetch    (:foreground gruvbox-bright_aqua :bold t))
     (elpaca-ui-marked-install  (:foreground gruvbox-bright_blue :bold t))
     (elpaca-ui-marked-rebuild  (:foreground gruvbox-bright_purple :bold t))
     (elpaca-ui-marked-update   (:foreground gruvbox-bright_orange :bold t))

     ) ;;; END

    ,@body))

(provide 'gruvbox)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox.el ends here
