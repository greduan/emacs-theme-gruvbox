;;; gruvbox-dark-theme.el --- A retro-groove colour theme for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Eduardo Lavaque
;; Copyright (c) 2016-2017 Jason Milkins
;; Copyright (c) 2017 Martijn Terpstra

;; Author: Jason Milkins <jasonm23@gmail.com>
;; (current maintainer)
;;
;; Author-list: Lee Machin <ljmachin@gmail.com>,
;;              Eduardo Lavaque <me@greduan.com>
;;
;; URL: http://github.com/greduan/emacs-theme-gruvbox
;; Version: 1.26.0

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
    ((default                                   (:background gruvbox-bg :foreground gruvbox-light0))
     (cursor                                    (:background gruvbox-light0))
     (mode-line                                 (:background gruvbox-dark3 :foreground gruvbox-light2 :box nil))
     (mode-line-inactive                        (:background gruvbox-dark1 :foreground gruvbox-light4 :box nil))
     (fringe                                    (:background gruvbox-bg))
     (hl-line                                   (:background gruvbox-dark1))
     (region                                    (:background gruvbox-dark2)) ;;selection
     (secondary-selection                       (:background gruvbox-dark1))
     (minibuffer-prompt                         (:background gruvbox-bg :foreground gruvbox-bright_green :bold t))
     (vertical-border                           (:foreground gruvbox-dark2))
     (window-divider                            (:foreground gruvbox-dark2))
     (link                                      (:foreground gruvbox-faded_blue :underline t))
     (shadow                                    (:foreground gruvbox-dark4))

     ;; Built-in syntax

     (font-lock-builtin-face                            (:foreground gruvbox-bright_orange))
     (font-lock-constant-face                           (:foreground gruvbox-bright_purple))
     (font-lock-comment-face                            (:foreground gruvbox-dark4))
     (font-lock-function-name-face                      (:foreground gruvbox-bright_yellow))
     (font-lock-keyword-face                            (:foreground gruvbox-bright_red))
     (font-lock-string-face                             (:foreground gruvbox-bright_green))
     (font-lock-variable-name-face                      (:foreground gruvbox-bright_blue))
     (font-lock-type-face                               (:foreground gruvbox-bright_purple))
     (font-lock-warning-face                            (:foreground gruvbox-bright_red :bold t))

     ;; Basic faces
     (error                                             (:foreground gruvbox-bright_red :bold t))
     (success                                           (:foreground gruvbox-bright_green :bold t))
     (warning                                           (:foreground gruvbox-bright_yellow :bold t))
     (trailing-whitespace                               (:background gruvbox-bright_red))
     (escape-glyph                                      (:foreground gruvbox-bright_aqua))
     (header-line                                       (:background gruvbox-dark0 :foreground gruvbox-light3 :box nil :inherit nil))
     (highlight                                         (:background gruvbox-dark4 :foreground gruvbox-light0))
     (homoglyph                                         (:foreground gruvbox-bright_yellow))
     (match                                             (:foreground gruvbox-dark0 :background gruvbox-bright_blue))

     ;; Customize faces
     (widget-field                                      (:background gruvbox-dark3))
     (custom-group-tag                                  (:foreground gruvbox-bright_blue :weight 'bold))
     (custom-variable-tag                               (:foreground gruvbox-bright_blue :weight 'bold))

     ;; whitespace-mode

     (whitespace-space                          (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-hspace                         (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-tab                            (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-newline                        (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-trailing                       (:background gruvbox-dark1 :foreground gruvbox-bright_red))
     (whitespace-line                           (:background gruvbox-dark1 :foreground gruvbox-bright_red))
     (whitespace-space-before-tab               (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-indentation                    (:background gruvbox-bg :foreground gruvbox-dark4))
     (whitespace-empty                          (:background nil :foreground nil))
     (whitespace-space-after-tab                (:background gruvbox-bg :foreground gruvbox-dark4))

     ;; RainbowDelimiters

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
     (rainbow-delimiters-unmatched-face         (:background nil :foreground gruvbox-light0))


     ;; line numbers
     (line-number                               (:foreground gruvbox-dark4 :background gruvbox-dark1))
     (line-number-current-line                  (:foreground gruvbox-bright_orange :background gruvbox-dark2))
     (linum                                     (:foreground gruvbox-dark4 :background gruvbox-dark1))
     (linum-highlight-face                      (:foreground gruvbox-bright_orange :background gruvbox-dark2))
     (linum-relative-current-face               (:foreground gruvbox-bright_orange :background gruvbox-dark2))

     ;; Highlight indentation mode
     (highlight-indentation-current-column-face (:background gruvbox-dark2))
     (highlight-indentation-face                (:background gruvbox-dark1))

     ;; smartparens
     (sp-pair-overlay-face                      (:background gruvbox-dark2))
     (sp-show-pair-match-face                   (:background gruvbox-dark2)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face                (:background gruvbox-bright_red)) ;; Highlight for bracket without pair
     ;;(sp-wrap-overlay-face                     (:inherit 'sp-wrap-overlay-face))
     ;;(sp-wrap-tag-overlay-face                 (:inherit 'sp-wrap-overlay-face))

     ;; elscreen
     (elscreen-tab-background-face              (:background gruvbox-bg :box nil)) ;; Tab bar, not the tabs
     (elscreen-tab-control-face                 (:background gruvbox-dark2 :foreground gruvbox-bright_red :underline nil :box nil)) ;; The controls
     (elscreen-tab-current-screen-face          (:background gruvbox-dark4 :foreground gruvbox-dark0 :box nil)) ;; Current tab
     (elscreen-tab-other-screen-face            (:background gruvbox-dark2 :foreground gruvbox-light4 :underline nil :box nil)) ;; Inactive tab

     ;; ag (The Silver Searcher)
     (ag-hit-face                               (:foreground gruvbox-bright_blue))
     (ag-match-face                             (:foreground gruvbox-bright_red))

     ;; Diffs
     (diff-changed                              (:background nil :foreground gruvbox-light1))
     (diff-added                                (:background nil :foreground gruvbox-bright_green))
     (diff-removed                              (:background nil :foreground gruvbox-bright_red))
     (diff-indicator-changed                    (:inherit 'diff-changed))
     (diff-indicator-added                      (:inherit 'diff-added))
     (diff-indicator-removed                    (:inherit 'diff-removed))

     (js2-warning                               (:underline (:color gruvbox-bright_yellow :style 'wave)))
     (js2-error                                 (:underline (:color gruvbox-bright_red :style 'wave)))
     (js2-external-variable                     (:underline (:color gruvbox-bright_aqua :style 'wave)))
     (js2-jsdoc-tag                             (:background nil :foreground gruvbox-gray  ))
     (js2-jsdoc-type                            (:background nil :foreground gruvbox-light4))
     (js2-jsdoc-value                           (:background nil :foreground gruvbox-light3))
     (js2-function-param                        (:background nil :foreground gruvbox-bright_aqua))
     (js2-function-call                         (:background nil :foreground gruvbox-bright_blue))
     (js2-instance-member                       (:background nil :foreground gruvbox-bright_orange))
     (js2-private-member                        (:background nil :foreground gruvbox-faded_yellow))
     (js2-private-function-call                 (:background nil :foreground gruvbox-faded_aqua))
     (js2-jsdoc-html-tag-name                   (:background nil :foreground gruvbox-light4))
     (js2-jsdoc-html-tag-delimiter              (:background nil :foreground gruvbox-light3))

     ;; popup
     (popup-face                                (:underline nil :foreground gruvbox-light1 :background gruvbox-dark1))
     (popup-menu-mouse-face                     (:underline nil :foreground gruvbox-light0 :background gruvbox-faded_green))
     (popup-menu-selection-face                 (:underline nil :foreground gruvbox-light0 :background gruvbox-faded_green))
     (popup-tip-face                            (:underline nil :foreground gruvbox-light2 :background gruvbox-dark2))

     ;; helm
     (helm-M-x-key                              (:foreground gruvbox-bright_orange ))
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

     ;;hi-lock-mode
     (hi-black-b                                (:foreground gruvbox-black :weight 'bold))
     (hi-black-hb                               (:foreground gruvbox-black :weight 'bold :height 1.5))
     (hi-blue                                   (:foreground gruvbox-dark0 :background gruvbox-bright_blue))
     (hi-blue-b                                 (:foreground gruvbox-bright_blue :weight 'bold))
     (hi-green                                  (:foreground gruvbox-dark0 :background gruvbox-bright_green))
     (hi-green-b                                (:foreground gruvbox-bright_green :weight 'bold))
     (hi-pink                                   (:foreground gruvbox-dark0 :background gruvbox-bright_purple))
     (hi-red-b                                  (:foreground gruvbox-bright_red :weight 'bold))
     (hi-yellow                                 (:foreground gruvbox-dark0 :background gruvbox-faded_yellow))

     ;; company-mode
     (company-scrollbar-bg                      (:background gruvbox-dark1))
     (company-scrollbar-fg                      (:background gruvbox-dark0_soft))
     (company-tooltip                           (:background gruvbox-dark0_soft))
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

     ;; tool tips
     (tooltip                                   (:foreground gruvbox-light1 :background gruvbox-dark1))

     ;; term
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

     ;; message-mode
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

     ;; org-mode
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

     ;; org-habit
     (org-habit-clear-face                      (:background gruvbox-faded_blue))
     (org-habit-clear-future-face               (:background gruvbox-bright_blue))
     (org-habit-ready-face                      (:background gruvbox-faded_green))
     (org-habit-ready-future-face               (:background gruvbox-bright_green))
     (org-habit-alert-face                      (:background gruvbox-faded_yellow))
     (org-habit-alert-future-face               (:background gruvbox-bright_yellow))
     (org-habit-overdue-face                    (:background gruvbox-faded_red))
     (org-habit-overdue-future-face             (:background gruvbox-bright_red))

     ;; elfeed
     (elfeed-search-title-face                  (:foreground gruvbox-gray  ))
     (elfeed-search-unread-title-face           (:foreground gruvbox-light0))
     (elfeed-search-date-face                   (:inherit 'font-lock-builtin-face :underline t))
     (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-tag-face                    (:inherit 'font-lock-keyword-face))
     (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
     (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
     (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

     ;; smart-mode-line
     (sml/global                                (:foreground gruvbox-light4 :inverse-video nil))
     (sml/modes                                 (:foreground gruvbox-bright_green))
     (sml/filename                              (:foreground gruvbox-bright_red :weight 'bold))
     (sml/prefix                                (:foreground gruvbox-light1))
     (sml/read-only                             (:foreground gruvbox-bright_blue))
     (persp-selected-face                       (:foreground gruvbox-bright_orange))

     ;; powerline
     (powerline-active0                         (:background gruvbox-dark4 :foreground gruvbox-light0))
     (powerline-active1                         (:background gruvbox-dark3 :foreground gruvbox-light0))
     (powerline-active2                         (:background gruvbox-dark2 :foreground gruvbox-light0))
     (powerline-inactive0                       (:background gruvbox-dark2 :foreground gruvbox-light4))
     (powerline-inactive1                       (:background gruvbox-dark1 :foreground gruvbox-light4))
     (powerline-inactive2                       (:background gruvbox-dark0 :foreground gruvbox-light4))

     ;; isearch
     (isearch                                   (:foreground gruvbox-black :background gruvbox-bright_orange))
     (lazy-highlight                            (:foreground gruvbox-black :background gruvbox-bright_yellow))
     (isearch-fail                              (:foreground gruvbox-light0 :background gruvbox-bright_red))

     ;; markdown-mode
     (markdown-header-face-1                    (:foreground gruvbox-bright_blue))
     (markdown-header-face-2                    (:foreground gruvbox-bright_yellow))
     (markdown-header-face-3                    (:foreground gruvbox-bright_purple))
     (markdown-header-face-4                    (:foreground gruvbox-bright_red))
     (markdown-header-face-5                    (:foreground gruvbox-bright_green))
     (markdown-header-face-6                    (:foreground gruvbox-bright_aqua))

     ;; anzu-mode
     (anzu-mode-line                            (:foreground gruvbox-bright_yellow :weight 'bold))
     (anzu-match-1                              (:background gruvbox-bright_green))
     (anzu-match-2                              (:background gruvbox-faded_yellow))
     (anzu-match-3                              (:background gruvbox-aquamarine4))
     (anzu-replace-to                           (:foreground gruvbox-bright_yellow))
     (anzu-replace-highlight                    (:inherit 'isearch))

     ;; ace-jump-mode
     (ace-jump-face-background                  (:foreground gruvbox-light4 :background gruvbox-bg :inverse-video nil))
     (ace-jump-face-foreground                  (:foreground gruvbox-bright_red :background gruvbox-bg :inverse-video nil))

     ;; ace-window
     (aw-background-face                        (:forground  gruvbox-light1 :background gruvbox-bg :inverse-video nil))
     (aw-leading-char-face                      (:foreground gruvbox-bright_red :background gruvbox-bg :height 4.0))

     ;; show-paren
     (show-paren-match                          (:background gruvbox-dark3 :foreground gruvbox-bright_blue  :weight 'bold))
     (show-paren-mismatch                       (:background gruvbox-bright_red :foreground gruvbox-dark3 :weight 'bold))

     ;; ivy
     (ivy-current-match                         (:foreground gruvbox-light0_hard :weight 'bold :underline t))
     (ivy-minibuffer-match-face-1               (:foreground gruvbox-bright_orange))
     (ivy-minibuffer-match-face-2               (:foreground gruvbox-bright_yellow))
     (ivy-minibuffer-match-face-3               (:foreground gruvbox-faded_orange))
     (ivy-minibuffer-match-face-4               (:foreground gruvbox-faded_yellow))

     ;; ido
     (ido-only-match                            (:foreground gruvbox-faded_green))
     (ido-first-match                           (:foreground gruvbox-faded_green))
     (ido-subdir                                (:foreground gruvbox-faded_red))

     ;; magit
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

     ;; git-gutter
     (git-gutter:modified                       (:background gruvbox-faded_blue :foreground gruvbox-faded_blue))
     (git-gutter:added                          (:background gruvbox-faded_green :foreground gruvbox-faded_green))
     (git-gutter:deleted                        (:background gruvbox-faded_red :foreground gruvbox-faded_red))

     ;; git-gutter+
     (git-gutter+-modified                      (:foreground gruvbox-faded_blue :background gruvbox-faded_blue))
     (git-gutter+-added                         (:foreground gruvbox-faded_green :background gruvbox-faded_green))
     (git-gutter+-deleted                       (:foreground gruvbox-faded_red :background gruvbox-faded_red))

     ;; git-gutter-fringe
     (git-gutter-fr:modified                    (:inherit 'git-gutter:modified))
     (git-gutter-fr:added                       (:inherit 'git-gutter:added))
     (git-gutter-fr:deleted                     (:inherit 'git-gutter:deleted))

     ;; flyspell
     (flyspell-duplicate                        (:underline (:color gruvbox-light4 :style 'line)))
     (flyspell-incorrect                        (:underline (:color gruvbox-bright_red :style 'line)))

     ;; langtool
     (langtool-errline                          (:foreground gruvbox-dark0 :background gruvbox-bright_red))
     (langtool-correction-face                  (:foreground gruvbox-bright_yellow :weight 'bold))

     ;; latex
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

     ;; mu4e
     (mu4e-header-key-face                      (:foreground gruvbox-bright_green :weight 'bold ))
     (mu4e-unread-face                          (:foreground gruvbox-bright_blue :weight 'bold ))
     (mu4e-highlight-face                       (:foreground gruvbox-bright_green))

     ;; shell script
     (sh-quoted-exec                            (:foreground gruvbox-bright_purple))
     (sh-heredoc                                (:foreground gruvbox-bright_orange))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face   (:foreground gruvbox-light0))
     (undo-tree-visualizer-current-face         (:foreground gruvbox-bright_red))
     (undo-tree-visualizer-default-face         (:foreground gruvbox-dark4))
     (undo-tree-visualizer-register-face        (:foreground gruvbox-bright_yellow))
     (undo-tree-visualizer-unmodified-face      (:foreground gruvbox-bright_aqua))

     ;; widget faces
     (widget-button-pressed-face                (:foreground gruvbox-bright_red))
     (widget-documentation-face                 (:foreground gruvbox-faded_green))
     (widget-field                              (:foreground gruvbox-light0 :background gruvbox-dark2))
     (widget-single-line-field                  (:foreground gruvbox-light0 :background gruvbox-dark2))

     ;; dired+
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

     ;; neotree
     (neo-banner-face                           (:foreground gruvbox-bright_purple :bold t))
     (neo-dir-link-face                         (:foreground gruvbox-bright_yellow))
     (neo-expand-btn-face                       (:foreground gruvbox-bright_orange))
     (neo-file-link-face                        (:foreground gruvbox-light0))
     (neo-header-face                           (:foreground gruvbox-bright_purple))
     (neo-root-dir-face                         (:foreground gruvbox-bright_purple :bold t))

     ;; eshell
     (eshell-prompt-face                         (:foreground gruvbox-bright_aqua))
     (eshell-ls-archive-face                     (:foreground gruvbox-light3))
     (eshell-ls-backup-face                      (:foreground gruvbox-light4))
     (eshell-ls-clutter-face                     (:foreground gruvbox-bright_orange :weight 'bold))
     (eshell-ls-directory-face                   (:foreground gruvbox-bright_yellow))
     (eshell-ls-executable-face                  (:weight 'bold))
     (eshell-ls-missing-face                     (:foreground gruvbox-bright_red :bold t))
     (eshell-ls-product-face                     (:foreground gruvbox-faded_red))
     (eshell-ls-readonly-face                    (:foreground gruvbox-light2))
     (eshell-ls-special-face                     (:foreground gruvbox-bright_yellow :bold t))
     (eshell-ls-symlink-face                     (:foreground gruvbox-bright_red))
     (eshell-ls-unreadable-face                  (:foreground gruvbox-bright_red :bold t))

     ;; tabbar
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

     ;; wgrep
     (wgrep-delete-face                          (:strike-through gruvbox-bright_red))
     (wgrep-done-face                            (:foreground gruvbox-turquoise4))
     (wgrep-face                                 (:underline (:color gruvbox-bright_yellow :style 'line)))
     (wgrep-file-face                            (:inherit 'highlight))
     (wgrep-reject-face                          (:foreground gruvbox-bright_red :bold t))

     ;; hydra
     (hydra-face-red (:foreground gruvbox-bright_red :weight 'bold))
     (hydra-face-blue (:foreground gruvbox-bright_blue :weight 'bold))
     (hydra-face-amaranth (:foreground gruvbox-bright_yellow :weight 'bold))
     (hydra-face-pink (:foreground gruvbox-bright_purple :weight 'bold))
     (hydra-face-teal (:foreground gruvbox-bright_aqua :weight 'bold))

     ;; which-function-mode
     (which-func                                 (:foreground gruvbox-faded_blue))

     ;; auto-dim-other-buffers
     (auto-dim-other-buffers-face                (:background gruvbox-bg_inactive))
     )
    ,@body))

(provide 'gruvbox)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-dark-theme.el ends here
