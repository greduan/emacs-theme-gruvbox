;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Greduan
;; Copyright (c) 2016-2017 Jason Milkins

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
  "Contrast level for the theme background."
  :options '(soft medium hard)
  :group   'gruvbox)

(autothemer-deftheme
 gruvbox
 "A retro-groove colour theme (dark version)"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (gruvbox0_hard      "#1d2021" "#1c1c1c")
  (gruvbox0           "#282828" "#262626")
  (gruvbox0_soft      "#32302f" "#303030")
  (gruvbox1           "#3c3836" "#3a3a3a")
  (gruvbox2           "#504945" "#4e4e4e")
  (gruvbox3           "#665c54" "#626262")
  (gruvbox4           "#7c6f64" "#767676")

  (gruvbox-gray            "#928374" "#8a8a8a")

  (gruvbox-light0_hard     "#ffffc8" "#ffffd7")
  (gruvbox-light0          "#fdf4c1" "#ffffaf")
  (gruvbox-light0_soft     "#f4e8ba" "#ffffaf")
  (gruvbox-light1          "#ebdbb2" "#ffdfaf")
  (gruvbox-light2          "#d5c4a1" "#bcbcbc")
  (gruvbox-light3          "#bdae93" "#a8a8a8")
  (gruvbox-light4          "#a89984" "#949494")

  (gruvbox-bright_red      "#fb4933" "#d75f5f")
  (gruvbox-bright_green    "#b8bb26" "#afaf00")
  (gruvbox-bright_yellow   "#fabd2f" "#ffaf00")
  (gruvbox-bright_blue     "#83a598" "#87afaf")
  (gruvbox-bright_purple   "#d3869b" "#d787af")
  (gruvbox-bright_aqua     "#8ec07c" "#87af87")
  (gruvbox-bright_orange   "#fe8019" "#ff8700")

  (gruvbox-neutral_red     "#fb4934" "#d75f5f")
  (gruvbox-neutral_green   "#b8bb26" "#afaf00")
  (gruvbox-neutral_yellow  "#fabd2f" "#ffaf00")
  (gruvbox-neutral_blue    "#83a598" "#87afaf")
  (gruvbox-neutral_purple  "#d3869b" "#d787af")
  (gruvbox-neutral_aqua    "#8ec07c" "#87af87")
  (gruvbox-neutral_orange  "#fe8019" "#ff8700")

  (gruvbox-faded_red       "#9d0006" "#870000")
  (gruvbox-faded_green     "#79740e" "#878700")
  (gruvbox-faded_yellow    "#b57614" "#af8700")
  (gruvbox-faded_blue      "#076678" "#005f87")
  (gruvbox-faded_purple    "#8f3f71" "#875f87")
  (gruvbox-faded_aqua      "#427b58" "#5f8787")
  (gruvbox-faded_orange    "#af3a03" "#af5f00")

  (gruvbox_red        "#421E1E" "#5f0000")
  (gruvbox_blue       "#2B3C44" "#000087")
  (gruvbox_aqua       "#36473A" "#005f5f")

  (gruvbox-delimiter-one   "#458588" "#008787")
  (gruvbox-delimiter-two   "#b16286" "#d75f87")
  (gruvbox-delimiter-three "#8ec07c" "#87af87")
  (gruvbox-delimiter-four  "#d65d0e" "#d75f00")
  (gruvbox-white           "#FFFFFF" "#FFFFFF")
  (gruvbox-black           "#000000" "#000000")
  (gruvbox-sienna          "#DD6F48" "#d7875f")
  (gruvboxslategray4  "#528B8B" "#5f8787")
  (gruvbox-lightblue4      "#66999D" "#5fafaf")
  (gruvbox-burlywood4      "#BBAA97" "#afaf87")
  (gruvbox-aquamarine4     "#83A598" "#87af87")
  (gruvbox-turquoise4      "#61ACBB" "#5fafaf")

  (gruvbox-bg (cl-case gruvbox-contrast
                (hard gruvbox0_hard)
                (soft gruvbox0_soft)
                ;; Medium by default.
                (t    gruvbox0))))

 ;; UI
 ((default                                   (:background gruvbox-bg :foreground gruvbox-light0))
  (cursor                                    (:background gruvbox-light0))
  (mode-line                                 (:background gruvbox2 :foreground gruvbox-light2 :box nil))
  (mode-line-inactive                        (:background gruvbox1 :foreground gruvbox-light4 :box nil))
  (fringe                                    (:background gruvbox-bg))
  (linum                                     (:background gruvbox-bg :foreground gruvbox4))
  (hl-line                                   (:background gruvbox1))
  (region                                    (:background gruvbox2)) ;;selection
  (secondary-selection                       (:background gruvbox1))
  (minibuffer-prompt                         (:background gruvbox-bg :foreground gruvbox-neutral_green :bold t))
  (vertical-border                           (:foreground gruvbox2))
  (link                                      (:foreground gruvbox-faded_blue :underline t))
  (shadow                                    (:foreground gruvbox4))

  ;; Built-in syntax
  (font-lock-builtin-face                            (:foreground gruvbox-neutral_orange))
  (font-lock-constant-face                           (:foreground gruvbox-neutral_purple))
  (font-lock-comment-face                            (:foreground gruvbox4))
  (font-lock-function-name-face                      (:foreground gruvbox-neutral_yellow))
  (font-lock-keyword-face                            (:foreground gruvbox-neutral_red))
  (font-lock-string-face                             (:foreground gruvbox-neutral_green))
  (font-lock-variable-name-face                      (:foreground gruvbox-neutral_blue))
  (font-lock-type-face                               (:foreground gruvbox-neutral_purple))
  (font-lock-warning-face                            (:foreground gruvbox-neutral_red :bold t))

  ;; whitespace-mode
  (whitespace-space                          (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-hspace                         (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-tab                            (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-newline                        (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-trailing                       (:background gruvbox1 :foreground gruvbox-neutral_red))
  (whitespace-line                           (:background gruvbox1 :foreground gruvbox-neutral_red))
  (whitespace-space-before-tab               (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-indentation                    (:background gruvbox-bg :foreground gruvbox4))
  (whitespace-empty                          (:background nil :foreground nil))
  (whitespace-space-after-tab                (:background gruvbox-bg :foreground gruvbox4))

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

  ;; linum-relative
  (linum-relative-current-face               (:background gruvbox1 :foreground gruvbox-light4))

  ;; Highlight indentation mode
  (highlight-indentation-current-column-face (:background gruvbox2))
  (highlight-indentation-face                (:background gruvbox1))

  ;; Highlight linum
  (linum-highlight-face                      (:background gruvbox1 :foreground gruvbox-neutral_yellow))

  ;; Smartparens
  (sp-pair-overlay-face                      (:background gruvbox2))
  (sp-show-pair-match-face                   (:background gruvbox2)) ;; Pair tags highlight
  (sp-show-pair-mismatch-face                (:background gruvbox-neutral_red)) ;; Highlight for bracket without pair
  ;;(sp-wrap-overlay-face                     (:inherit 'sp-wrap-overlay-face))
  ;;(sp-wrap-tag-overlay-face                 (:inherit 'sp-wrap-overlay-face))

  ;; elscreen
  (elscreen-tab-background-face              (:background gruvbox-bg :box nil)) ;; Tab bar, not the tabs
  (elscreen-tab-control-face                 (:background gruvbox2 :foreground gruvbox-neutral_red :underline nil :box nil)) ;; The controls
  (elscreen-tab-current-screen-face          (:background gruvbox4 :foreground gruvbox0 :box nil)) ;; Current tab
  (elscreen-tab-other-screen-face            (:background gruvbox2 :foreground gruvbox-light4 :underline nil :box nil)) ;; Inactive tab

  ;; ag (The Silver Searcher)
  (ag-hit-face                               (:foreground gruvbox-neutral_blue))
  (ag-match-face                             (:foreground gruvbox-neutral_red))

  ;; Diffs
  (diff-changed                              (:background nil :foreground gruvbox-light1))
  (diff-added                                (:background nil :foreground gruvbox-neutral_green))
  (diff-removed                              (:background nil :foreground gruvbox-neutral_red))
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
  (popup-face                                (:foreground gruvbox-light1 :background gruvbox1))
  (popup-menu-mouse-face                     (:foreground gruvbox-light0 :background gruvbox-faded_green))
  (popup-menu-selection-face                 (:foreground gruvbox-light0 :background gruvbox-faded_green))
  (popup-tip-face                            (:foreground gruvbox-light2 :background gruvbox2))

  ;; helm
  (helm-M-x-key                              (:foreground gruvbox-neutral_orange ))
  (helm-action                               (:foreground gruvbox-white :underline t))
  (helm-bookmark-addressbook                 (:foreground gruvbox-neutral_red))
  (helm-bookmark-directory                   (:foreground gruvbox-bright_purple))
  (helm-bookmark-file                        (:foreground gruvbox-faded_blue))
  (helm-bookmark-gnus                        (:foreground gruvbox-faded_purple))
  (helm-bookmark-info                        (:foreground gruvbox-turquoise4))
  (helm-bookmark-man                         (:foreground gruvbox-sienna))
  (helm-bookmark-w3m                         (:foreground gruvbox-neutral_yellow))
  (helm-buffer-directory                     (:foreground gruvbox-white :background gruvbox-bright_blue))
  (helm-buffer-not-saved                     (:foreground gruvbox-faded_red))
  (helm-buffer-process                       (:foreground gruvbox-burlywood4))
  (helm-buffer-saved-out                     (:foreground gruvbox-bright_red))
  (helm-buffer-size                          (:foreground gruvbox-bright_purple))
  (helm-candidate-number                     (:foreground gruvbox-neutral_green))
  (helm-ff-directory                         (:foreground gruvbox-neutral_purple))
  (helm-ff-executable                        (:foreground gruvbox-turquoise4))
  (helm-ff-file                              (:foreground gruvbox-sienna))
  (helm-ff-invalid-symlink                   (:foreground gruvbox-white :background gruvbox-bright_red))
  (helm-ff-prefix                            (:foreground gruvbox-black :background gruvbox-neutral_yellow))
  (helm-ff-symlink                           (:foreground gruvbox-neutral_orange))
  (helm-grep-cmd-line                        (:foreground gruvbox-neutral_green))
  (helm-grep-file                            (:foreground gruvbox-faded_purple))
  (helm-grep-finish                          (:foreground gruvbox-turquoise4))
  (helm-grep-lineno                          (:foreground gruvbox-neutral_orange))
  (helm-grep-match                           (:foreground gruvbox-neutral_yellow))
  (helm-grep-running                         (:foreground gruvbox-neutral_red))
  (helm-header                               (:foreground gruvbox-aquamarine4))
  (helm-helper                               (:foreground gruvbox-aquamarine4))
  (helm-history-deleted                      (:foreground gruvbox-black :background gruvbox-bright_red))
  (helm-history-remote                       (:foreground gruvbox-faded_red))
  (helm-lisp-completion-info                 (:foreground gruvbox-faded_orange))
  (helm-lisp-show-completion                 (:foreground gruvbox-bright_red))
  (helm-locate-finish                        (:foreground gruvbox-white :background gruvbox-aquamarine4))
  (helm-match                                (:foreground gruvbox-neutral_orange))
  (helm-moccur-buffer                        (:foreground gruvbox-bright_aqua :underline t))
  (helm-prefarg                              (:foreground gruvbox-turquoise4))
  (helm-selection                            (:foreground gruvbox-white :background gruvbox2))
  (helm-selection-line                       (:foreground gruvbox-white :background gruvbox2))
  (helm-separator                            (:foreground gruvbox-faded_red))
  (helm-source-header                        (:foreground gruvbox-light2))
  (helm-visible-mark                         (:foreground gruvbox-black :background gruvbox-light3))

  ;; company-mode
  (company-scrollbar-bg                      (:background gruvbox1))
  (company-scrollbar-fg                      (:background gruvbox0_soft))
  (company-tooltip                           (:background gruvbox0_soft))
  (company-tooltip-annotation                (:foreground gruvbox-neutral_green))
  (company-tooltip-selection                 (:foreground gruvbox-neutral_purple))
  (company-tooltip-common                    (:foreground gruvbox-neutral_blue :underline t))
  (company-tooltip-common-selection          (:foreground gruvbox-neutral_blue :underline t))
  (company-preview-common                    (:foreground gruvbox-neutral_purple))

  ;; Term
  (term-color-black                          (:foreground gruvbox2 :background gruvbox1))
  (term-color-blue                           (:foreground gruvbox-bright_blue :background gruvbox-neutral_blue))
  (term-color-cyan                           (:foreground gruvbox-bright_aqua :background gruvbox-neutral_aqua))
  (term-color-green                          (:foreground gruvbox-bright_green :background gruvbox-neutral_green))
  (term-color-magenta                        (:foreground gruvbox-bright_purple :background gruvbox-neutral_purple))
  (term-color-red                            (:foreground gruvbox-bright_red :background gruvbox-neutral_red))
  (term-color-white                          (:foreground gruvbox-light1 :background gruvbox-light1))
  (term-color-yellow                         (:foreground gruvbox-bright_yellow :background gruvbox-neutral_yellow))
  (term-default-fg-color                     (:foreground gruvbox-light0))
  (term-default-bg-color                     (:background gruvbox-bg))

  ;; message-mode
  (message-header-to                         (:inherit 'font-lock-variable-name-face))
  (message-header-cc                         (:inherit 'font-lock-variable-name-face))
  (message-header-subject                    (:foreground gruvbox-neutral_orange :weight 'bold))
  (message-header-newsgroups                 (:foreground gruvbox-neutral_yellow :weight 'bold))
  (message-header-other                      (:inherit 'font-lock-variable-name-face))
  (message-header-name                       (:inherit 'font-lock-keyword-face))
  (message-header-xheader                    (:foreground gruvbox-faded_blue))
  (message-separator                         (:inherit 'font-lock-comment-face))
  (message-cited-text                        (:inherit 'font-lock-comment-face))
  (message-mml                               (:foreground gruvbox-faded_green :weight 'bold))

  ;; org-mode
  (org-hide                                  (:foreground gruvbox0))
  (org-level-1                               (:foreground gruvbox-neutral_blue))
  (org-level-2                               (:foreground gruvbox-neutral_yellow))
  (org-level-3                               (:foreground gruvbox-neutral_purple))
  (org-level-4                               (:foreground gruvbox-neutral_red))
  (org-level-5                               (:foreground gruvbox-neutral_green))
  (org-level-6                               (:foreground gruvbox-neutral_aqua))
  (org-level-7                               (:foreground gruvbox-faded_blue))
  (org-level-8                               (:foreground gruvbox-neutral_orange))
  (org-special-keyword                       (:inherit 'font-lock-comment-face))
  (org-drawer                                (:inherit 'font-lock-function-face))
  (org-column                                (:background gruvbox0))
  (org-column-title                          (:background gruvbox0 :underline t :weight 'bold))
  (org-warning                               (:foreground gruvbox-neutral_red :weight 'bold :underline nil :bold t))
  (org-archived                              (:foreground gruvbox-light0 :weight 'bold))
  (org-link                                  (:foreground gruvbox-faded_aqua :underline t))
  (org-footnote                              (:foreground gruvbox-neutral_aqua :underline t))
  (org-ellipsis                              (:foreground gruvbox-light4 :underline t))
  (org-date                                  (:foreground gruvbox-neutral_blue :underline t))
  (org-sexp-date                             (:foreground gruvbox-faded_blue :underline t))
  (org-tag                                   (:bold t :weight 'bold))
  (org-list-dt                               (:bold t :weight 'bold))
  (org-todo                                  (:foreground gruvbox-neutral_red :weight 'bold :bold t))
  (org-done                                  (:foreground gruvbox-neutral_aqua :weight 'bold :bold t))
  (org-agenda-done                           (:foreground gruvbox-neutral_aqua))
  (org-headline-done                         (:foreground gruvbox-neutral_aqua))
  (org-table                                 (:foreground gruvbox-neutral_blue))
  (org-formula                               (:foreground gruvbox-neutral_yellow))
  (org-document-title                        (:foreground gruvbox-faded_blue))
  (org-document-info                         (:foreground gruvbox-faded_blue))
  (org-agenda-structure                      (:inherit 'font-lock-comment-face))
  (org-agenda-date-today                     (:foreground gruvbox-light0 :weight 'bold :italic t))
  (org-scheduled                             (:foreground gruvbox-neutral_yellow))
  (org-scheduled-today                       (:foreground gruvbox-neutral_blue))
  (org-scheduled-previously                  (:foreground gruvbox-faded_red))
  (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
  (org-deadline-announce                     (:foreground gruvbox-faded_red))
  (org-time-grid                             (:foreground gruvbox-faded_orange))

  ;; org-habit
  (org-habit-clear-face                      (:background gruvbox-faded_blue))
  (org-habit-clear-future-face               (:background gruvbox-neutral_blue))
  (org-habit-ready-face                      (:background gruvbox-faded_green))
  (org-habit-ready-future-face               (:background gruvbox-neutral_green))
  (org-habit-alert-face                      (:background gruvbox-faded_yellow))
  (org-habit-alert-future-face               (:background gruvbox-neutral_yellow))
  (org-habit-overdue-face                    (:background gruvbox-faded_red))
  (org-habit-overdue-future-face             (:background gruvbox-neutral_red))

  ;; elfeed
  (elfeed-search-title-face                  (:foreground gruvbox-gray  ))
  (elfeed-search-unread-title-face           (:foreground gruvbox-light0))
  (elfeed-search-date-face                   (:inherit 'font-lock-builtin-face :underline t))
  (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
  (elfeed-search-tag-face                    (:inherit 'font-lock-keyword-face))
  (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
  (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
  (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

  ;; Smart-mode-line
  (sml/global                                (:foreground gruvbox-burlywood4 :inverse-video nil))
  (sml/modes                                 (:foreground gruvbox-bright_green))
  (sml/filename                              (:foreground gruvbox-bright_red :weight 'bold))
  (sml/prefix                                (:foreground gruvbox-light1))
  (sml/read-only                             (:foreground gruvbox-neutral_blue))
  (persp-selected-face                       (:foreground gruvbox-neutral_orange))

  ;;isearch
  (isearch                                   (:foreground gruvbox-black :background gruvbox-neutral_orange))
  (lazy-highlight                            (:foreground gruvbox-black :background gruvbox-neutral_yellow))
  (isearch-fail                              (:foreground gruvbox-light0 :background gruvbox-bright_red))

  ;; markdown-mode
  (markdown-header-face-1                    (:foreground gruvbox-neutral_blue))
  (markdown-header-face-2                    (:foreground gruvbox-neutral_yellow))
  (markdown-header-face-3                    (:foreground gruvbox-neutral_purple))
  (markdown-header-face-4                    (:foreground gruvbox-neutral_red))
  (markdown-header-face-5                    (:foreground gruvbox-neutral_green))
  (markdown-header-face-6                    (:foreground gruvbox-neutral_aqua))

  ;; anzu-mode
  (anzu-mode-line                            (:foreground gruvbox-bright_yellow :weight 'bold))
  (anzu-match-1                              (:background gruvbox-bright_green))
  (anzu-match-2                              (:background gruvbox-faded_yellow))
  (anzu-match-3                              (:background gruvbox-aquamarine4))
  (anzu-replace-to                           (:foreground gruvbox-bright_yellow))
  (anzu-replace-highlight                    (:inherit 'isearch))

  ;; Ace-jump-mode
  (ace-jump-face-background                  (:foreground gruvbox-light4 :background gruvbox-bg :inverse-video nil))
  (ace-jump-face-foreground                  (:foreground gruvbox-bright_red :background gruvbox-bg :inverse-video nil :box 1))

  ;; Ace-window
  (aw-background-face                        (:forground  gruvbox-light1 :background gruvbox-bg :inverse-video nil))
  (aw-leading-char-face                      (:foreground gruvbox-bright_orange :background gruvbox-bg :height 4.0 :box (:line-width 1 :color gruvbox-bright_orange)))

  ;; show-paren
  (show-paren-match                          (:background gruvbox3 :weight 'bold))
  (show-paren-mismatch                       (:background gruvbox-bright_red :foreground gruvbox3 :weight 'bold))

  ;; ivy
  (ivy-current-match                         (:foreground gruvbox-white :weight 'bold :underline t))
  (ivy-minibuffer-match-face-1               (:foreground gruvbox-neutral_orange))
  (ivy-minibuffer-match-face-2               (:foreground gruvbox-neutral_yellow))
  (ivy-minibuffer-match-face-3               (:foreground gruvbox-faded_orange))
  (ivy-minibuffer-match-face-4               (:foreground gruvbox-faded_yellow))

  ;; mu4e
  (mu4e-header-key-face                      (:foreground gruvbox-bright_green :weight 'bold ))
  (mu4e-unread-face                          (:foreground gruvbox-bright_blue :weight 'bold ))
  (mu4e-highlight-face                       (:foreground gruvbox-neutral_green))

  ;; MODE SUPPORT: dired+
  (diredp-file-name                          (:foreground gruvbox-light2))
  (diredp-file-suffix                        (:foreground gruvbox-light4))
  (diredp-compressed-file-suffix             (:foreground gruvbox-faded_blue))
  (diredp-dir-name                           (:foreground gruvbox-faded_blue))
  (diredp-dir-heading                        (:foreground gruvbox-bright_blue))
  (diredp-symlink                            (:foreground gruvbox-bright_orange))
  (diredp-date-time                          (:foreground gruvbox-light3))
  (diredp-number                             (:foreground gruvbox-faded_blue))
  (diredp-no-priv                            (:foreground gruvbox4))
  (diredp-other-priv                         (:foreground gruvbox2))
  (diredp-rare-priv                          (:foreground gruvbox4))
  (diredp-ignored-file-name                  (:foreground gruvbox4))

  (diredp-dir-priv                           (:foreground gruvbox-faded_blue  :background gruvbox_blue))
  (diredp-exec-priv                          (:foreground gruvbox-faded_blue  :background gruvbox_blue))
  (diredp-link-priv                          (:foreground gruvbox-faded_aqua  :background gruvbox_aqua))
  (diredp-read-priv                          (:foreground gruvbox-bright_red  :background gruvbox_red))
  (diredp-write-priv                         (:foreground gruvbox-bright_aqua :background gruvbox_aqua)))

 (custom-theme-set-variables 'gruvbox
                             `(ansi-color-names-vector
                               [,gruvbox1
                                ,gruvbox-neutral_red
                                ,gruvbox-neutral_green
                                ,gruvbox-neutral_yellow
                                ,gruvbox-neutral_blue
                                ,gruvbox-neutral_purple
                                ,gruvbox-neutral_aqua
                                ,gruvbox-light1])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbox)


;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-theme.el ends here
