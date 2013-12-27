;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin, Eduán Lávaque

;; Author: Eduán Lávaque <eduanlavaque@gmail.com>
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 0.7

;;; Commentary:

;; A port of the Gruvbox colorscheme for Vim, built on top of the new
;; built-in theme support in Emacs 24.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim on which this port
;; is based.
;; Lee Machin who created first port of the original theme, off of which
;; I'm working on to make it better.

;;; Code:

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme gruvbox
  "A retro-groove colour theme")

;;gui version
(let ((gruvbox-dark-0   "#282828")
      (gruvbox-dark-1   "#3c3836")
      (gruvbox-dark-2   "#504945")
      (gruvbox-dark-3   "#665c54")
      (gruvbox-dark-4   "#7c6f64")

      (gruvbox-medium   "#928374")

      (gruvbox-light-0  "#fdf4c1")
      (gruvbox-light-1  "#ebdbb2")
      (gruvbox-light-2  "#d5c4a1")
      (gruvbox-light-3  "#bdae93")
      (gruvbox-light-4  "#a89984")

      (gruvbox-red      "#fb4934")
      (gruvbox-green    "#b8bb26")
      (gruvbox-yellow   "#fabd2f")
      (gruvbox-blue     "#83a598")
      (gruvbox-purple   "#d3869b")
      (gruvbox-aqua     "#8ec07c")
      (gruvbox-orange   "#fe8019"))

;;;256 colors version
;(let ((gruvbox-dark-0   "#262626")  ;235
;      (gruvbox-dark-1   "#3a3a3a")  ;237
;      (gruvbox-dark-2   "#4e4e4e")  ;239
;      (gruvbox-dark-3   "#606060")  ;241
;      (gruvbox-dark-4   "#767676")  ;243
;
;      (gruvbox-medium   "#8a8a8a")  ;245
;
;      (gruvbox-light-0  "#ffffaf")  ;229
;      (gruvbox-light-1  "#ffd7af")  ;223
;      (gruvbox-light-2  "#bcbcbc")  ;250
;      (gruvbox-light-3  "#a8a8a8")  ;248
;      (gruvbox-light-4  "#949494")  ;246
;
;      (gruvbox-red      "#d75f5f")  ;167
;      (gruvbox-green    "#afaf00")  ;142
;      (gruvbox-yellow   "#ffaf00")  ;214
;      (gruvbox-blue     "#87afaf")  ;109
;      (gruvbox-purple   "#d787af")  ;175
;      (gruvbox-aqua     "#87af87")  ;108
;      (gruvbox-orange   "#ff8700")) ;208

(custom-theme-set-faces
  'gruvbox

  ;;ui
  `(default ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-light-0))))
  `(cursor ((t (:background ,gruvbox-yellow))))
  `(mode-line ((t (:box nil :background ,gruvbox-dark-4 :foreground ,gruvbox-dark-0))))
  `(mode-line-inactive ((t (:box nil :background ,gruvbox-dark-2 :foreground ,gruvbox-light-4))))
  `(fringe ((t (:background ,gruvbox-dark-0))))
  `(linum ((t (:foreground ,gruvbox-dark-4))))
  `(hl-line ((t (:background ,gruvbox-dark-1))))
  `(region ((t (:background ,gruvbox-dark-2)))) ;;selection
  `(minibuffer-prompt ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-green :bold t))))
  `(ag-hit-face ((t (:foreground ,gruvbox-green))))
  `(ag-match-face ((t (:foreground ,gruvbox-red))))

  ;;basic
  `(font-lock-builtin-face ((t (:foreground ,gruvbox-orange))))
  `(font-lock-constant-face ((t (:foreground ,gruvbox-purple))))
  `(font-lock-comment-face ((t (:foreground ,gruvbox-dark-4))))
  `(font-lock-function-name-face ((t (:foreground ,gruvbox-green))))
  `(font-lock-keyword-face ((t (:foreground ,gruvbox-red))))
  `(font-lock-string-face ((t (:foreground ,gruvbox-green))))
  `(font-lock-variable-name-face ((t (:foreground ,gruvbox-blue))))
  `(font-lock-type-face ((t (:foreground ,gruvbox-purple))))
  `(font-lock-warning-face ((t (:foreground ,gruvbox-red :bold t))))

  ;;whitespace-mode
  `(whitespace-space ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-3))))
  `(whitespace-hspace ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-3))))
  `(whitespace-tab ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-3))))
  `(whitespace-newline ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-3))))
  `(whitespace-trailing ((t (:background ,gruvbox-orange))))
  `(whitespace-line ((t (:background ,gruvbox-red :foreground ,gruvbox-dark-0))))
  `(whitespace-space-before-tab ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-4))))
  `(whitespace-indentation ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-4))))
  `(whitespace-empty ((t (:background nil :foreground nil))))
  `(whitespace-space-after-tab ((t (:background ,gruvbox-dark-0 :foreground ,gruvbox-dark-4))))

  ;;RainbowDelimiters
  `(rainbow-delimiters-depth-1-face ((t (:foreground "#458588"))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground "#b16286"))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground "#cc241d"))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground "#d65d0e"))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground "#458488"))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground "#b16286"))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground "#cc241d"))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground "#d65d0e"))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground "#458588"))))
  `(rainbow-delimiters-depth-10-face ((t (:foreground "#b16286"))))
  `(rainbow-delimiters-depth-11-face ((t (:foreground "#cc241d"))))
  `(rainbow-delimiters-depth-12-face ((t (:foreground "#d65d0e"))))
  `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,gruvbox-light-0))))

  ;;linum-relative
  `(linum-relative-current-face ((t (:background ,gruvbox-dark-1 :foreground ,gruvbox-light-4))))

  ;;Smartparens
  `(sp-pair-overlay-face ((t (:background ,gruvbox-dark-2))))
  ;`(sp-wrap-overlay-face ((t (:inherit sp-wrap-overlay-face))))
  ;`(sp-wrap-tag-overlay-face ((t (:inherit sp-wrap-overlay-face))))
  `(sp-show-pair-match-face ((t (:background ,gruvbox-light-2)))) ;;pair tags highlight
  `(sp-show-pair-mismatch-face ((t (:background "red")))) ;;highlight for bracket without pair

  ;;elscreen
  `(elscreen-tab-background-face ((t (:box nil :background ,gruvbox-dark-0)))) ;;tab bar, not tabs
  `(elscreen-tab-control-face ((t (:box nil :background ,gruvbox-dark-2 :foreground ,gruvbox-red :underline nil)))) ;;the controls, arrows 'n' stuff
  `(elscreen-tab-current-screen-face ((t (:box nil :background ,gruvbox-dark-4 :foreground ,gruvbox-dark-0)))) ;;current tab
  `(elscreen-tab-other-screen-face ((t (:box nil :background ,gruvbox-dark-2 :foreground ,gruvbox-light-4 :underline nil)))) ;;inactive tab
  
  ;;ag (The Silver Searcher)
  `(ag-hit-face ((t (:foreground ,gruvbox-blue))))
  `(ag-match-face ((t (:foreground ,gruvbox-red))))
))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gruvbox)
