;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2014 Greduan

;; Authors: Lee Machin <ljmachin@gmail.com>
;;          Greduan <eduan@websharks-inc.com>
;; Maintainer: Greduan <eduan@websharks-inc.com>
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 0.14

;;; Commentary:

;; A port of the Gruvbox colorscheme for Vim, built on top of the new
;; built-in theme support in Emacs 24.
;;
;; This theme contains my own modifications and it's a bit opinionated
;; sometimes, deviating from the original because of it. I try to stay true to
;; the original, however.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim on which this port
;; is based.

;; Lee Machin is who created first port of the original theme, off of which
;; I'm working on to make it better.

;;; Code:

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme gruvbox "A retro-groove colour theme")

(let ((gruvbox-dark0_hard  (if (display-graphic-p) "#1d2021" "color-234"))
      (gruvbox-dark0       (if (display-graphic-p) "#282828" "color-235"))
      (gruvbox-dark0_soft  (if (display-graphic-p) "#32302f" "color-236"))
      (gruvbox-dark1       (if (display-graphic-p) "#3c3836" "color-237"))
      (gruvbox-dark2       (if (display-graphic-p) "#504945" "color-239"))
      (gruvbox-dark3       (if (display-graphic-p) "#665c54" "color-241"))
      (gruvbox-dark4       (if (display-graphic-p) "#7c6f64" "color-243"))

      (gruvbox-medium      (if (display-graphic-p) "#928374" "color-245")) ;; or 244

      (gruvbox-light0_hard (if (display-graphic-p) "#ffffc8" "color-230"))
      (gruvbox-light0      (if (display-graphic-p) "#fdf4c1" "color-229"))
      (gruvbox-light0_soft (if (display-graphic-p) "#f4e8ba" "color-228"))
      (gruvbox-light1      (if (display-graphic-p) "#ebdbb2" "color-223"))
      (gruvbox-light2      (if (display-graphic-p) "#d5c4a1" "color-250"))
      (gruvbox-light3      (if (display-graphic-p) "#bdae93" "color-248"))
      (gruvbox-light4      (if (display-graphic-p) "#a89984" "color-246"))

      ;; neutral, no 256-color version, will request
      ;(gruvbox-red         (if (display-graphic-p) "#fb4934" "color-"))
      ;(gruvbox-green       (if (display-graphic-p) "#b8bb26" "color-"))
      ;(gruvbox-yellow      (if (display-graphic-p) "#fabd2f" "color-"))
      ;(gruvbox-blue        (if (display-graphic-p) "#83a598" "color-"))
      ;(gruvbox-purple      (if (display-graphic-p) "#d3869b" "color-"))
      ;(gruvbox-aqua        (if (display-graphic-p) "#8ec07c" "color-"))
      ;(gruvbox-orange      (if (display-graphic-p) "#fe8019" "color-")))
      (gruvbox-red         "#fb4934")
      (gruvbox-green       "#b8bb26")
      (gruvbox-yellow      "#fabd2f")
      (gruvbox-blue        "#83a598")
      (gruvbox-purple      "#d3869b")
      (gruvbox-aqua        "#8ec07c")
      (gruvbox-orange      "#fe8019"))

  (custom-theme-set-faces
    'gruvbox

    ;;UI
    `(default ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-light0))))
    `(cursor ((t (:background ,gruvbox-light0))))
    `(mode-line ((t (:box nil :background ,gruvbox-dark4 :foreground ,gruvbox-dark0))))
    `(mode-line-inactive ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light4))))
    `(fringe ((t (:background ,gruvbox-dark0))))
    `(linum ((t (:foreground ,gruvbox-dark4))))
    `(hl-line ((t (:background ,gruvbox-dark1))))
    `(region ((t (:background ,gruvbox-dark2)))) ;;selection
    `(minibuffer-prompt ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-green :bold t))))

    ;;Built-in syntax
    `(font-lock-builtin-face ((t (:foreground ,gruvbox-orange))))
    `(font-lock-constant-face ((t (:foreground ,gruvbox-purple))))
    `(font-lock-comment-face ((t (:foreground ,gruvbox-dark4))))
    `(font-lock-function-name-face ((t (:foreground ,gruvbox-green))))
    `(font-lock-keyword-face ((t (:foreground ,gruvbox-red))))
    `(font-lock-string-face ((t (:foreground ,gruvbox-green))))
    `(font-lock-variable-name-face ((t (:foreground ,gruvbox-blue))))
    `(font-lock-type-face ((t (:foreground ,gruvbox-purple))))
    `(font-lock-warning-face ((t (:foreground ,gruvbox-red :bold t))))

    ;;whitespace-mode
    `(whitespace-space ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-hspace ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-newline ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-trailing ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-red))))
    `(whitespace-line ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-red))))
    `(whitespace-space-before-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-indentation ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-empty ((t (:background nil :foreground nil))))
    `(whitespace-space-after-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))

    ;; TODO
    ;; - Replace with variable values
    ;;RainbowDelimiters
    `(rainbow-delimiters-depth-1-face   ((t (:foreground "#458588"))))
    `(rainbow-delimiters-depth-2-face   ((t (:foreground "#b16286"))))
    `(rainbow-delimiters-depth-3-face   ((t (:foreground "#cc241d"))))
    `(rainbow-delimiters-depth-4-face   ((t (:foreground "#d65d0e"))))
    `(rainbow-delimiters-depth-5-face   ((t (:foreground "#458488"))))
    `(rainbow-delimiters-depth-6-face   ((t (:foreground "#b16286"))))
    `(rainbow-delimiters-depth-7-face   ((t (:foreground "#cc241d"))))
    `(rainbow-delimiters-depth-8-face   ((t (:foreground "#d65d0e"))))
    `(rainbow-delimiters-depth-9-face   ((t (:foreground "#458588"))))
    `(rainbow-delimiters-depth-10-face  ((t (:foreground "#b16286"))))
    `(rainbow-delimiters-depth-11-face  ((t (:foreground "#cc241d"))))
    `(rainbow-delimiters-depth-12-face  ((t (:foreground "#d65d0e"))))
    `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,gruvbox-light0))))

    ;;linum-relative
    `(linum-relative-current-face ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light4))))

    ;;Smartparens
    `(sp-pair-overlay-face ((t (:background ,gruvbox-dark2))))
    ;`(sp-wrap-overlay-face ((t (:inherit sp-wrap-overlay-face))))
    ;`(sp-wrap-tag-overlay-face ((t (:inherit sp-wrap-overlay-face))))
    `(sp-show-pair-match-face ((t (:background ,gruvbox-dark2)))) ;;Pair tags highlight
    `(sp-show-pair-mismatch-face ((t (:background "red")))) ;;Highlight for bracket without pair

    ;;elscreen
    `(elscreen-tab-background-face ((t (:box nil :background ,gruvbox-dark0)))) ;;tab bar, not tabs
    `(elscreen-tab-control-face ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-red :underline nil)))) ;;the controls, arrows 'n' stuff
    `(elscreen-tab-current-screen-face ((t (:box nil :background ,gruvbox-dark4 :foreground ,gruvbox-dark0)))) ;;current tab
    `(elscreen-tab-other-screen-face ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light4 :underline nil)))) ;;inactive tab

    ;;ag (The Silver Searcher)
    `(ag-hit-face ((t (:foreground ,gruvbox-blue))))
    `(ag-match-face ((t (:foreground ,gruvbox-red))))

    ;;Diffs
    `(diff-changed ((t (:background nil :foreground ,gruvbox-light1))))
    `(diff-added ((t (:background nil :foreground ,gruvbox-green))))
    `(diff-removed ((t (:background nil :foreground ,gruvbox-red))))
    `(diff-indicator-changed ((t (:inherit diff-changed))))
    `(diff-indicator-added ((t (:inherit diff-added))))
    `(diff-indicator-removed ((t (:inherit diff-removed))))

    ;;Term
    `(term-color-black ((t (:foreground ,gruvbox-dark1))))
    `(term-color-blue ((t (:foreground ,gruvbox-blue))))
    `(term-color-cyan ((t (:foreground ,gruvbox-aqua))))
    `(term-color-green ((t (:foreground ,gruvbox-green))))
    `(term-color-magenta ((t (:foreground ,gruvbox-purple))))
    `(term-color-red ((t (:foreground ,gruvbox-red))))
    `(term-color-white ((t (:foreground ,gruvbox-light1))))
    `(term-color-yellow ((t (:foreground ,gruvbox-yellow))))
    `(term-default-fg-color ((t (:foreground ,gruvbox-light0))))
    `(term-default-bg-color ((t (:background ,gruvbox-dark0)))))

(custom-theme-set-variables
  'gruvbox

  `(ansi-color-names-vector [,gruvbox-dark1 ,gruvbox-red ,gruvbox-green ,gruvbox-yellow
    ,gruvbox-blue ,gruvbox-purple ,gruvbox-aqua ,gruvbox-light1])))

(defun gruvbox-set-ansi-color-names-vector ()
  "Give comint and the like the same colours as the term colours we set"
  (setq ansi-color-names-vector
    [term-color-black term-color-red term-color-green term-color-yellow term-color-blue
     term-color-purple term-color-aqua term-color-white]))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'gruvbox)

;;; gruvbox-theme.el ends here
