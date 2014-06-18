;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2014 Greduan

;; Authors: Lee Machin <ljmachin@gmail.com>
;;          Greduan <eduan@websharks-inc.com>
;; Maintainer: Greduan <eduan@websharks-inc.com>
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 0.15.0

;;; Commentary:

;; A port of the Gruvbox colorscheme for Vim, built on top of the new built-in
;; theme support in Emacs 24.
;;
;; This theme contains my own modifications and it's a bit opinionated
;; sometimes, deviating from the original because of it. I try to stay true to
;; the original as much as possible, however. I only make changes where I would
;; have made the changes on the original.
;;
;; Since there is no direct equivalent in syntax highlighting from Vim to Emacs
;; some stuff may look different, especially in stuff like JS2-mode, where it
;; adds stuff that Vim doesn't have, in terms of syntax.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim, on which this port
;; is based.

;; Lee Machin created the first port of the original theme, which I'm working on
;; to make better and more feature complete.

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

      (gruvbox-bright_red     (if (display-graphic-p) "#fb4933" "color-167"))
      (gruvbox-bright_green   (if (display-graphic-p) "#b8bb26" "color-142"))
      (gruvbox-bright_yellow  (if (display-graphic-p) "#fabd2f" "color-214"))
      (gruvbox-bright_blue    (if (display-graphic-p) "#83a598" "color-109"))
      (gruvbox-bright_purple  (if (display-graphic-p) "#d3869b" "color-175"))
      (gruvbox-bright_aqua    (if (display-graphic-p) "#8ec07c" "color-108"))
      (gruvbox-bright_orange  (if (display-graphic-p) "#fe8019" "color-208"))

      ;; neutral, no 256-color code, requested, nice work-around meanwhile
      (gruvbox-neutral_red    (if (display-graphic-p) "#fb4934" "#d75f5f"))
      (gruvbox-neutral_green  (if (display-graphic-p) "#b8bb26" "#afaf00"))
      (gruvbox-neutral_yellow (if (display-graphic-p) "#fabd2f" "#ffaf00"))
      (gruvbox-neutral_blue   (if (display-graphic-p) "#83a598" "#87afaf"))
      (gruvbox-neutral_purple (if (display-graphic-p) "#d3869b" "#d787af"))
      (gruvbox-neutral_aqua   (if (display-graphic-p) "#8ec07c" "#87af87"))
      (gruvbox-neutral_orange (if (display-graphic-p) "#fe8019" "#ff8700"))

      (gruvbox-faded_red      (if (display-graphic-p) "#9d0006" "color-88"))
      (gruvbox-faded_green    (if (display-graphic-p) "#79740e" "color-100"))
      (gruvbox-faded_yellow   (if (display-graphic-p) "#b57614" "color-136"))
      (gruvbox-faded_blue     (if (display-graphic-p) "#076678" "color-24"))
      (gruvbox-faded_purple   (if (display-graphic-p) "#8f3f71" "color-96"))
      (gruvbox-faded_aqua     (if (display-graphic-p) "#427b58" "color-66"))
      (gruvbox-faded_orange   (if (display-graphic-p) "#af3a03" "color-130")))

  (custom-theme-set-faces
    'gruvbox

    ;; UI
    `(default ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-light0))))
    `(cursor ((t (:background ,gruvbox-light0))))
    `(mode-line ((t (:box nil :background ,gruvbox-dark4 :foreground ,gruvbox-dark0))))
    `(mode-line-inactive ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light4))))
    `(fringe ((t (:background ,gruvbox-dark0))))
    `(linum ((t (:foreground ,gruvbox-dark4))))
    `(hl-line ((t (:background ,gruvbox-dark1))))
    `(region ((t (:background ,gruvbox-dark2)))) ;;selection
    `(minibuffer-prompt ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-neutral_green :bold t))))

    ;; Built-in syntax
    `(font-lock-builtin-face ((t (:foreground ,gruvbox-neutral_orange))))
    `(font-lock-constant-face ((t (:foreground ,gruvbox-neutral_purple))))
    `(font-lock-comment-face ((t (:foreground ,gruvbox-dark4))))
    `(font-lock-function-name-face ((t (:foreground ,gruvbox-neutral_green))))
    `(font-lock-keyword-face ((t (:foreground ,gruvbox-neutral_red))))
    `(font-lock-string-face ((t (:foreground ,gruvbox-neutral_green))))
    `(font-lock-variable-name-face ((t (:foreground ,gruvbox-neutral_blue))))
    `(font-lock-type-face ((t (:foreground ,gruvbox-neutral_purple))))
    `(font-lock-warning-face ((t (:foreground ,gruvbox-neutral_red :bold t))))

    ;; whitespace-mode
    `(whitespace-space ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-hspace ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-newline ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-trailing ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-neutral_red))))
    `(whitespace-line ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-neutral_red))))
    `(whitespace-space-before-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-indentation ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))
    `(whitespace-empty ((t (:background nil :foreground nil))))
    `(whitespace-space-after-tab ((t (:background ,gruvbox-dark0 :foreground ,gruvbox-dark4))))

    ;; TODO
    ;; - Replace with variable values
    ;; RainbowDelimiters
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

    ;; linum-relative
    `(linum-relative-current-face ((t (:background ,gruvbox-dark1 :foreground ,gruvbox-light4))))

    ;; Smartparens
    `(sp-pair-overlay-face ((t (:background ,gruvbox-dark2))))
    ;`(sp-wrap-overlay-face ((t (:inherit sp-wrap-overlay-face))))
    ;`(sp-wrap-tag-overlay-face ((t (:inherit sp-wrap-overlay-face))))
    `(sp-show-pair-match-face ((t (:background ,gruvbox-dark2)))) ;; Pair tags highlight
    `(sp-show-pair-mismatch-face ((t (:background ,gruvbox-neutral_red)))) ;; Highlight for bracket without pair

    ;; elscreen
    `(elscreen-tab-background-face ((t (:box nil :background ,gruvbox-dark0)))) ;; Tab bar, not the tabs
    `(elscreen-tab-control-face ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-neutral_red :underline nil)))) ;; The controls
    `(elscreen-tab-current-screen-face ((t (:box nil :background ,gruvbox-dark4 :foreground ,gruvbox-dark0)))) ;; Current tab
    `(elscreen-tab-other-screen-face ((t (:box nil :background ,gruvbox-dark2 :foreground ,gruvbox-light4 :underline nil)))) ;; Inactive tab

    ;; ag (The Silver Searcher)
    `(ag-hit-face ((t (:foreground ,gruvbox-neutral_blue))))
    `(ag-match-face ((t (:foreground ,gruvbox-neutral_red))))

    ;; Diffs
    `(diff-changed ((t (:background nil :foreground ,gruvbox-light1))))
    `(diff-added ((t (:background nil :foreground ,gruvbox-neutral_green))))
    `(diff-removed ((t (:background nil :foreground ,gruvbox-neutral_red))))
    `(diff-indicator-changed ((t (:inherit diff-changed))))
    `(diff-indicator-added ((t (:inherit diff-added))))
    `(diff-indicator-removed ((t (:inherit diff-removed))))

    ;; Term
    `(term-color-black ((t (:foreground ,gruvbox-dark1))))
    `(term-color-blue ((t (:foreground ,gruvbox-neutral_blue))))
    `(term-color-cyan ((t (:foreground ,gruvbox-neutral_aqua))))
    `(term-color-green ((t (:foreground ,gruvbox-neutral_green))))
    `(term-color-magenta ((t (:foreground ,gruvbox-neutral_purple))))
    `(term-color-red ((t (:foreground ,gruvbox-neutral_red))))
    `(term-color-white ((t (:foreground ,gruvbox-light1))))
    `(term-color-yellow ((t (:foreground ,gruvbox-neutral_yellow))))
    `(term-default-fg-color ((t (:foreground ,gruvbox-light0))))
    `(term-default-bg-color ((t (:background ,gruvbox-dark0)))))

(custom-theme-set-variables
  'gruvbox

  `(ansi-color-names-vector [,gruvbox-dark1 ,gruvbox-neutral_red
    ,gruvbox-neutral_green ,gruvbox-neutral_yellow ,gruvbox-neutral_blue
    ,gruvbox-neutral_purple ,gruvbox-neutral_aqua ,gruvbox-light1])))

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
