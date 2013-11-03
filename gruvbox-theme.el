;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;; Copyright (C)  2013 Eduán Lávaque

;; Author: Eduán Lávaque <eduanlavaque@gmail.com>
;; URL: http://github.com/Greduan/emacs-theme-gruvbox
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(let ((gruvbox-background "#282828")
      (gruvbox-foreground "#ebdbb2")
      (gruvbox-dark-1     "#3c3836")
      (gruvbox-dark-2     "#504945")
      (gruvbox-dark-3     "#665c54")
      (gruvbox-dark-4     "#7c6f64")
      (gruvbox-light-1    "#fdf4c1")
      (gruvbox-light-2    "#d5c4a1")
      (gruvbox-light-3    "#bdae93")
      (gruvbox-light-4    "#a89984")
      (gruvbox-red        "#fb4934")
      (gruvbox-orange     "#fe8019")
      (gruvbox-yellow     "#fabd2f")
      (gruvbox-green      "#b8bb26")
      (gruvbox-aqua       "#8ec07c")
      (gruvbox-blue       "#83a598")
      (gruvbox-purple     "#d3869b"))

(custom-theme-set-faces
  'gruvbox

  `(default ((t (:background ,gruvbox-background :foreground ,gruvbox-foreground))))
  `(cursor  ((t (:background ,gruvbox-light-2))))
  `(hl-line ((t (:background ,gruvbox-dark-3))))
  `(mode-line-inactive ((t (:box nil :foreground ,gruvbox-background :background ,gruvbox-dark-4))))
  `(mode-line ((t (:box nil :foreground ,gruvbox-light-1 :background ,gruvbox-dark-4))))
  `(fringe ((t (:background ,gruvbox-background))))
  `(linum ((t (:background ,gruvbox-background))))
  `(region ((t (:background ,gruvbox-dark-1))))
  `(minibuffer-prompt ((default (:foreground ,gruvbox-green :background ,gruvbox-background :bold t))))
  `(ag-hit-face ((t (:foreground ,gruvbox-green))))
  `(ag-match-face ((t (:foreground ,gruvbox-red))))

  `(font-lock-builtin-face ((t (:foreground ,gruvbox-orange))))
  `(font-lock-constant-face ((t (:foreground ,gruvbox-purple))))
  `(font-lock-comment-face ((t (:foreground ,gruvbox-dark-4))))
  `(font-lock-function-name-face ((t (:foreground ,gruvbox-green))))
  `(font-lock-keyword-face ((t (:foreground ,gruvbox-red))))
  `(font-lock-string-face ((t (:foreground ,gruvbox-green))))
  `(font-lock-variable-name-face ((t (:foreground ,gruvbox-blue))))
  `(font-lock-type-face ((t (:foreground ,gruvbox-purple))))
  `(font-lock-warning-face ((t (:foreground ,gruvbox-red :bold t))))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background "nil")))))))

(provide-theme 'gruvbox)
