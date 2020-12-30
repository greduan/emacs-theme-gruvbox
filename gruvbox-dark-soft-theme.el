;;; gruvbox-dark-soft-theme.el --- A retro-groove colour theme for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Eduardo Lavaque
;; Copyright (c) 2016-2017 Jason Milkins
;; Copyright (c) 2017-2018 Martijn Terpstra

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

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'gruvbox)

(gruvbox-deftheme
 gruvbox-dark-soft
 "A retro-groove colour theme (dark version, soft contrast)"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (gruvbox-dark0_hard      "#1d2021" "#1c1c1c")
  (gruvbox-dark0           "#282828" "#262626")
  (gruvbox-dark0_soft      "#32302f" "#303030")
  (gruvbox-dark1           "#3c3836" "#3a3a3a")
  (gruvbox-dark2           "#504945" "#4e4e4e")
  (gruvbox-dark3           "#665c54" "#626262")
  (gruvbox-dark4           "#7c6f64" "#767676")

  (gruvbox-gray            "#928374" "#8a8a8a")

  (gruvbox-light0_hard     "#ffffc8" "#ffffd7")
  (gruvbox-light0          "#fdf4c1" "#ffffaf")
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

  (gruvbox-neutral_red     "#fb4933" "#d75f5f")
  (gruvbox-neutral_green   "#b8bb26" "#afaf00")
  (gruvbox-neutral_yellow  "#fabd2f" "#ffaf00")
  (gruvbox-neutral_blue    "#83a598" "#87afaf")
  (gruvbox-neutral_purple  "#d3869b" "#d787af")
  (gruvbox-neutral_aqua    "#8ec07c" "#87af87")
  (gruvbox-neutral_orange  "#fe8019" "#ff8700")

  (gruvbox-faded_red       "#cc241d" "#d75f5f")
  (gruvbox-faded_green     "#98971a" "#afaf00")
  (gruvbox-faded_yellow    "#d79921" "#ffaf00")
  (gruvbox-faded_blue      "#458588" "#87afaf")
  (gruvbox-faded_purple    "#b16286" "#d787af")
  (gruvbox-faded_aqua      "#689d6a" "#87af87")
  (gruvbox-faded_orange    "#d65d0e" "#ff8700")

  (gruvbox-dark_red        "#421E1E" "#5f0000")
  (gruvbox-dark_blue       "#2B3C44" "#000087")
  (gruvbox-dark_aqua       "#36473A" "#005f5f")

  (gruvbox-delimiter-one   "#458588" "#008787")
  (gruvbox-delimiter-two   "#b16286" "#d75f87")
  (gruvbox-delimiter-three "#8ec07c" "#87af87")
  (gruvbox-delimiter-four  "#d65d0e" "#d75f00")
  (gruvbox-white           "#FFFFFF" "#FFFFFF")
  (gruvbox-black           "#000000" "#000000")
  (gruvbox-sienna          "#DD6F48" "#d7875f")
  (gruvbox-lightblue4      "#66999D" "#5fafaf")
  (gruvbox-burlywood4      "#BBAA97" "#afaf87")
  (gruvbox-aquamarine4     "#83A598" "#87af87")
  (gruvbox-turquoise4      "#61ACBB" "#5fafaf")

  (gruvbox-ediff-current-diff-A        "#4f2121" "#4f2121")
  (gruvbox-ediff-current-diff-B        "#243c24" "#5f5f00")
  (gruvbox-ediff-current-diff-C        "#4f214f" "#4f214f")
  (gruvbox-ediff-current-diff-Ancestor "#21214f" "#21214f")
  (gruvbox-ediff-fine-diff-A           "#761919" "#761919")
  (gruvbox-ediff-fine-diff-B           "#1c691c" "#1c691c")
  (gruvbox-ediff-fine-diff-C           "#761976" "#761976")
  (gruvbox-ediff-fine-diff-Ancestor    "#12129d" "#12129d")

  (gruvbox-bg gruvbox-dark0_soft)
  (gruvbox-bg_inactive gruvbox-dark1)
  )

 (custom-theme-set-variables 'gruvbox-dark-soft
                             `(ansi-color-names-vector
                               [,gruvbox-dark1
                                ,gruvbox-bright_red
                                ,gruvbox-bright_green
                                ,gruvbox-bright_yellow
                                ,gruvbox-bright_blue
                                ,gruvbox-bright_purple
                                ,gruvbox-bright_aqua
                                ,gruvbox-light1])
			     `(pdf-view-midnight-colors '(,gruvbox-light0 . ,gruvbox-bg))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbox-dark-soft)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-dark-soft-theme.el ends here
