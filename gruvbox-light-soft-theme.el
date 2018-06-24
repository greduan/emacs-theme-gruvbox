;;; gruvbox-light-soft-theme.el --- A retro-groove colour theme for Emacs

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
 gruvbox-light-soft
 "A retro-groove colour theme (light version)"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (gruvbox-dark0_hard      "#f9f5d7" "#ffffd7")
  (gruvbox-dark0           "#fbf1c7" "#ffffd7")
  (gruvbox-dark0_soft      "#f2e5bc" "#ffffd7")
  (gruvbox-dark1           "#ebdbb2" "#ffffaf")
  (gruvbox-dark2           "#d5c4a1" "#d7d6af")
  (gruvbox-dark3           "#bdae93" "#afaf87")
  (gruvbox-dark4           "#a89984" "#afafaf")

  (gruvbox-gray            "#928374" "#8a8a8a")

  (gruvbox-light0_hard     "#1d2021" "#1c1c1c")
  (gruvbox-light0          "#282828" "#262626")
  (gruvbox-light0_soft     "#32302f" "#303030")
  (gruvbox-light1          "#3c3836" "#3a3a3a")
  (gruvbox-light2          "#504945" "#4e4e4e")
  (gruvbox-light3          "#665c54" "#626262")
  (gruvbox-light4          "#7c6f64" "#767676")

  (gruvbox-bright_red      "#9d0006" "#870000")
  (gruvbox-bright_green    "#79740e" "#878700")
  (gruvbox-bright_yellow   "#b57614" "#af8700")
  (gruvbox-bright_blue     "#076678" "#005f87")
  (gruvbox-bright_purple   "#8f3f71" "#875f87")
  (gruvbox-bright_aqua     "#427b58" "#5f8787")
  (gruvbox-bright_orange   "#af3a03" "#af5f00")

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
  (gruvbox-darkslategray4  "#528B8B" "#5f8787")
  (gruvbox-lightblue4      "#66999D" "#5fafaf")
  (gruvbox-burlywood4      "#BBAA97" "#afaf87")
  (gruvbox-aquamarine4     "#83A598" "#87af87")
  (gruvbox-turquoise4      "#61ACBB" "#5fafaf")

  (gruvbox-bg gruvbox-dark0_soft))

 (custom-theme-set-variables 'gruvbox-light-soft
                             `(ansi-color-names-vector
                               [,gruvbox-dark1
                                ,gruvbox-bright_red
                                ,gruvbox-bright_green
                                ,gruvbox-bright_yellow
                                ,gruvbox-bright_blue
                                ,gruvbox-bright_purple
                                ,gruvbox-bright_aqua
                                ,gruvbox-light1])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbox-light-soft)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruvbox-light-theme.el ends here
