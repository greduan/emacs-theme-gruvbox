;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defcustom gruvbox-contrast 'medium
  "Contrast level for the theme background."
  :options '(soft medium hard))


(autothemer-deftheme

    ;; Theme Name
    gruvbox

    ;; Theme Description
    "A gruvebox theme defined using autothemer.el"

    (

        ;; Display filters
        (
            ;; Display supports 16777216 colors
            ((class color) (min-colors 16777216))
            ;; Display supports 256 colors
            ((class color) (min-colors 256))
            ;; Display supports 8 colors
            ((class color) (min-colors 8))
            ;; Any other display type
            t
        )

        ;; Color definitions

        (gruvbox-bg             "#282828" "color-235" "black") ; XXX todo

        (gruvbox-dark0-hard     "#1d2021" "color-234" "black")
        (gruvbox-dark0          "#282828" "color-235" "black")
        (gruvbox-dark0-soft     "#32302f" "color-236" "black")
        (gruvbox-dark1          "#3c3836" "color-237" "black")
        (gruvbox-dark2          "#504945" "color-239" "black")
        (gruvbox-dark3          "#665c54" "color-241" "black")
        (gruvbox-dark4          "#7c6f64" "color-243" "black")

        (gruvbox-medium         "#928374" "color-245" "white"); or 244

        (gruvbox-light0-hard    "#f9f5d7" "color-230" "white")
        (gruvbox-light0         "#fbf1c7" "color-229" "white")
        (gruvbox-light0-soft    "#f2e5bc" "color-228" "white")
        (gruvbox-light1         "#ebdbb2" "color-223" "white")
        (gruvbox-light2         "#d5c4a1" "color-250" "white")
        (gruvbox-light3         "#bdae93" "color-248" "white")
        (gruvbox-light4         "#a89984" "color-246" "white")

        (gruvbox-bright-red     "#fb4934" "color-167" "red")
        (gruvbox-bright-green   "#b8bb26" "color-142" "green")
        (gruvbox-bright-yellow  "#fabd2f" "color-214" "yellow")
        (gruvbox-bright-blue    "#83a598" "color-109" "blue")
        (gruvbox-bright-purple  "#d3869b" "color-175" "magenta")
        (gruvbox-bright-aqua    "#8ec07c" "color-108" "cyan")
        (gruvbox-bright-orange  "#fe8019" "color-208" "yellow") ; XXX is "yellow" a good choice here?

        (gruvbox-neutral-red    "#cc241d" "color-124" "red")
        (gruvbox-neutral-green  "#98971a" "color-106" "green")
        (gruvbox-neutral-yellow "#d79921" "color-172" "yellow")
        (gruvbox-neutral-blue   "#458588" "color-66"  "blue")
        (gruvbox-neutral-purple "#b16286" "color-132" "magenta")
        (gruvbox-neutral-aqua   "#689d6a" "color-72"  "cyan")
        (gruvbox-neutral-orange "#d65d0e" "color-166" "yellow") ; XXX is "yellow" a good choice here?

        (gruvbox-faded-red      "#9d0006" "color-88"  "red")
        (gruvbox-faded-green    "#79740e" "color-100" "green")
        (gruvbox-faded-yellow   "#b57614" "color-136" "yellow")
        (gruvbox-faded-blue     "#076678" "color-24"  "blue")
        (gruvbox-faded-purple   "#8f3f71" "color-96"  "magenta")
        (gruvbox-faded-aqua     "#427b58" "color-66"  "cyan")
        (gruvbox-faded-orange   "#af3a03" "color-130" "yellow") ; XXX is "yellow" a good choice here?

        (gruvbox-dark-red        "#421E1E" "color-52" "red")  ; XXX: Do we...
        (gruvbox-dark-blue       "#2B3C44" "color-4"  "blue") ; XXX: really need...
        (gruvbox-dark-aqua       "#36473A" "color-23" "cyan") ; XXX: these colors?

        (gruvbox-delimiter-one   "#458588" "color-30"       nil) ; XXX: do
        (gruvbox-delimiter-two   "#b16286" "color-168"      nil) ; XXX: 
        (gruvbox-delimiter-three "#8ec07c" "color-108"      nil) ; XXX: we
        (gruvbox-delimiter-four  "#d65d0e" "color-166"      nil) ; XXX: 
        (gruvbox-white           "#FFFFFF" "white"          nil) ; XXX: really
        (gruvbox-black           "#000000" "black"          nil) ; XXX: 
        (gruvbox-sienna          "#DD6F48" "sienna"         nil) ; XXX: need
        (gruvbox-darkslategray4  "#528B8B" "DarkSlateGray4" nil) ; XXX: 
        (gruvbox-lightblue4      "#66999D" "LightBlue4"     nil) ; XXX: these
        (gruvbox-burlywood4      "#BBAA97" "burlywood4"     nil) ; XXX: 
        (gruvbox-aquamarine4     "#83A598" "aquamarine4"    nil) ; XXX: colors?
        (gruvbox-turquoise4      "#61ACBB" "turquoise4"     nil) ; XXX: 
    )


    ;; Face Specifications
    (

        ;; User Interface
        (default             (:background gruvbox-bg :foreground gruvbox-light0))
        (cursor              (:background gruvbox-light0))
        (mode-line           (:box nil :background gruvbox-dark2 :foreground gruvbox-light2))
        (mode-line-inactive  (:box nil :background gruvbox-dark1 :foreground gruvbox-light4))
        (fringe              (:background gruvbox-bg))
        (linum               (:background gruvbox-bg :foreground gruvbox-dark4))
        (hl-line             (:background gruvbox-dark1))
        (region              (:background gruvbox-dark2))
        (secondary-selection (:background gruvbox-dark1))
        (minibuffer-prompt   (:background gruvbox-bg :foreground gruvbox-neutral-green :bold t))
        (vertical-border     (:foreground gruvbox-dark2))
        (link                (:foreground gruvbox-faded-blue :underline t))
        (shadow              (:foreground gruvbox-dark4))

        ;; Built-In Syntax
        (font-lock-builtin-face       (:foreground gruvbox-neutral-orange))
        (font-lock-constant-face      (:foreground gruvbox-neutral-purple))
        (font-lock-comment-face       (:foreground gruvbox-dark4))
        (font-lock-function-name-face (:foreground gruvbox-neutral-yellow))
        (font-lock-keyword-face       (:foreground gruvbox-neutral-red))
        (font-lock-string-face        (:foreground gruvbox-neutral-green))
        (font-lock-variable-name-face (:foreground gruvbox-neutral-blue))
        (font-lock-type-face          (:foreground gruvbox-neutral-purple))
        (font-lock-warning-face       (:foreground gruvbox-neutral-red :bold t))

        ;; Whitespace-Mode
        (whitespace-space            (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-hspace           (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-tab              (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-newline          (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-trailing         (:background gruvbox-dark1 :foreground gruvbox-neutral-red))
        (whitespace-line             (:background gruvbox-dark1 :foreground gruvbox-neutral-red))
        (whitespace-space-before-tab (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-indentation      (:background gruvbox-bg    :foreground gruvbox-dark4))
        (whitespace-empty            (:background nil              :foreground nil))
        (whitespace-space-after-tab  (:background gruvbox-bg    :foreground gruvbox-dark4))

        ;; Rainbow Delimiters
        (rainbow-delimiters-depth-1-face   (:foreground gruvbox-delimiter-one))
        (rainbow-delimiters-depth-2-face   (:foreground gruvbox-delimiter-two))
        (rainbow-delimiters-depth-3-face   (:foreground gruvbox-delimiter-three))
        (rainbow-delimiters-depth-4-face   (:foreground gruvbox-delimiter-four))
        (rainbow-delimiters-depth-5-face   (:foreground gruvbox-delimiter-one))
        (rainbow-delimiters-depth-6-face   (:foreground gruvbox-delimiter-two))
        (rainbow-delimiters-depth-7-face   (:foreground gruvbox-delimiter-three))
        (rainbow-delimiters-depth-8-face   (:foreground gruvbox-delimiter-four))
        (rainbow-delimiters-depth-9-face   (:foreground gruvbox-delimiter-one))
        (rainbow-delimiters-depth-10-face  (:foreground gruvbox-delimiter-two))
        (rainbow-delimiters-depth-11-face  (:foreground gruvbox-delimiter-three))
        (rainbow-delimiters-depth-12-face  (:foreground gruvbox-delimiter-four))
        (rainbow-delimiters-unmatched-face (:foreground gruvbox-light0 :background nil))

        ;; Linum-Relative
        (linum-relative-current-face (:background gruvbox-dark1 :foreground gruvbox-light4))

        ;; Highlight-Indentation-Mode
        (highlight-indentation-current-column-face (:background gruvbox-dark2 ))
        (highlight-indentation-face                (:background gruvbox-dark1 ))

        ;; Smartparens
        (sp-pair-overlay-face       (:background gruvbox-dark2))
        (sp-wrap-overlay-face       (:inherit 'sp-wrap-overlay-face))
        (sp-wrap-tag-overlay-face   (:inherit 'sp-wrap-overlay-face))
        (sp-show-pair-match-face    (:background gruvbox-dark2)) ;; Pair tags highlight
        (sp-show-pair-mismatch-face (:background gruvbox-neutral-red)) ;; Highlight for bracket without pair

        ;; Elscreen
        (elscreen-tab-background-face     (:box nil :background gruvbox-bg)) ;; Tab bar, not the tabs
        (elscreen-tab-current-screen-face (:box nil :background gruvbox-dark4 :foreground gruvbox-dark0))
        (elscreen-tab-other-screen-face   (:box nil :background gruvbox-dark2 :foreground gruvbox-light4      :underline nil))
        (elscreen-tab-control-face        (:box nil :background gruvbox-dark2 :foreground gruvbox-neutral-red :underline nil))

        ;; AG (The Silver Searcher)
        (ag-hit-face   (:foreground gruvbox-neutral-blue))
        (ag-match-face (:foreground gruvbox-neutral-red))

        ;; Diff
        (diff-changed           (:background nil :foreground gruvbox-light1))
        (diff-added             (:background nil :foreground gruvbox-neutral-green))
        (diff-removed           (:background nil :foreground gruvbox-neutral-red))
        (diff-indicator-changed (:inherit 'diff-changed))
        (diff-indicator-added   (:inherit 'diff-added))
        (diff-indicator-removed (:inherit 'diff-removed))

        ;; Diff-HL-Mode (https://github.com/dgutov/diff-hl)
        (diff-hl-changed (:background nil :foreground gruvbox-faded-blue))
        (diff-hl-insert  (:background nil :foreground gruvbox-faded-green))
        (diff-hl-delete  (:background nil :foreground gruvbox-faded-red))

        ;; JS2-Mode
        (js2-warning                  (:underline (:color gruvbox-bright-yellow :style 'wave)))
        (js2-error                    (:underline (:color gruvbox-bright-red :style 'wave)))
        (js2-external-variable        (:underline (:color gruvbox-bright-aqua :style 'wave)))
        (js2-jsdoc-tag                (:background nil :foreground gruvbox-medium ))
        (js2-jsdoc-type               (:background nil :foreground gruvbox-light4 ))
        (js2-jsdoc-value              (:background nil :foreground gruvbox-light3 ))
        (js2-function-param           (:background nil :foreground gruvbox-bright-aqua ))
        (js2-function-call            (:background nil :foreground gruvbox-bright-blue ))
        (js2-instance-member          (:background nil :foreground gruvbox-bright-orange ))
        (js2-private-member           (:background nil :foreground gruvbox-faded-yellow ))
        (js2-private-function-call    (:background nil :foreground gruvbox-faded-aqua ))
        (js2-jsdoc-html-tag-name      (:background nil :foreground gruvbox-light4 ))
        (js2-jsdoc-html-tag-delimiter (:background nil :foreground gruvbox-light3 ))

        ;; Popup
        (popup-face                (:foreground gruvbox-light1 :background gruvbox-dark1))
        (popup-menu-mouse-face     (:foreground gruvbox-light0 :background gruvbox-faded-green))
        (popup-menu-selection-face (:foreground gruvbox-light0 :background gruvbox-faded-green))
        (popup-tip-face            (:foreground gruvbox-light2 :background gruvbox-dark2))

        ;; Helm
        (helm-M-x-key              (:foreground gruvbox-neutral-orange))
        (helm-action               (:foreground gruvbox-white :underline t))
        (helm-bookmark-addressbook (:foreground gruvbox-neutral-red))
        (helm-bookmark-directory   (:foreground gruvbox-bright-purple))
        (helm-bookmark-file        (:foreground gruvbox-faded-blue))
        (helm-bookmark-gnus        (:foreground gruvbox-faded-purple))
        (helm-bookmark-info        (:foreground gruvbox-turquoise4))
        (helm-bookmark-man         (:foreground gruvbox-sienna))
        (helm-bookmark-w3m         (:foreground gruvbox-neutral-yellow))
        (helm-buffer-directory     (:foreground gruvbox-white :background gruvbox-bright-blue))
        (helm-buffer-not-saved     (:foreground gruvbox-faded-red))
        (helm-buffer-process       (:foreground gruvbox-burlywood4))
        (helm-buffer-saved-out     (:foreground gruvbox-bright-red))
        (helm-buffer-size          (:foreground gruvbox-bright-purple))
        (helm-candidate-number     (:foreground gruvbox-neutral-green))
        (helm-ff-directory         (:foreground gruvbox-neutral-purple))
        (helm-ff-executable        (:foreground gruvbox-turquoise4))
        (helm-ff-file              (:foreground gruvbox-sienna))
        (helm-ff-invalid-symlink   (:foreground gruvbox-white :background gruvbox-bright-red))
        (helm-ff-prefix            (:foreground gruvbox-black :background gruvbox-neutral-yellow))
        (helm-ff-symlink           (:foreground gruvbox-neutral-orange))
        (helm-grep-cmd-line        (:foreground gruvbox-neutral-green))
        (helm-grep-file            (:foreground gruvbox-faded-purple))
        (helm-grep-finish          (:foreground gruvbox-turquoise4))
        (helm-grep-lineno          (:foreground gruvbox-neutral-orange))
        (helm-grep-match           (:foreground gruvbox-neutral-yellow))
        (helm-grep-running         (:foreground gruvbox-neutral-red))
        (helm-header               (:foreground gruvbox-aquamarine4))
        (helm-helper               (:foreground gruvbox-aquamarine4))
        (helm-history-deleted      (:foreground gruvbox-black :background gruvbox-bright-red))
        (helm-history-remote       (:foreground gruvbox-faded-red))
        (helm-lisp-completion-info (:foreground gruvbox-faded-orange))
        (helm-lisp-show-completion (:foreground gruvbox-bright-red))
        (helm-locate-finish        (:foreground gruvbox-white :background gruvbox-aquamarine4))
        (helm-match                (:foreground gruvbox-neutral-orange))
        (helm-moccur-buffer        (:foreground gruvbox-bright-aqua :underline t))
        (helm-prefarg              (:foreground gruvbox-turquoise4))
        (helm-selection            (:foreground gruvbox-white :background gruvbox-dark2))
        (helm-selection-line       (:foreground gruvbox-white :background gruvbox-dark2))
        (helm-separator            (:foreground gruvbox-faded-red))
        (helm-source-header        (:foreground gruvbox-light2))
        (helm-visible-mark         (:foreground gruvbox-black :background gruvbox-light3))

        ;; Company-Mode
        (company-scrollbar-bg             (:background gruvbox-dark1))
        (company-scrollbar-fg             (:background gruvbox-dark0-soft))
        (company-tooltip                  (:background gruvbox-dark0-soft))
        (company-tooltip-annotation       (:foreground gruvbox-neutral-green))
        (company-tooltip-selection        (:foreground gruvbox-neutral-purple))
        (company-tooltip-common           (:foreground gruvbox-neutral-blue :underline t))
        (company-tooltip-common-selection (:foreground gruvbox-neutral-blue :underline t))
        (company-preview-common           (:foreground gruvbox-neutral-purple))

        ;; Term
        (term-color-black      (:foreground gruvbox-dark1))
        (term-color-blue       (:foreground gruvbox-neutral-blue))
        (term-color-cyan       (:foreground gruvbox-neutral-aqua))
        (term-color-green      (:foreground gruvbox-neutral-green))
        (term-color-magenta    (:foreground gruvbox-neutral-purple))
        (term-color-red        (:foreground gruvbox-neutral-red))
        (term-color-white      (:foreground gruvbox-light1))
        (term-color-yellow     (:foreground gruvbox-neutral-yellow))
        (term-default-fg-color (:foreground gruvbox-light0))
        (term-default-bg-color (:background gruvbox-bg))

        ;; Message-Mode
        (message-header-to         (:inherit 'font-lock-variable-name-face))
        (message-header-cc         (:inherit 'font-lock-variable-name-face))
        (message-header-subject    (:foreground gruvbox-neutral-orange :weight 'bold))
        (message-header-newsgroups (:foreground gruvbox-neutral-yellow :weight 'bold))
        (message-header-other      (:inherit 'font-lock-variable-name-face))
        (message-header-name       (:inherit 'font-lock-keyword-face))
        (message-header-xheader    (:foreground gruvbox-faded-blue))
        (message-separator         (:inherit 'font-lock-comment-face))
        (message-cited-text        (:inherit 'font-lock-comment-face))
        (message-mml               (:foreground gruvbox-faded-green :weight 'bold))

        ;; Org-Mode
        (org-hide                 (:foreground gruvbox-dark0))
        (org-level-1              (:foreground gruvbox-neutral-blue))
        (org-level-2              (:foreground gruvbox-neutral-yellow))
        (org-level-3              (:foreground gruvbox-neutral-purple))
        (org-level-4              (:foreground gruvbox-neutral-red))
        (org-level-5              (:foreground gruvbox-neutral-green))
        (org-level-6              (:foreground gruvbox-neutral-aqua))
        (org-level-7              (:foreground gruvbox-faded-blue))
        (org-level-8              (:foreground gruvbox-neutral-orange))
        (org-special-keyword      (:inherit 'font-lock-comment-face))
        (org-drawer               (:inherit 'font-lock-function-face))
        (org-column               (:background gruvbox-dark0))
        (org-column-title         (:background gruvbox-dark0 :underline t :weight 'bold))
        (org-warning              (:bold t :foreground gruvbox-neutral-red :weight 'bold :underline nil))
        (org-archived             (:foreground gruvbox-light0 :weight 'bold))
        (org-link                 (:foreground gruvbox-faded-aqua :underline t))
        (org-footnote             (:foreground gruvbox-neutral-aqua :underline t))
        (org-ellipsis             (:foreground gruvbox-light4 :underline t))
        (org-date                 (:foreground gruvbox-neutral-blue :underline t))
        (org-sexp-date            (:foreground gruvbox-faded-blue :underline t))
        (org-tag                  (:bold t :weight 'bold))
        (org-list-dt              (:bold t :weight 'bold))
        (org-todo                 (:bold t :foreground gruvbox-neutral-red :weight 'bold))
        (org-done                 (:bold t :foreground gruvbox-neutral-aqua :weight 'bold))
        (org-agenda-done          (:foreground gruvbox-neutral-aqua))
        (org-headline-done        (:foreground gruvbox-neutral-aqua))
        (org-table                (:foreground gruvbox-neutral-blue))
        (org-formula              (:foreground gruvbox-neutral-yellow))
        (org-document-title       (:foreground gruvbox-faded-blue))
        (org-document-info        (:foreground gruvbox-faded-blue))
        (org-agenda-structure     (:inherit 'font-lock-comment-face))
        (org-agenda-date-today    (:foreground gruvbox-light0 :weight 'bold :italic t))
        (org-scheduled            (:foreground gruvbox-neutral-yellow))
        (org-scheduled-today      (:foreground gruvbox-neutral-blue))
        (org-scheduled-previously (:foreground gruvbox-faded-red))
        (org-upcoming-deadline    (:inherit 'font-lock-keyword-face))
        (org-deadline-announce    (:foreground gruvbox-faded-red))
        (org-time-grid            (:foreground gruvbox-faded-orange))

        ;; Org-Habit
        (org-habit-clear-face          (:background gruvbox-faded-blue))
        (org-habit-clear-future-face   (:background gruvbox-neutral-blue))
        (org-habit-ready-face          (:background gruvbox-faded-green))
        (org-habit-ready-future-face   (:background gruvbox-neutral-green))
        (org-habit-alert-face          (:background gruvbox-faded-yellow))
        (org-habit-alert-future-face   (:background gruvbox-neutral-yellow))
        (org-habit-overdue-face        (:background gruvbox-faded-red))
        (org-habit-overdue-future-face (:background gruvbox-neutral-red))

        ;; Elfeed
        (elfeed-search-title-face        (:foreground gruvbox-medium))
        (elfeed-search-unread-title-face (:foreground gruvbox-light0))
        (elfeed-search-date-face         (:inherit 'font-lock-builtin-face :underline t))
        (elfeed-search-feed-face         (:inherit 'font-lock-variable-name-face))
        (elfeed-search-tag-face          (:inherit 'font-lock-keyword-face))
        (elfeed-search-last-update-face  (:inherit 'font-lock-comment-face))
        (elfeed-search-unread-count-face (:inherit 'font-lock-comment-face))
        (elfeed-search-filter-face       (:inherit 'font-lock-string-face))

        ;; Smart-Mode-Line
        (sml/global          (:foreground gruvbox-burlywood4 :inverse-video nil))
        (sml/modes           (:foreground gruvbox-bright-green))
        (sml/filename        (:foreground gruvbox-bright-red :weight 'bold))
        (sml/prefix          (:foreground gruvbox-light1))
        (sml/read-only       (:foreground gruvbox-neutral-blue))
        (persp-selected-face (:foreground gruvbox-neutral-orange))

        ;; Isearch
        (isearch        (:foreground gruvbox-black  :background gruvbox-neutral-orange))
        (lazy-highlight (:foreground gruvbox-black  :background gruvbox-neutral-yellow))
        (isearch-fail   (:foreground gruvbox-light0 :background gruvbox-bright-red))

        ;; Anzu-Mode
        (anzu-mode-line         (:foreground gruvbox-bright-yellow :weight 'bold))
        (anzu-match-1           (:background gruvbox-bright-green))
        (anzu-match-2           (:background gruvbox-faded-yellow))
        (anzu-match-3           (:background gruvbox-aquamarine4))
        (anzu-replace-to        (:foreground gruvbox-bright-yellow))
        (anzu-replace-highlight (:inherit 'isearch))

        ;; Ace-Jump-Mode
        (ace-jump-face-background (:foreground gruvbox-light4     :background gruvbox-bg :inverse-video nil))
        (ace-jump-face-foreground (:foreground gruvbox-bright-red :background gruvbox-bg :inverse-video nil :box 1))

        ;; Ace-window
        (aw-background-face   (:forground  gruvbox-light1        :background gruvbox-bg :inverse-video nil))
        (aw-leading-char-face (:foreground gruvbox-bright-orange :background gruvbox-bg :height 4.0 :box (:line-width 1 :color gruvbox-bright-orange)))
    
        ;; Show-Paren
        (show-paren-match    (:background gruvbox-dark3                                   :weight 'bold))
        (show-paren-mismatch (:background gruvbox-bright-red :foreground gruvbox-dark3 :weight 'bold))

        ;; Dired+
        (diredp-file-name              (:foreground gruvbox-light2))
        (diredp-file-suffix            (:foreground gruvbox-light4))
        (diredp-compressed-file-suffix (:foreground gruvbox-faded-blue))
        (diredp-dir-name               (:foreground gruvbox-faded-blue))
        (diredp-dir-heading            (:foreground gruvbox-bright-blue))
        (diredp-symlink                (:foreground gruvbox-bright-orange))
        (diredp-date-time              (:foreground gruvbox-light3))
        (diredp-number                 (:foreground gruvbox-faded-blue))
        (diredp-no-priv                (:foreground gruvbox-dark4))
        (diredp-other-priv             (:foreground gruvbox-dark2))
        (diredp-rare-priv              (:foreground gruvbox-dark4))
        (diredp-ignored-file-name      (:foreground gruvbox-dark4))
   
        (diredp-dir-priv               (:foreground gruvbox-faded-blue  :background gruvbox-dark-blue))
        (diredp-exec-priv              (:foreground gruvbox-faded-blue  :background gruvbox-dark-blue))
        (diredp-link-priv              (:foreground gruvbox-faded-aqua  :background gruvbox-dark-aqua))
        (diredp-read-priv              (:foreground gruvbox-bright-red  :background gruvbox-dark-red))
        (diredp-write-priv             (:foreground gruvbox-bright-aqua :background gruvbox-dark-aqua))

        ;; ERC
        (erc-default-face           (:inherit 'default))
        (erc-fool-face              (:inherit 'shadow))
        (erc-action-face            (:slant 'italic))
        (erc-bold-face              (:weight 'bold))
        (erc-button                 (:underline t))
        (erc-command-indicator-face (:weight 'bold))
        (erc-underline-face         (:underline t))
        (erc-inverse-face           (:inverse-video t))
        (erc-nick-default-face      (:foreground gruvbox-bright-yellow))
        (erc-current-nick-face      (:foreground gruvbox-bright-red :weight 'bold))
        (erc-input-face             (:foreground gruvbox-bright-orange))
        (erc-keyword-face           (:foreground gruvbox-bright-purple))
        (erc-notice-face            (:foreground gruvbox-medium))
        (erc-timestamp-face         (:foreground gruvbox-medium))
        (erc-pal-face               (:foreground gruvbox-bright-green))
        (erc-error-face             (:foreground gruvbox-bright-red :weight 'bold))
        (erc-dangerous-host-face    (:foreground gruvbox-bright-red :weight 'bold))
        (erc-my-nick-prefix-face    (:inherit 'erc-nick-prefix-face))
        (erc-my-nick-face           (:inherit 'erc-current-nick-face))
        (erc-nick-prefix-face       (:inherit 'erc-default-face))
        (erc-prompt-face            (:inherit 'erc-input-face))
        (erc-header-line            (:inherit 'header-line))
        (erc-direct-msg-face        (:inherit 'erc-default-face))
        (erc-nick-msg-face          (:inherit 'erc-nick-default-face))
        
        ;; Ido-Mode
        (ido-first-match (:foreground gruvbox-bright-yellow :weight 'bold))
        (ido-only-match  (:foreground gruvbox-bright-orange :weight 'bold))
        (ido-subdir      (:foreground gruvbox-neutral-blue :weight 'bold))
        (ido-indicator   (:background gruvbox-bright-yellow :foreground gruvbox-dark0 :width 'condensed))

    )

    ;; Set some variables
    ;; (custom-theme-set-variables
    ;;     'gruvbox

    ;;     ;; Frame-Background
    ;;     '(frame-background-mode 'dark)

    ;;     ;; FCI-Mode
    ;;     '(fci-rule-color gruvbox-neutral-blue)

    ;;     ;; Ansi Colors
    ;;     '(ansi-color-names-vector
    ;;          [
    ;;              gruvbox-dark0
    ;;              gruvbox-bright-red
    ;;              gruvbox-bright-green
    ;;              gruvbox-bright-yellow
    ;;              gruvbox-bright-blue
    ;;              gruvbox-bright-purple
    ;;              gruvbox-bright-aqua
    ;;              gruvbox-light1])
    ;; )
)


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
