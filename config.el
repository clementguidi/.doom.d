;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Clément Guidi"
      user-mail-address "cguidi@ciena.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'normal))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(menu-bar-mode t)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

(which-function-mode t)

;; Projectile
(setq projectile-tags-command "etags.ctags -Re -f \"%s\" %s \"%s\"")
;; Projectile

;; Org
(setq org-clock-in-switch-to-state "STRT")
(setq org-clock-out-switch-to-state "HOLD")
;; Org

;; Centaur
(after! gud
  (add-hook! 'gud-mode-hook #'centaur-tabs-local-mode))
(after! gdb-mi
  (add-hook! (gdb-locals-mode
              gdb-registers-mode
              gdb-inferior-io-mode
              gdb-frames-mode
              gdb-threads-mode
              gdb-breakpoints-mode)
             #'centaur-tabs-local-mode))
(setq centaur-tabs-cycle-scope 'tabs)
(after! centaur-tabs
  (add-hook '+doom-dashboard-mode-hook 'centaur-tabs-local-mode)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-buffer-reordering)
  (define-key centaur-tabs-mode-map (kbd "<C-iso-lefttab>") 'centaur-tabs-backward)
  (define-key centaur-tabs-mode-map (kbd "<C-tab>") 'centaur-tabs-forward))
(setq centaur-tabs-adjust-buffer-order t)
;; Centaur

;; Meson
(add-hook 'meson-mode-hook 'company-mode)
;; Meson

;; GUD
(define-key global-map (kbd "C-x C-a SPC") 'gdb-restore-windows)
;; GUD

;; Org
(require 'ox)
(org-export-define-derived-backend 'html-confluence 'html
  :menu-entry
  '(?h "Export to HTML"
    ((?C "As Confluence buffer"
         (lambda (a s v b) (org-html-export-as-confluence a s v b)))
     (?c "As Confluence file" (lambda (a s v b) (org-html-export-to-confluence a s v b)))))
  :translate-alist '((headline . org-html-confluence-headline)
                     (inner-template . org-html-confluence-inner-template)
                     (keyword . org-html-confluence-keyword)
                     (link . org-html-confluence-link))
  :options-alist '((:headline-levels nil "H" 2)
                   (:html-confluence-jira-number "JIRA" nil nil space)
                   (:with-inlinetasks nil "inline" nil)
                   (:with-statistics-cookies nil "stat" nil)
                   (:with-sub-superscript nil "^" nil)
                   (:with-tasks nil "tasks"
                                '("TODO" "DONE" "PROJ" "STRT" "WAIT" "HOLD"))
                   (:with-todo-keywords nil "todo" nil)))

(defun org-html-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML Confluence buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Confluence Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'html-confluence "*Org HTML Confluence Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(defun org-html-export-to-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML Confluence file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html-confluence file
      async subtreep visible-only body-only ext-plist)))

(defun org-html-confluence-toc ()
  "<span><ac:structured-macro ac:name=\"toc\" ac:schema-version=\"1\"/></span>")

(defun org-html-confluence-jira (jira)
  (concat
   "<div>"
   "<p>\n"
   "<ac:structured-macro ac:name=\"jira\" ac:schema-version=\"1\">\n"
   "<ac:parameter ac:name=\"server\">Agile JIRA</ac:parameter>\n"
   "<ac:parameter ac:name=\"key\">"
   jira
   "</ac:parameter>\n"
   "</ac:structured-macro>\n"
   "</p>"
   "</div>"))

(defun org-html-confluence-footnote-section (info)
  (let ((jiras (plist-get info :html-confluence-jira-number)))
    (mapconcat #'org-html-confluence-jira (split-string jiras) "\n")))

(defun org-html-confluence-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (org-html-confluence-toc)
   "\n"
   ;; Document contents.
   contents
   ;; Footnotes section.
   "\n"
   (org-html-confluence-footnote-section info)))

(defun org-html-confluence-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC") (org-html-confluence-toc)))))

(defun org-html-confluence-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (id (org-html--reference headline info)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (format "<%s>\n" html-type))
	     (org-html-format-list-item
	      contents (if numberedp 'ordered 'unordered)
	      nil info nil
	      (concat (org-html--anchor id nil nil info) full-text)) "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((first-content (car (org-element-contents headline))))
          (format "<br/><%s>%s<br/>%s</%s>\n"
                  (org-html--container headline info)
                  (format "\n<h%d>%s</h%d>\n"
                          level
			  full-text
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(defun org-html-confluence--format-image (source)
  (concat
   "<ac:image ac:height=\"150\">\n"
   (format "<ri:attachment ri:filename=\"%s\"/>\n" source)
   "</ac:image>"))

(defun org-html-confluence-link (link desc info)
  "Transcode a LINK object from Org to HTML Confluence.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (if (and (string= "file" type)
             (plist-get info :html-inline-images)
	     (org-export-inline-image-p
	      link (plist-get info :html-inline-image-rules)))
        (org-html-confluence--format-image path)
      (org-html-link link desc info))))
;;

;; yasnippet
(defun doc-format-args (args index)
  (let ((arg (car args))
        (rest (cdr args)))
    (concat
     "${"
     (number-to-string index)
     ":"
     "\n * @${"
     (number-to-string (1+ index))
     ":"
     (car arg)
     "} - $"
     (number-to-string (+ 2 index))
     "}"
     (if rest
         (doc-format-args rest (+ index 3))))))
;; yasnippet


;; compilation
(remove-hook 'compilation-filter-hook 'doom-apply-ansi-color-to-compilation-buffer-h)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
;; compilation
