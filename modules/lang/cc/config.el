;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-highlight-func-mode
        global-semantic-decoration-mode
        global-semantic-stickyfunc-mode
        global-semantic-mru-bookmark-mode
        global-semantic-highlight-edits-mode
        ;; global-semantic-idle-completions-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-local-symbol-highlight-mode))
(add-hook 'semanticdb-project-root-functions #'projectile-project-root)
(global-set-key '[(S-down-mouse-1)] 'semantic-ia-fast-mouse-jump)
(set-company-backend! 'c-mode
  '(company-semantic
    company-etags)
  'company-c-headers
  'company-yasnippet)
(semantic-mode 1)

(use-package! stickyfunc-enhance)

(setq company-transformers '(delete-consecutive-dups))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(use-package! cc-mode
  :hook (c-mode-common . rainbow-delimiters-mode)
  ;; :hook ((c-mode c++-mode) . +cc-fontify-constants-h)
  :config
  (set-electric! '(c-mode c++-mode objc-mode java-mode) :chars '(?\n ?\} ?\{))
  (set-ligatures! '(c-mode c++-mode)
    ;; Functional
    ;; :def "void "
    ;; Types
    :null "nullptr"
    :true "true" :false "false"
    :int "int" :float "float"
    :str "std::string"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    :yield "#require"
    ;; Custom style, based off of linux
    (setq c-basic-offset tab-width
          c-backspace-function #'delete-backward-char))

  (c-add-style
   "uftrace" '((c-basic-offset . 8)
               (c-label-minimum-indentation . 0)
               (c-offsets-alist
                . ((arglist-close         . c-lineup-arglist-tabs-only)
                   (arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))
                   (arglist-intro         . +)
                   (brace-list-intro      . +)
                   (c                     . c-lineup-C-comments)
                   (case-label            . 0)
                   (comment-intro         . c-lineup-comment)
                   (cpp-define-intro      . +)
                   (cpp-macro             . -1000)
                   (cpp-macro-cont        . +)
                   (defun-block-intro     . +)
                   (else-clause           . 0)
                   (func-decl-cont        . +)
                   (inclass               . +)
                   (inher-cont            . c-lineup-multi-inher)
                   (knr-argdecl-intro     . 0)
                   (label                 . -1000)
                   (statement             . 0)
                   (statement-block-intro . +)
                   (statement-case-intro  . +)
                   (statement-cont        . +)
                   (substatement          . +)
                   ))
               (indent-tabs-mode . t)
               (show-trailing-whitespace . t))))

(set-lookup-handlers! '(c-mode c++-mode)
  :definition #'semantic-ia-fast-jump
  :references #'semantic-symref-symbol)

(advice-remove #'xref-push-marker-stack #'doom-set-jump-a)
(advice-add #'semantic-complete-jump :around #'doom-set-jump-a)

(setq better-jumper-add-jump-behavior 'replace)

(defun projectile-semantic-parse-all ()
  "Force semantic to parse all code files in a projectile project."
  (interactive)
  (let ((recentf-mode-p recentf-mode))
    (recentf-mode nil)
    (projectile-process-current-project-files
     (lambda (f) (if (string-match-p "\\.[Shc]$" f)
                (semantic-file-tag-table (concat (projectile-project-root) f)))))
    (recentf-mode recentf-mode-p)))

(use-package! srefactor
  :bind ("M-RET" . srefactor-refactor-at-point))
