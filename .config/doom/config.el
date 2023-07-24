;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Rafael Palomar"
      user-mail-address "rafael.palomar@ous-research.no")

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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Specify your desired default font and its size
(setq doom-font (font-spec :family "Fira Code" :size 18))

;; Set the size of the font for big-mode
(setq doom-big-font (font-spec :family "Fira Code" :size 24))

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

;; (use-package! org-ref

;;   ;; this bit is highly recommended: make sure Org-ref is loaded after Org
;;   :after org

;;   ;; Put any Org-ref commands here that you would like to be auto loaded:
;;   ;; you'll be able to call these commands before the package is actually loaded.
;;   :commands
;;   (org-ref-cite-hydra/body
;;    org-ref-bibtex-hydra/body)

;;   ;; if you don't need any autoloaded commands, you'll need the following
;;   ;; :defer t

;;   ;; This initialization bit puts the `orhc-bibtex-cache-file` into `~/.doom/.local/cache/orhc-bibtex-cache
;;   ;; Not strictly required, but Org-ref will pollute your home directory otherwise, creating the cache file in ~/.orhc-bibtex-cache
;;   :init
;;   (let ((cache-dir (concat doom-cache-dir "org-ref")))
;;     (unless (file-exists-p cache-dir)
;;       (make-directory cache-dir t)
;;       (setq orhc-bibtex-cache-file (concat cache-dir "/orhc-bibtex-cache"))))
;;   (setq org-ref-completion-library 'org-ref-ivy-cite)
;;   (setq reftex-default-bibliography '("~/Dropbox/bibliography/bibfile.bib"))
;;   (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org")
;;   (setq org-ref-default-bibliography '("~/Dropbox/bibliography/bibfile.bib"))
;;   (setq org-ref-pdf-directory "~/Dropbox/bibliography/pdfs/"))

;; (after! org
;;   (require 'org-ref))

;; org-roam
(use-package! org-roam
  :custom
  (org-roam-directory "~/Dropbox/Org-Roam"))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref))

(use-package! auth-source
  :config
  (add-to-list 'auth-sources "~/.authinfo.gpg")

  (defun fetch-gptel-api-key ()
    "Retrieve the OpenAI API key from auth-source."
    (setq! gptel-api-key
           (funcall #'auth-source-pick-first-password
                    :host "api.openai.com"
                    :user "default")))

  (add-hook 'server-after-make-frame-hook #'fetch-gptel-api-key))

(after! org
  (require 'ox-reveal))

(require 'ob-mermaid)
(setq org-babel-mermaid-cli-path "/path/to/your/mermaid.cli.js")


(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(setq rafaelpalomar/default-bibliography `(,(expand-file-name "~/Dropbox/Library/Library.bib" org-directory)))
(after! bibtex-completion
  (setq! bibtex-completion-notes-path org-roam-directory
         bibtex-completion-bibliography rafaelpalomar/default-bibliography
         org-cite-global-bibliography rafaelpalomar/default-bibliography
         bibtex-completion-pdf-field "file"))

(after! bibtex-completion
  (after! org-roam
    (setq! bibtex-completion-notes-path org-roam-directory)))

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "rafaelpalomar/org-capture-slipbox" "<tab>" #'rafaelpalomar/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/Dropbox/Org-Roam/")
        ;org-roam-database-connector 'emacs-sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (defun rafaelpalomar/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'rafaelpalomar/tag-new-node-as-draft)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'citar)
  (defun rafaelpalomar/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))
