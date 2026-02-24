;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.

;;; Code:

(use-package grab-mac-link
  :ensure t
  :when *is-a-mac*)

(use-package org-cliplink
  :ensure t)

;; Global keybindings
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)

;; Core org settings
(use-package org
  :ensure nil
  :custom
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-tags-column 80)
  (org-use-tag-inheritance t)
  (org-support-shift-select t)
  :config
  ;; Navigation
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link)))

;; Org agenda
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-mode . hl-line-mode)
  :custom
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-agenda-window-setup 'current-window)
  :config
  ;; Re-align tags when window shape changes
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; Prose mode for writing
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;; Capturing
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))

;; Refiling
(setq org-refile-use-cache nil)
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; To-do settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

;; GTD Agenda views
(setq org-stuck-projects '("-INBOX/PROJECT" ("NEXT")))

(let ((active-project-match '(car org-stuck-projects)))
  (setq org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep)))))))))

;; Org clock
(use-package org-clock
  :ensure nil
  :custom
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-log-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  :config
  (org-clock-persistence-insinuate)

  ;; Show clocked-in task in header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

;; macOS status bar integration
(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))

;; Archiving
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

;; Pomodoro
(use-package org-pomodoro
  :ensure t
  :custom (org-pomodoro-keep-killed-pomodoro-time t)
  :config
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))

;; Org babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t)
      (shell . t)
      (sql . t)
      (sqlite . t)))))

;;========================================
;;; org-roam
;;========================================
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/repos/notes/") ;; 默认笔记目录, 提前手动创建好
  (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  :bind (("C-c n f" . org-roam-node-find)
         ;; 如果你的中文输入法会拦截非 ctrl 开头的快捷键, 也可考虑类似如下的设置
         ;; ("C-c C-n C-f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
         ("C-c n u" . org-roam-ui-mode)) ;; 浏览器中可视化
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map) ;; 日记菜单
  :config
  (require 'org-roam-dailies)  ;; 启用日记功能
  ;; 第四步: 用 org-roam 生成文献笔记
  (global-set-key (kbd "C-c n k") #'orb-insert-link) ; 创建文献笔记
  (global-set-key (kbd "C-c n a") #'orb-note-actions) ; 文献笔记界面的其它操作
  ;; 这里如果需要加入模板可以参考 https://gitee.com/mickey991/emacs-config/blob/master/demo-emacs-config/PKM/init.el 这个连接
  (org-roam-db-autosync-mode)) ;; 启动时自动同步数据库

;; 关键：Capture 模板 —— 这是你日记/笔记的"入口"
(setq org-capture-templates
      '(("j" "日记" entry
         (file+olp+datetree "~/org/journal.org")
         "* %U\n%?\n"  ; %U=时间戳带星期, %?=光标位置
         :empty-lines 1)

        ("n" "笔记" entry
         (file "~/org/inbox.org")
         "* %^{标题} :%^{标签}:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
         :empty-lines 1)

        ("t" "待办" entry
         (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\nSCHEDULED: %t\n")))


(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t)     ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

;; Screenshot with org-download
(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Pictures/org")
  (org-download-heading-lvl nil))

(with-eval-after-load 'org-download
  (defun my/org-screenshot ()
    "Take a screenshot and insert it into org-mode."
    (interactive)
    (let ((file (expand-file-name
                 (format-time-string "screenshot-%Y%m%d-%H%M%S.png")
                 (or (org-download-get-dir) "."))))
      (call-process "maim" nil nil nil "-s" "-u" file)
      (if (file-exists-p file)
          (org-download-insert file)
        (message "Screenshot cancelled"))))
  (define-key org-mode-map (kbd "C-c n s") 'my/org-screenshot))


(provide 'init-org)
;;; init-org.el ends here
