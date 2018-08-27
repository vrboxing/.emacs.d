;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;;; Code:



    ;;  This makes Emacs ignore the "-e (make-frame-visible)"
    ;;  that it gets passed when started by emacsclientw.
    ;;
    (add-to-list 'command-switch-alist '("(make-frame-visible)" .
                 (lambda (s))))

   ;;
    ;;  This starts the Emacs server when .emacs gets loaded
    ;;
    (require 'server)
    (if (not (server-running-p)) (server-start))
 ;;
    ;;  This changes C-x C-c to just hide Emacs until the next
    ;;  time you use it.  We rebind C-M-c to be the command to
    ;;  really kill Emacs.
    ;;
    (defun my-done ()
      "Exit server buffers and hide the main Emacs window"
      (interactive)
      (server-edit)
      (make-frame-invisible nil t))
(global-set-key (kbd "C-x C-c") 'my-done)
(global-set-key (kbd "C-M-c") 'save-buffers-kill-emacs)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default cursor-type 'bar)

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
            kill-emacs-query-functions))

;; 设置垃圾回收，在Windows下，emacs25版本会频繁出发垃圾回收，所以需要设置
(if (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; 显示垃圾回收信息，这个可以作为调试用
  ;; (setq garbage-collection-messages t)
  )

;;在图形界面的菜单中打开最近 编辑过的文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;;将删除功能配置成与其他图形界面的编辑器相同，即当你选中一段文字 之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode t)

;; ;; emacs注释快捷键
;; (defun my-comment-or-uncomment-region (beg end &optional arg)
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end) nil)
;;                  (list (line-beginning-position)
;;                        (line-beginning-position 2))))
;;   (comment-or-uncomment-region beg end arg)
;;   )
;; (global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)


(provide 'init-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-server.el ends here
