;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations.
;;

;;; Code:
(prefer-coding-system 'utf-8)

;; Menu/Tool/Scroll bars
(menu-bar-mode t)
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; 启动窗口大小
;;设置窗口位置为屏库左上角(0,0)
(set-frame-position (selected-frame) 655 180)
(setq default-frame-alist
      '((height . 35) (width . 125) (menu-bar-lines . 20) (tool-bar-lines . 0)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq inhibit-startup-message t
      ;; initial-scratch-message ""
      initial-major-mode 'text-mode)

;; (global-linum-mode t)
(if (fboundp 'display-line-numbers-mode)
    (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (use-package nlinum
    :hook (after-init . global-nlinum-mode)
    ))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)


;;thems
(defun doom|init-theme ()
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-one t)
  )

(defun doom|init-theme-in-frame (frame)
  (with-selected-frame frame
    (doom|init-theme))

  ;; Unregister this hook once its run
  (remove-hook 'after-make-frame-functions
	           'doom|init-theme-in-frame))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	          'doom|init-theme-in-frame)
  (doom|init-theme))

(require 'doom-modeline)
(doom-modeline-init)

(doom-modeline-def-modeline 'main
                            '(workspace-number window-number bar evil-state " " buffer-position  " " selection-info)
                            '(buffer-encoding major-mode vcs flycheck))

;; ;; Charset 设置
;; ;; 编码设置 begin
;; (set-language-environment 'Chinese-GB)
;; ;; default-buffer-file-coding-system变量在emacs23.2之后已被废弃，使用buffer-file-coding-system代替
;; (set-default buffer-file-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; (setq-default pathname-coding-system 'euc-cn)
;; (setq file-name-coding-system 'euc-cn)
;; ;; 另外建议按下面的先后顺序来设置中文编码识别方式。
;; ;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; ;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; ;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; (prefer-coding-system 'cp950)
;; (prefer-coding-system 'gb2312)
;; (prefer-coding-system 'cp936)
;; ;;(prefer-coding-system 'gb18030)
;; ;(prefer-coding-system 'utf-16le-with-signature)
;; (prefer-coding-system 'utf-16)
;; ;; 新建文件使用utf-8-unix方式
;; ;; 如果不写下面两句，只写
;; ;; (prefer-coding-system 'utf-8)
;; ;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
;; (prefer-coding-system 'utf-8-dos)
;; (prefer-coding-system 'utf-8-unix)
;; ;; 编码设置 end

;; Fonts
(use-package cnfonts
  :preface
  (set-fontset-font t 'symbol (font-spec :family "Courier New"))
  (set-fontset-font t 'symbol (font-spec :family "Unifont") nil 'append)
  ;; Fallback to `all-the-icons'.
  (defun cnfonts--set-all-the-icons-fonts (&optional _)
    "Show icons in all-the-icons."
    (when (featurep 'all-the-icons)
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font "fontset-default" charset "all-the-icons" nil 'append)
        (set-fontset-font "fontset-default" charset "github-octicons" nil 'append)
        (set-fontset-font "fontset-default" charset "FontAwesome" nil 'append)
        (set-fontset-font "fontset-default" charset "Material Icons" nil 'append))))
  :hook ((after-init . cnfonts-enable)
         ;; (cnfonts-set-font-finish . cnfonts--set-all-the-icons-fonts)
	 )
  :config
  ;; NOTE: on macOS, the frame size is changed during the startup without below.
  ;; Keep frame size
  (setq cnfonts-keep-frame-size nil)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq cnfonts-keep-frame-size t)))
  (add-hook 'after-make-frame-functions #'cnfonts-set-font-with-saved-step)

  ;; Set profiles
  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles '("notebook" "desktop")
	cnfonts--profiles-steps '(("notebook" . 1)
				  ("desktop"  . 2))
	)
  )

(global-set-key (kbd "C-+") 'cnfonts-increase-fontsize)
(global-set-key (kbd "C--") 'cnfonts-decrease-fontsize)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
