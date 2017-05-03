;;; 基本設定
;; ロードパス
(add-to-list 'load-path "~/.emacs.d/elisp")

;; 日本語環境
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; スクロールを一行ずつ
(setq scroll-step 1)

;; スクロールバーを非表示
(scroll-bar-mode 0)

;; 現在の関数名をモードラインに表示
(which-function-mode 1)

;; 行番号と列番号を表示
(line-number-mode t)
(column-number-mode t)

;; 右クリックを無効化
(global-unset-key [down-mouse-3])

;; 起動時に初期メッセージを表示しない
(setq inhibit-startup-message t)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; メニューバーを消す
(menu-bar-mode -1)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; ツールバーを消す
(tool-bar-mode -1)

;; C-zで最小化しない
(define-key global-map (kbd "C-z") nil)

;; カーソルの点滅をとめる
(blink-cursor-mode 0)

;; ファイルの最終行に1行追加しないように
(setq require-final-newline nil)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけカッコ内も光らせる
(setq show-paren-style 'mixed)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;; 現在行を目立たせる
(global-hl-line-mode)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 補完
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完可能なものを随時表示
(icomplete-mode 1)

;; 履歴数
(setq history-length 10000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; 最近開いたファイルを保存する数
(setq recentf-max-saved-items 10000)

;; ediffを1ウィンドウで実行
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのオプション
(setq diff-switches '("-u" "-p" "-N"))

;; 以前開いたファイルを再度開いたとき、元のカーソル位置を復元する
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;; cua-modeをオン
(cua-mode t)
;; CUAキーバインドを無効化
(setq cua-enable-cua-keys nil)

;; フォントの設定
(set-frame-font "ricty-13")

;; タブ文字の設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; 行末の改行を削除
;; 参考サイト: http://pokutuna.hatenablog.com/entry/20111117/1321523457
;; 削除を実行しない拡張子
(setq delete-trailing-whitespace-exclude-patterns (list "\\.md$" "\\.markdown$"))

(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)


;; ファイル末尾の改行を削除
;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; ファイル名が重複していたらディレクトリ名を追加する
(when (require 'uniquify nil t)
  (setq uniqufy-buffer-name-style 'post-forward-angle-brackets))

;;; パッケージ管理
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize))



;;; color theme の設定
;; (package-install 'color-theme-sanityinc-solarized)
(when (require 'color-theme-sanityinc-solarized nil t)
  (color-theme-sanityinc-solarized--define-theme dark)

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (let ((mode (if (display-graphic-p frame) 'light 'dark)))
                (set-frame-parameter frame 'background-mode mode)
                (set-terminal-parameter frame 'background-mode mode))
              (enable-theme 'solarized))))



;;; Dired
(require 'dired-x nil t)

;; "r"でファイル名インライン編集する
(require 'wdired nil t)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)



;;; 基本キーバインド
;; バックスペース
(global-set-key (kbd "C-h") 'delete-backward-char)
;; カーソルの前の単語を削除
(global-set-key (kbd "M-h") 'backward-kill-word)
;; ヘルプ
(global-set-key (kbd "M-?") 'help-for-help)
;; 補完
(global-set-key (kbd "C-o") 'hippie-expand)
;; コメントアウト
(global-set-key (kbd "M-;") 'comment-dwim)
;; 行番号で移動
(global-set-key (kbd "M-g") 'goto-line)
;; 使用しないマウスのキーを無効化
(global-unset-key [drag-mouse-1])
(global-unset-key [double-mouse-1])
(global-unset-key [double-drag-Mouse-1])
(global-unset-key [triple-mouse-1])
(global-unset-key [triple-drag-mouse-1])
(global-unset-key [\S-down-mouse-1])
(global-unset-key [\C-down-mouse-1])
(global-unset-key [\M-mouse-1])
(global-unset-key [\M-down-mouse-1])
(global-unset-key [\M-drag-mouse-1])
(global-unset-key [mouse-2])
(global-unset-key [mouse-3])
(global-unset-key [\S-mouse-3])
(global-unset-key [\S-down-mouse-3])
(global-unset-key [\C-down-mouse-3])
(global-unset-key [\M-mouse-3])


;;; Helm
;; (package-install 'helm)
(when (require 'helm-config nil t)
  (helm-mode 1)
  ;; 無効にしたい機能
  ;;(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-exclude '(".recentf"
                          "/elpa/"))

  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-c C-r") 'helm-recentf)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (global-set-key (kbd "C-c i")   'helm-imenu)
  (global-set-key (kbd "C-x b")   'helm-mini)
  (global-set-key (kbd "C-x C-b")   'helm-buffers-list)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-tag . nil))

  (setq helm-buffer-details-flag nil)

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (setq helm-ff-fuzzy-matching nil)
  (defadvice helm-ff--transform-pattern-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        (substring input-pattern 1)
                      (concat ".*" input-pattern))))))

  (defun helm-buffers-list-pattern-transformer (pattern)
    (if (equal pattern "")
        pattern
      (let* ((first-char (substring pattern 0 1))
             (pattern (cond ((equal first-char "*")
                             (concat " " pattern))
                            ((equal first-char "=")
                             (concat "*" (substring pattern 1)))
                            (t
                             pattern))))
        ;; Escape some characters
        (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
        (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
        pattern)))


  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (add-to-list 'helm-source-buffers-list
               '(pattern-transformer helm-buffers-list-pattern-transformer))

  (defadvice helm-ff-sort-candidates (around no-sort activate)
    "Don't sort candidates in a confusing order!"
    (setq ad-return-value (ad-get-arg 0)))

  ;; Helmの候補選択を楽に行う
  ;; (package-install 'ace-jump-helm-line)
  (when (require 'ace-jump-helm-line nil t)
    (define-key helm-map (kbd "@") 'ace-jump-helm-line)
    (setq ace-jump-helm-line-default-action 'select))

  ;; (package-install 'helm-swoop)
  (when (require 'helm-swoop nil t)

    ;; Change the keybinds to whatever you like :)
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)

    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)

    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)

    ;; If you would like to use migemo, enable helm's migemo feature
    (when (require 'migemo nil t)
      (helm-migemo-mode 1))))


;;; auto-complete
;; (package-install 'auto-complete)
(when (require 'auto-complete nil t)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-menu-map t)
   ;; 曖昧マッチ
  (setq ac-use-fuzzy t))



;;; avy
;; (package-install 'avy)
(when (require 'avy nil t)
  (global-set-key (kbd "C-@") 'avy-goto-char)
  (global-set-key (kbd "M-@") 'avy-goto-char-2))



;;; multiple cursors
;; (package-install 'multiple-cursors)
;; (package-install 'smartrep)
(when (and (require 'multiple-cursors nil t)
           (require 'smartrep nil t))

  (declare-function smartrep-define-key "smartrep")

  (global-unset-key (kbd "C-."))
  (smartrep-define-key global-map "C-."
    '(("C-."      . 'mc/mark-next-like-this)
      ("n"        . 'mc/mark-next-like-this)
      ("p"        . 'mc/mark-previous-like-this)
      ("m"        . 'mc/mark-more-like-this-extended)
      ("u"        . 'mc/unmark-next-like-this)
      ("U"        . 'mc/unmark-previous-like-this)
      ("s"        . 'mc/skip-to-next-like-this)
      ("S"        . 'mc/skip-to-previous-like-this)
      ("*"        . 'mc/mark-all-like-this)
      ("d"        . 'mc/mark-all-like-this-dwim)
      ("i"        . 'mc/insert-numbers)
      ("o"        . 'mc/sort-regions)
      ("O"        . 'mc/reverse-regions))))

;;; easy-repeat
;; (package-install 'easy-repeat)
(require 'easy-repeat nil t)



;;; Migemo
;; $ sudo apt-get install cmigemo
;; $ sudo apt-get install migemo
;; (package-install 'migemo)
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))



;;; SKK
;; (package-install 'ddskk)
;;
;; 辞書のダウンロード:
;; M-x skk-get
;;
;; OSのIMEを無効化:
;; $ echo 'Emacs24*useXIM: false' >> ~/.Xresources
(when (require 'skk nil t)
  ;; Use AZIK
  (setq skk-use-azik t)
  (delete "l" skk-rom-kana-base-rule-list)
  (setq skk-azik-keyboard-type 'jp106)
  (setq skk-sticky-key (kbd "l"))
  (setq skk-kakutei-key (kbd "C-;"))
  ;; 変換候補がひとつしかない場合は確定する
  (setq skk-kakutei-when-unique-candidate t)
  ;; skk-isearch を無効化
  (setq skk-isearch-start-mode 'latin)

  (setq skk-large-jisyo (expand-file-name "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L"))

  (global-set-key (kbd "C-;") 'skk-mode)
  (global-set-key (kbd "C-:") 'skk-mode)
  ;; 候補が1つの場合、skk-kakutei-when-unique-candidateで確定しているが、その単語に対して
  ;; 辞書登録したい場合は、続けてskk-undo-kakuteiを実行することで辞書登録モードに遷移する。
  (global-set-key (kbd "C-M-;") 'skk-undo-kakutei)

  ;; キーバインドの再割り当て
  (defvar skk-my-unnecessary-rule-list
    '(("l" nil nil)))
  (setq skk-rom-kana-rule-list
        (append skk-rom-kana-rule-list skk-my-unnecessary-rule-list)))


;;; Magit
;; (package-install 'magit)
(when (require 'magit nil t)
  (global-unset-key (kbd "C-x g"))
  (global-set-key (kbd "C-x g s") 'magit-status)
  (global-set-key (kbd "C-x g b") 'magit-blame))



;;; flycheck
;; (package-install 'flycheck)
(when (require 'flycheck nil t)
  ;; 保存時に実行する
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode)
  (define-key global-map (kbd "C-c n") 'flycheck-next-error)
  (define-key global-map (kbd "C-c p") 'flycheck-previous-error)
  (define-key global-map (kbd "C-c d") 'flycheck-list-errors)

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; undohist
;; ファイルを閉じた後も履歴を保持する
;; (package-install 'undohist)
(require 'undohist nil t)


;;; undo-tree
;; undoの履歴を可視化する
;; (package-install 'undo-tree)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))



;;; expand-region
;; (package-install 'expand-region)
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-,")   'er/expand-region)
  (global-set-key (kbd "C-M-,") 'er/contract-region))



;;; foreign-regexp
;; Rubyの正規表現をEmacsで使用する
;; (package-install 'foreign-regexp)
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp))

  (global-set-key (kbd "M-%") 'foreign-regexp/query-replace)
  (global-set-key (kbd "M-s") 'foreign-regexp/isearch-forward)
  (global-set-key (kbd "M-s") 'foreign-regexp/isearch-backward))



;;; rainbow-delimiters
;; 括弧の色付け
;; (package-install 'rainbow-delimiters)
(when (require 'rainbow-delimiters nil t)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))



;;; git-gutter
;; バージョン管理している場合、変更箇所を分かりやすく表示する
;; (package-install 'git-gutter)
(when (require 'git-gutter nil t)
  (global-git-gutter-mode +1))



;;; カーソル位置の履歴管理
;; point-undo
;; (package-install 'point-undo)
(when (require 'point-undo nil t)
  (global-set-key [f7] 'point-undo)
  (global-set-key [M-f7] 'point-redo))



;;; goto-chg
;; 編集履歴によるカーソル位置の記憶
;; (package-install 'goto-chg)
(when (require 'goto-chg nil t)
  (global-set-key [f8] 'goto-last-change)
  (global-set-key [M-f8] 'goto-last-change-reverse))



;;; sequential-command
;; C-a C-a でバッファの先頭、C-e C-e でバッファの末尾に移動
;; (package-install 'sequential-command)
(when (require 'sequential-command-config nil t)
  (sequential-command-setup-keys))



;;; dmacro
;; 繰り返し処理を楽にする
;; $ mkdir -p ~/.emacs.d/elisp
;; $ wget http://www.pitecan.com/DynamicMacro/dmacro.el -P ~/.emacs.d/elisp
(defconst *dmacro-key* (kbd "C-t") "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)



;;; markdown-mode
;; (package-install 'markdown-mode)
;; $ sudo apt-get install markdown
