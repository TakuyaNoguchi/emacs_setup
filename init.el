;;; パッケージ管理
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize))

;;; 基本設定
;; ロードパス
(add-to-list 'load-path "~/.emacs.d/elisp")

;; 外部パッケージによって変更されたカスタム変数は別ファイルに記述する
(setq custom-file (locate-user-emacs-file "custom.el"))

;; zshのパスを設定
;; 必要なパスは ~/.zshenv に記載する
;; 参考サイト: http://d.hatena.ne.jp/zonu_exe/20120509/1336583187
(let ((zshpath (shell-command-to-string "/usr/bin/zsh -c 'printenv PATH'")))
  (let ((pathlst (split-string zshpath ":")))
    (setq exec-path pathlst))
  (setq eshell-path-env zshpath)
  (setenv "PATH" zshpath))

;; 起動時のメッセージを削除
(setq inhibit-startup-message t)

;; Warningは緊急性の高いもののみ表示する
(setq warning-minimum-level :emergency)

;; URLをブラウザで開く
(global-set-key (kbd "C-c C-o") 'browse-url-xdg-open)
(global-set-key (kbd "C-c o") 'browse-url-xdg-open)

;; *Help* などのウィンドウを操作しやすくする
(when (require 'popwin nil t)
  (popwin-mode 1))

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

;; バッファの再読み込み
(global-set-key (kbd "<f5>") 'revert-buffer)

;; コマンドの繰り返しを割り当て
(global-set-key (kbd "C-z") 'repeat)

;; 右クリックを無効化
(global-unset-key [down-mouse-3])

;; 起動時に初期メッセージを表示しない
(setq inhibit-startup-message t)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; インデントの設定
(electric-indent-mode 1)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-m") 'electric-newline-and-maybe-indent)

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
(setq mode-require-final-newline nil)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけカッコ内も光らせる
(setq show-paren-style 'mixed)

;; C-u C-SPC C-SPC ... で過去のマークに戻れるように設定
(setq set-mark-command-repeat-pop t)
;; 保持できるマークの数を32個に変更
(setq mark-ring-max 32)

;; yesとnoの入力をyとnで入力できるように設定
(defalias 'yes-or-no-p 'y-or-n-p)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

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
(package-install 'saveplace)
(when (require 'saveplace nil t)
  (setq save-place-file "~/.emacs.d/saved-places")

  (if (<= 25 emacs-major-version)
      (save-place-mode 1)
    (setq-default save-place t)))

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

;; リージョンを選択せずに Ctrl-w を押下したときに、カーソルの前の単語を削除する。
;; 参考サイト: http://d.hatena.ne.jp/plonk123/20121016/1350412750
(defun kill-word-or-region ()
  "Kill word backwards, kill region if there is an active one"
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'kill-word-or-region)


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



;;; color theme の設定
(package-install 'solarized-theme)
(when (require 'solarized-theme nil t)
  (load-theme 'solarized-dark t)

  ;; カーソルの色
  (set-cursor-color "pink"))



;;; Dired
(require 'dired-x nil t)

;; "r"でファイル名インライン編集する
(when (require 'wdired nil t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))



(package-install 'elscreen)
(when (require 'elscreen nil t)
  ;; プレフィクスキーはC-z
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)

  (global-set-key (kbd "C-z x") 'elscreen-kill)
  (global-set-key (kbd "C-z z") 'repeat)
  (global-set-key (kbd "C-z C-z") 'repeat))



;;; 基本キーバインド
;; バックスペース
(global-set-key (kbd "C-h") 'delete-backward-char)
;; ミニバッファ内でBackspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; カーソルの前の単語を削除
(global-set-key (kbd "M-h") 'backward-kill-word)
;; ヘルプ
(global-set-key (kbd "M-?") 'help-for-help)
;; 補完
(global-set-key (kbd "C-o") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev              ; カレントバッファでdabbrev
        try-expand-dabbrev-all-buffers  ; 全てのバッファでdabbrev
        try-expand-dabbrev-from-kill    ; キルリングの中からdabbrev
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol))
;; ruby-modeの時、シンボルの補完に関する設定
;; 参考サイト: https://emacs.stackexchange.com/questions/13078/use-hippie-expand-to-complete-ruby-symbols-without-prefix
(defun hippie-expand-ruby-symbols (orig-fun &rest args)
  (if (eq major-mode 'ruby-mode)
      (let ((table (make-syntax-table ruby-mode-syntax-table)))
        (modify-syntax-entry ?: "." table)
        (with-syntax-table table (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)

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
(package-install 'helm)
(when (require 'helm-config nil t)
  (helm-mode 1)

  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-exclude '(".recentf"
                          "/elpa/"))

  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  ;; 定義された関数を検索するのに便利
  (global-set-key (kbd "C-c C-f")   'helm-imenu)
  (global-set-key (kbd "C-x b")   'helm-mini)
  (global-set-key (kbd "C-x C-b")   'helm-buffers-list)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "C-l") 'skk-mode)
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
  (package-install 'ace-jump-helm-line)
  (when (require 'ace-jump-helm-line nil t)
    (define-key helm-map (kbd "@") 'ace-jump-helm-line)
    (setq ace-jump-helm-line-default-action 'select))

  ;; TAGの生成
  (package-install 'helm-etags-plus)
  (package-install 'ctags-update)
  (when (and (require 'helm-etags-plus nil t)
             (require 'ctags-update nil t))
    (setq ctags-update-command "/usr/bin/ctags")
    (add-hook 'ruby-mode-hook  'turn-on-ctags-auto-update-mode)
    (add-hook 'js2-mode-hook  'turn-on-ctags-auto-update-mode)

    (global-set-key (kbd "C-:") 'helm-etags-plus-select)
    ;;list all visited tags
    (global-set-key (kbd "M-.") 'helm-etags-plus-history)
    ;;go back directly
    (global-set-key (kbd "M-p") 'helm-etags-plus-history-go-back)
    ;;go forward directly
    (global-set-key (kbd "M-n") 'helm-etags-plus-history-go-forward))

  (package-install 'helm-swoop)
  (when (require 'helm-swoop nil t)

    ;; Change the keybinds to whatever you like :)
    (global-set-key (kbd "M-o") 'helm-swoop)
    (global-set-key (kbd "C-M-o") 'helm-swoop)
    (global-set-key (kbd "C-c M-o") 'helm-multi-swoop)
    (global-set-key (kbd "C-c C-M-o") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-o") 'helm-multi-swoop)
    (global-set-key (kbd "C-x C-M-o") 'helm-multi-swoop)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-o") 'helm-swoop-from-isearch)
    (define-key isearch-mode-map (kbd "C-M-o") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-o") 'helm-multi-swoop-all-from-helm-swoop)
    (define-key helm-swoop-map (kbd "C-M-o") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

    (define-key helm-swoop-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-multi-swoop-map (kbd "C-w") 'backward-kill-word)

    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)

    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)

    ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)

    ;; If you would like to use migemo, enable helm's migemo feature
    (when (require 'migemo nil t)
      (helm-migemo-mode 1)))

  ;; $ sudo apt-get install silversearcher-ag
  (package-install 'helm-ag)
  (when (require (and (executable-find "ag")
                      (require 'helm-ag nil t)))

    (setq helm-ag-base-command "ag --nocolor --nogroup")
    ;; 現在のシンボルをデフォルトのクエリにする
    (setq helm-ag-insert-at-point 'symbol)
    (global-set-key (kbd "C-M-a") 'helm-ag)

    (when (require 'projectile nil t)
      (defun helm-projectile-ag ()
        "Projectileと連携"
        (interactive)
        (helm-ag (projectile-project-root)))

      (global-set-key (kbd "C-c C-M-a") 'helm-projectile-ag)))

  (defun helm-skip-dots (old-func &rest args)
    "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
    (apply old-func args)
    (let ((sel (helm-get-selection)))
      (if (and (stringp sel) (string-match "/\\..$" sel))
          (helm-next-line 2)))
    (let ((sel (helm-get-selection))) ; if we reached .. move back
      (if (and (stringp sel) (string-match "/\\.\\.$" sel))
          (helm-previous-line 1))))

  (advice-add #'helm-preselect :around #'helm-skip-dots)
  (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots))



;;; auto-complete
(package-install 'auto-complete)
(when (require 'auto-complete nil t)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-menu-map t)
   ;; 曖昧マッチ
  (setq ac-use-fuzzy t))



;;; avy
(package-install 'avy)
(when (require 'avy nil t)
  (global-set-key (kbd "C-@") 'avy-goto-char-in-line)
  (global-set-key (kbd "M-@") 'avy-goto-char)
  (global-set-key (kbd "C-M-@") 'avy-goto-char-2))

(package-install 'avy-zap)
(when (require 'avy-zap nil t)
  (global-set-key (kbd "M-z") 'avy-zap-up-to-char))



;;; multiple cursors
(package-install 'multiple-cursors)
(package-install 'smartrep)
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
      ("O"        . 'mc/reverse-regions)))

  ;; multiple cursor 起動時に連番を挿入
  ;; 参考サイト: http://qiita.com/ShingoFukuyama/items/3ad7e24cb2d8f55b4cc5
  (defvar mc/insert-custom-numbers-hist nil)
  (defvar mc/insert-custom-numbers-inc 1)
  (defvar mc/insert-custom-numbers-pad "%01d")

  (defun mc/insert-custom-numbers (start inc pad)
    "Insert increasing numbers for each cursor specifically."
    (interactive
     (list (read-number "Start from: " 0)
           (read-number "Increment by: " 1)
           (read-string "Padding (%01d): " nil mc/insert-custom-numbers-hist "%01d")))
    (setq mc--insert-custom-numbers-number start)
    (setq mc/insert-custom-numbers-inc inc)
    (setq mc/insert-custom-numbers-pad pad)
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'mc--insert-custom-number-and-increase
      cursor)))

  (defun mc--insert-custom-number-and-increase ()
    (interactive)
    (insert (format mc/insert-custom-numbers-pad mc--insert-custom-numbers-number))
    (setq mc--insert-custom-numbers-number (+ mc--insert-custom-numbers-number mc/insert-custom-numbers-inc)))

  (define-key mc/keymap (kbd "C-c i") 'mc/insert-custom-numbers))



;;; Migemo
;; $ sudo apt-get install cmigemo
;; $ sudo apt-get install migemo
(package-install 'migemo)
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))



;;; SKK
(package-install 'ddskk)
;;
;; 辞書のダウンロード:
;; M-x skk-get
;;
;; OSのIMEを無効化:
;; $ echo 'Emacs24*useXIM: false' >> ~/.Xresources
(when (require 'skk nil t)
  ;; カーソル色を変更
  (setq skk-cursor-hiragana-color       "DodgerBlue1"
        skk-cursor-katakana-color       "green"
        skk-cursor-abbrev-color         "goldenrod"
        skk-cursor-jisx0208-latin-color "goldenrod"
        skk-cursor-jisx0201-color       "purple"
        skk-cursor-latin-color          "red")

  ;; Use AZIK
  (setq skk-use-azik t)
  (delete "l" skk-rom-kana-base-rule-list)
  (setq skk-azik-keyboard-type 'jp106)
  (setq skk-sticky-key (kbd "l"))
  (setq skk-kakutei-key (kbd "C-;"))
  ;; 単語の注釈を表示しないように修正
  (setq skk-show-annotation nil)
  ;; 変換候補がひとつしかない場合は確定する
  (setq skk-kakutei-when-unique-candidate t)
  ;; skk-isearch を無効化
  (setq skk-isearch-start-mode 'latin)
  (setq skk-large-jisyo (expand-file-name "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L"))
  ;; 最後の見出し語を表示
  (setq skk-dcomp-activate t)

  (global-set-key (kbd "C-l") 'skk-mode)
  (global-set-key (kbd "C-;") 'skk-mode)
  ;; 候補が1つの場合、skk-kakutei-when-unique-candidateで確定しているが、その単語に対して
  ;; 辞書登録したい場合は、続けてskk-undo-kakuteiを実行することで辞書登録モードに遷移する。
  ;; 辞書に誤って登録した場合は変換候補を出し skk-purge-from-jisyo を実行して削除する。
  (global-set-key (kbd "C-M-;") 'skk-undo-kakutei)

  ;; キーバインドの再割り当て
  (defvar skk-my-unnecessary-rule-list
    '(("l" nil nil)))
  (setq skk-rom-kana-rule-list
        (append skk-rom-kana-rule-list skk-my-unnecessary-rule-list))

  ;; M-x skk-auto-replace-mode でマイナーモードのON/OFFを切り替えを行う。
  ;; 見出し語のひらがなの先頭で skk-sticky-key を押して変換を行うと、以降の文章に同様の文字列
  ;; に対してquery-replace を実行する
  ;; 参考サイト: http://emacs.rubikitch.com/skk-auto-replace-mode
  (define-minor-mode skk-auto-replace-mode
    "同じ見出し語をquery-replaceする。議事録校正のためのモード。"
    nil " SKK置換")
  (defvar skk-my-kakutei-key nil "")
  (defadvice skk-start-henkan (before auto-replace activate)
    (and (eq skk-henkan-mode 'on)
         (setq skk-my-kakutei-key (buffer-substring skk-henkan-start-point (point)))))

  (defadvice skk-kakutei (after auto-replace activate)
    (skk-replace-after-kakutei))

  (defun skk-replace-after-kakutei ()
    (interactive)
    (when (and skk-auto-replace-mode
               skk-my-kakutei-key)
      (unwind-protect
          (perform-replace
           skk-my-kakutei-key (buffer-substring skk-henkan-start-point (point))
           t nil nil)
        (setq skk-my-kakutei-key nil))))

  (provide 'mylisp-skk-replace))



;;; Magit
(package-install 'magit)
(when (require 'magit nil t)
  (global-unset-key (kbd "C-x m"))
  (global-set-key (kbd "C-x m s") 'magit-status)
  (global-set-key (kbd "C-x m b") 'magit-blame))



;;; flycheck
(package-install 'flycheck)
(when (require 'flycheck nil t)
  ;; 保存時に実行する
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.3 3.0))

  (global-flycheck-mode)
  (define-key global-map (kbd "C-c n") 'flycheck-next-error)
  (define-key global-map (kbd "C-c p") 'flycheck-previous-error)
  (define-key global-map (kbd "C-c d") 'flycheck-list-errors)

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (add-hook 'ruby-mode-hook
            '(lambda ()
               (when (executable-find "rubocop")
                 (setq flycheck-checker 'ruby-rubocop)))))

;;; undohist
;; ファイルを閉じた後も履歴を保持する
(package-install 'undohist)
(require 'undohist nil t)


;;; undo-tree
;; undoの履歴を可視化する
(package-install 'undo-tree)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))



;;; expand-region
(package-install 'expand-region)
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-,")   'er/expand-region)
  (global-set-key (kbd "C-M-,") 'er/contract-region)
  (eval-after-load "org"
    '(progn
       (define-key org-mode-map (kbd "C-,") 'er/expand-region))))



;;; foreign-regexp
;; Rubyの正規表現をEmacsで使用する
(package-install 'foreign-regexp)
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp))

  (global-set-key (kbd "M-%") 'foreign-regexp/query-replace)
  (global-set-key (kbd "M-s") 'foreign-regexp/isearch-forward)
  (global-set-key (kbd "M-r") 'foreign-regexp/isearch-backward))



;;; rainbow-delimiters
;; 括弧の色付け
(package-install 'rainbow-delimiters)
(when (require 'rainbow-delimiters nil t)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))



;;; git-gutter
;; バージョン管理している場合、変更箇所を分かりやすく表示する
(package-install 'git-gutter)
(when (require 'git-gutter nil t)
  (global-git-gutter-mode +1))



;;; カーソル位置の履歴管理
;; point-undo
(package-install 'point-undo)
(when (require 'point-undo nil t)
  (global-set-key [f7] 'point-undo)
  (global-set-key [M-f7] 'point-redo))



;;; goto-chg
;; 編集履歴によるカーソル位置の記憶
(package-install 'goto-chg)
(when (require 'goto-chg nil t)
  (global-set-key [f8] 'goto-last-change)
  (global-set-key [M-f8] 'goto-last-change-reverse))



;;; sequential-command
;; C-a C-a でバッファの先頭、C-e C-e でバッファの末尾に移動
(package-install 'sequential-command)
(when (require 'sequential-command-config nil t)
  (sequential-command-setup-keys))



;;; dmacro
;; 繰り返し処理を楽にする
;; $ mkdir -p ~/.emacs.d/elisp
;; $ wget http://www.pitecan.com/DynamicMacro/dmacro.el -P ~/.emacs.d/elisp
(defconst *dmacro-key* (kbd "C-t") "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)



;;; RubyとRailsの設定
;; modeの設定
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))

(setq ruby-insert-encoding-magic-comment nil)

;; endに対応する行のハイライト
(package-install 'ruby-block)
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

(package-install 'projectile-rails)
(when (require 'projectile-rails nil t)
  (setq projectile-completion-system 'helm)
  (projectile-rails-global-mode)

  (push "*projectile-rails-generate*" popwin:special-display-config)
  (push "*projectile-rails-compilation*" popwin:special-display-config))

(package-install 'rbenv)
(when (require 'rbenv nil t)
  (global-rbenv-mode))

(package-install 'rspec-mode)
(when (require 'rspec-mode nil t)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  (push '("*rspec-compilation*" :height 25) popwin:special-display-config))



;;; JavaScriptの設定
(package-install 'js2-mode)
(when (require 'js2-mode nil t)
  (add-hook 'js2-mode-hook
            (lambda () (setq js2-basic-offset 2)))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode)))



;;; smartparens
;; 対応する括弧の入力
(package-install 'smartparens)
(when (require 'smartparens-config nil t)
  (smartparens-global-mode)
  ;; ERBの括弧を補完するための設定を追加
  (sp-with-modes '(web-mode)
    (sp-local-pair "<%" "%>")))



;;; markdown-mode
;; $ sudo apt-get install markdown
(package-install 'markdown-mode)
(require 'markdown-mode nil t)
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))



;;; org-mode
;; LOGBOOK に計測時間を格納する(折り畳み可能なセクション)
(setq org-clock-into-drawer t)

;; DONEにステータス変更時、メモを記載したい場合はtimeをnoteに変更
(setq org-log-done 'time)

;; 現在開いているファイルの行へのリンクをコピー
;; コピー後、orgファイルを開き C-c C-l でリンクをペーストできる
(global-set-key (kbd "C-c l") 'org-store-link)


(global-set-key (kbd "C-c a") 'org-agenda)

;; 予定の一覧を閲覧
(setq org-agenda-files '("~/org/agenda/todo.org" "~/org/agenda/memo.org"))

;; capture templates
(setq org-capture-templates
      '(("p" "Project Task" entry (file+headline (expand-file-name "~/org/capture/todo.org") "Inbox")
             "** TODO %?\n    %i\n    %a\n    %T")
        ("m" "memo" entry (file (expand-file-name "~/org/capture/memo.org"))
             "* %?\n    %i\n    %a\n    %T")))

(global-set-key (kbd "C-c c") 'org-capture)

;; outlineの移動を楽にする
(when (require 'smartrep nil t)
  (smartrep-define-key
      org-mode-map "C-c" '(("C-n" . (outline-next-visible-heading 1))
                           ("C-p" . (outline-previous-visible-heading 1)))))


;;; 色文字列に色をつける
(package-install 'rainbow-mode)
(setq rainbow-html-colors t)
(setq rainbow-x-colors t)
(setq rainbow-latex-colors t)
(setq rainbow-ansi-colors t)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)



;;; Web関連の設定
(package-install 'web-mode)
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)


  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))

  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  ;; 参考サイト http://biwakonbu.com/?p=589
  (custom-set-faces
   '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
   '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
   '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
   '(web-mode-comment-face           ((t (:foreground "#587F35"))))
   '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))

   '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
   '(web-mode-comment-face           ((t (:foreground "#587F35"))))
   '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
   '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
   '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
   '(web-mode-css-string-face        ((t (:foreground "#D78181"))))))

(package-install 'emmet-mode)
(when (require 'emmet-mode nil t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (eval-after-load "emmet-mode"
    '(define-key emmet-mode-keymap (kbd "C-j") nil))
  (define-key emmet-mode-keymap (kbd "C-i") 'emmet-expand-line))



;;; JavaScript
(package-install 'js2-mode)
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-mode)))




;;; yaml-mode
(package-install 'yaml-mode)
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode)))



;;; view-mode
;; 参考サイト: http://syohex.hatenablog.com/entry/20110114/1294958917
(when (require 'view nil t)
  ;; less like
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward )
  (define-key view-mode-map (kbd "G") 'View-goto-line-last)
  (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
  ;; vi/w3m like
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "K") 'View-scroll-line-backward)
  (define-key view-mode-map (kbd "C-f") 'scroll-up)
  (define-key view-mode-map (kbd "SPC") 'scroll-up)
  (define-key view-mode-map (kbd "C-b") 'scroll-down)
  (define-key view-mode-map (kbd "C-SPC") 'scroll-down)

  ;; 参考サイト: http://valvallow.blogspot.jp/2010/05/emacs-view-mode.html
  ;; 書き込み不能なファイルを view-mode で開く
  (defadvice find-file
      (around find-file-switch-to-view-file (file &optional wild) activate)
    (if (and (not (file-writable-p file))
             (not (file-directory-p file)))
        (view-file file)
      ad-do-it))
  ;; 書き込み不能なファイルの場合は view-mode を抜けないように
  (defvar view-mode-force-exit nil)
  (defmacro do-not-exit-view-mode-unless-writable-advice (f)
    `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
       (if (and (buffer-file-name)
                (not view-mode-force-exit)
                (not (file-writable-p (buffer-file-name))))
           (message "File is unwritable, so stay in view-mode.")
         ad-do-it)))

  (package-install 'key-chord)
  (when (require 'key-chord nil t)
    (setq key-chord-two-keys-delay 0.04)
    (key-chord-mode 1)
    (key-chord-define-global "jk" 'view-mode))

  ;; 参考サイト: https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:viewer:start
  (package-install 'viewer)
  (when (require 'viewer nil t)
    (viewer-change-modeline-color-setup)
    ;; 書き込み禁止ファイルの色
    (setq viewer-modeline-color-unwritable "red")
    ;; view-modeのファイルの色
    (setq viewer-modeline-color-view "blue")))



;;; テンプレートシステム
(package-install 'yasnippet)
(package-install 'helm-c-yasnippet)
(when (and (require 'yasnippet nil t)
           (require 'helm-c-yasnippet nil t))
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-M-y") 'helm-yas-complete)
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)

  (yas-global-mode 1)

  (eval-after-load 'yasnippet
  '(progn
     (define-key yas-keymap (kbd "TAB") nil)
     (define-key yas-keymap (kbd "C-c o") 'yas-next-field-or-maybe-expand)
     (define-key yas-keymap (kbd "C-c C-o") 'yas-next-field-or-maybe-expand))))



;;; モードラインの表示を簡潔にする
(package-install 'diminish)
(require 'diminish nil t)

;; 参考サイト: http://syohex.hatenablog.com/entry/20130131/1359646452
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " Ut")
    (elisp-slime-nav-mode . " EN")
    (helm-mode . "")
    (helm-gtags-mode . "")
    (helm-migemo-mode . "")
    (flycheck-mode . " Fc")
    (git-gutter-mode . "")
    (ruby-block-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)



;;; シンボルの色付け
(package-install 'highlight-symbol)
(when (require 'highlight-symbol nil t)
  (setq highlight-symbol-colors
        '("LightSeaGreen" "HotPink"
          "SlateBlue1" "DarkOrange"
          "SpringGreen1" "tan" "DodgerBlue1"))
  (global-set-key (kbd "C-c C-l") 'highlight-symbol-at-point))



;;; 任意の行をマーキング
(package-install 'bm)
(when (require 'bm nil t)
  (setq-default bm-buffer-persistence nil)
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (global-set-key (kbd "C-M-SPC") 'bm-toggle)
  (global-set-key (kbd "M-[") 'bm-previous)
  (global-set-key (kbd "M-]") 'bm-next))



;;; keychain(SSHの秘密鍵のパスワード入力を省略する)の設定
(package-install 'keychain-environment)
(when (require 'keychain-environment nil t)
  (keychain-refresh-environment))



;; ダブルクォートとシングルクォートをトグルする
(package-install 'toggle-quotes)
(when (require 'toggle-quotes nil t)
  (global-set-key (kbd "C-c C-:") 'toggle-quotes))



;;; Google翻訳の設定
;; 参考サイト: http://blog.shibayu36.org/entry/2016/05/29/123342
(package-install 'google-translate)
(when (and (require 'google-translate nil t)
           (require 'google-translate-default-ui nil t))
  (defvar google-translate-english-chars "[:ascii:]"
    "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (thing-at-point 'word))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))

  (push '("*Google Translate*" :height 25 :stick t) popwin:special-display-config)

  (global-set-key (kbd "C-M-s") 'google-translate-enja-or-jaen))



;;; SQLの設定
;; M-x sql-mysql でMySQLのプロセスを起動する
(package-install 'sql)
(package-install 'sql-indent)
(when (and (require 'sql nil t)
           (require 'sql-indent nil t))
  (defun sql-mode-hooks()
    (setq sql-indent-offset 2)
    (setq indent-tabs-mode nil))

  (add-hook 'sql-mode-hook 'sql-mode-hooks)

  (add-hook 'sql-interactive-mode-hook
          (lambda ()
            ;; 「;」をタイプしたら SQL 文を実行
            (setq sql-electric-stuff 'semicolon))))