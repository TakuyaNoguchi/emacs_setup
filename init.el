;;; パッケージ管理
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize))

;; 外部パッケージによって変更されたカスタム変数は別ファイルに記述する
(setq custom-file (locate-user-emacs-file "custom.el"))

;; コンパイル時に画面描画に合わせて自動スクロールするための設定
(setq compilation-scroll-output t)

;; ダイアログを表示しないように
(defalias 'message-box 'message)
(setq use-dialog-box nil)

;; emacsclient を使用するための設定
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

;; シェルの環境変数を引き継ぐ
(package-install 'exec-path-from-shell)
(when (require 'exec-path-from-shell nil t)
  (let ((envs '("PATH" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))

;; 起動時のメッセージを削除
(setq inhibit-startup-message t)

;; Warningは緊急性の高いもののみ表示する
(setq warning-minimum-level :emergency)

;; カーソルの前の式をkillする
(global-set-key (kbd "C-M-k") 'backward-kill-sexp)

;; デフォルトのIMEの切り替えをOFF
(global-set-key (kbd "C-\\") nil)

;; URLをブラウザで開く
(global-set-key (kbd "C-c C-o") 'browse-url-xdg-open)
(global-set-key (kbd "C-c o") 'browse-url-xdg-open)

;; ウィンドウの移動
(defun reverse-other-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "M-t") 'reverse-other-window)
(global-set-key (kbd "C-M-t") 'reverse-other-window)

;;Shift + 矢印キーで出来るように
(windmove-default-keybindings)

;; C-x C-d にも diredを割り当て
(global-set-key (kbd "C-x C-d") 'dired)

;; 行の整列をするコマンドにキーバインドを割り当て
(global-set-key (kbd "C-c C-;") 'align-regexp)

;; 選択した文字列を大文字、小文字に変換
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; 括弧の対応はsmartparensを使う
(electric-pair-mode 0)

;; インデントの設定
(electric-indent-mode 1)

;;; 改行時に適宜空行を入れてくれるパッケージ
(package-install 'smart-newline)
(when (require 'smart-newline nil t)
  (global-set-key (kbd "C-j") 'smart-newline))

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
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; 以前開いたファイルを再度開いたとき、元のカーソル位置を復元する
(package-install 'saveplace)
(when (require 'saveplace nil t)
  (setq save-place-file "~/.emacs.d/saved-places")

  (if (<= 25 emacs-major-version)
      (save-place-mode 1)
    (setq-default save-place t)))

;; キーバインドの割り当て(minor-modeの競合の管理など)を楽にするパッケージ
;; 参考サイト: http://emacs.rubikitch.com/bind-key/
(package-install 'bind-key)
(require 'bind-key nil t)

;; cua-modeをオン
(cua-mode t)
;; CUAキーバインドを無効化
(setq cua-enable-cua-keys nil)

;; フォントの設定
;; org-modeのテーブルが崩れないようにフォントサイズは 1.5 の倍数にする
;; 参考サイト: http://oogatta.hatenadiary.jp/entry/20130816/1376621767
(set-frame-font "ricty-13.5")
(custom-set-faces
 '(variable-pitch ((t (:family "Ricty"))))
 '(fixed-pitch ((t (:family "Ricty")))))

;; タブ文字の設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)



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



;;; バッファの切り替え
(package-install 'iflipb)
(when (require 'iflipb nil t)
  ;; 参考サイト: https://qiita.com/minoruGH/items/aa96e92c1434f87940d6
  (setq iflipb-ignore-buffers (list "^[*]" "^magit" "]$"))
  (setq iflipb-wrap-around t)
  (bind-key* "M-o" 'iflipb-next-buffer)
  (bind-key* "M-O" 'iflipb-previous-buffer))



;;; EWW
(when (require 'eww nil t)
  (setq eww-search-prefix "https://www.google.co.jp/search?hl=ja&num=100&as_qdr=y5&lr=lang_ja&q=")
  (global-set-key (kbd "C-c C-s") 'eww-search-words)

  ;; キーバイント
  (define-key eww-mode-map "N" 'eww-next-url)
  (define-key eww-mode-map "P" 'eww-previous-url)
  (define-key eww-mode-map "l" 'eww-back-url)
  (define-key eww-mode-map "r" 'eww-forward-url)
  (define-key eww-mode-map "H" 'eww-list-histories)
  (define-key eww-mode-map "&" 'eww-browse-with-external-browser)
  (define-key eww-mode-map "b" 'eww-add-bookmark)
  (define-key eww-mode-map "B" 'eww-list-bookmarks)
  (define-key eww-mode-map "q" 'quit-window)
  (define-key eww-mode-map "r" 'eww-reload)
  (define-key eww-mode-map "c 0" 'eww-copy-page-url)
  (define-key eww-mode-map "p" 'scroll-down)
  (define-key eww-mode-map "n" 'scroll-up)

  ;; リンクをキーボードでクリック出来るように
  (package-install 'ace-link)
  (when (require 'ace-link nil t)
    (with-eval-after-load "eww"
      (define-key eww-mode-map "f" 'ace-link-eww)
      (ace-link-setup-default)))

  ;; 色の設定
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  (defun eww-disable-color ()
    "eww で文字色を反映させない"
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  (defun eww-enable-color ()
    "eww で文字色を反映させる"
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))

  ;; 参考サイト: http://emacs.rubikitch.com/eww-image/
  (defun eww-disable-images ()
    "ewwで画像表示させない"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image-alt)
    (eww-reload))
  (defun eww-enable-images ()
    "ewwで画像表示させる"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image)
    (eww-reload))
  (defun shr-put-image-alt (spec alt &optional flags)
    (insert alt))
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

  ;; 現在の URL をクリップボードにコピー
  ;; 参考サイト: http://futurismo.biz/archives/2989
  (defun eww-copy-page-org-link ()
    (interactive)
    (my/copy-org-link (eww-current-url) (eww-current-title)))
  (define-key eww-mode-map (kbd "0") 'eww-copy-page-org-link))



;;; color theme の設定
(package-install 'solarized-theme)
(when (require 'solarized-theme nil t)
  ;; org-mode の見出しの大きさを変更しない
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t)

  ;; カーソルの色
  (set-cursor-color "pink")

  (when (require 'whitespace nil t)
    (setq whitespace-style
          '(face       ; faceで可視化
            trailing   ; 行末
            tabs       ; タブ
            tab-mark   ; >> を表示する
            ))

    (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")

    (defvar my/background-color (face-background 'default))
    (set-face-attribute 'whitespace-trailing nil
                        :foreground my/background-color
                        :background "Chartreuse"
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :foreground my/background-color
                        :background "Chartreuse"
                        :underline nil)

    (global-whitespace-mode t)

    ;; 保存前に自動でクリーンアップ
    (setq whitespace-action '(auto-cleanup))

    ;; 行末の空白を削除したくないモード
    (add-hook 'markdown-mode-hook
              (lambda ()
                (set (make-local-variable 'whitespace-action) nil)))))



;;; Dired
(require 'dired-x nil t)

;; "r"でファイル名インライン編集する
(when (require 'wdired nil t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))



;;; Shellの設定
(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "C-t") 'other-window)
     (define-key term-raw-map (kbd "M-t") 'reverse-other-window)
     (define-key term-raw-map (kbd "C-M-t") 'reverse-other-window)
     (define-key term-raw-map (kbd "C-M-y") 'term-paste)))



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



;;; キルリングの操作を楽にする(M-w で行単位、M-w w で単語単位など)
(package-install 'easy-kill)
(when (require 'easy-kill nil t)
  (global-set-key (kbd "M-w") 'easy-kill))



;;; 基本キーバインド
;; バックスペース
(bind-key* "C-h" 'delete-backward-char)
;; skeleton 挿入時に C-h でバックスペースを入力できるようにするための設定
(setq help-char nil)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; カーソルの前の単語を削除
(bind-key* "M-h" 'backward-kill-word)
;; ヘルプ
(bind-key* "M-?" 'help-for-help)
;; 補完
(bind-key* "C-o" 'hippie-expand)
(bind-key* "C-c RET" 'open-line)
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

  (custom-set-variables
   '(helm-ff-skip-boring-files t)
   ;; 暗号化ディレクトリが候補に出ると邪魔なので除外
   '(helm-boring-file-regexp-list (append helm-boring-file-regexp-list '("\\.work$"))))

  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-exclude '(".recentf" "/elpa/"))

  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  ;; 定義された関数を検索
  (global-set-key (kbd "C-c C-f") 'helm-imenu)
  (global-set-key (kbd "C-c C-b")   'helm-mini)
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
    (define-key helm-map (kbd "C-@") 'ace-jump-helm-line)
    (setq ace-jump-helm-line-default-action 'select))

  ;; TAGの生成
  (package-install 'helm-etags-plus)
  (package-install 'ctags-update)
  (when (and (require 'helm-etags-plus nil t)
             (require 'ctags-update nil t))
    (setq ctags-update-command "/usr/bin/ctags")
    (add-hook 'ruby-mode-hook  'turn-on-ctags-auto-update-mode)
    (add-hook 'js2-mode-hook  'turn-on-ctags-auto-update-mode)

    (global-set-key (kbd "C-]") 'helm-etags-plus-select)
    (global-set-key (kbd "M-]") 'helm-etags-plus-history-go-back)
    (global-set-key (kbd "C-M-]") 'helm-etags-plus-history-go-back)
    ;;list all visited tags
    (global-set-key (kbd "C-M-.") 'helm-etags-plus-history)
    ;;go back directly
    (global-set-key (kbd "M-p") 'helm-etags-plus-history-go-back)
    ;;go forward directly
    (global-set-key (kbd "M-n") 'helm-etags-plus-history-go-forward))

  (package-install 'helm-swoop)
  (when (require 'helm-swoop nil t)

    ;; Change the keybinds to whatever you like :)
    (global-set-key (kbd "C-:") 'helm-swoop)
    (global-set-key (kbd "C-c C-:") 'helm-multi-swoop)
    (global-set-key (kbd "C-M-:") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "C-:") 'helm-swoop-from-isearch)
    (define-key isearch-mode-map (kbd "C-M-:") 'helm-multi-swoop-all-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "C-M-:") 'helm-multi-swoop-all-from-helm-swoop)
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
  (global-set-key (kbd "C-c C-@") 'avy-goto-char)
  (global-set-key (kbd "C-M-@") 'avy-goto-char-2))

(package-install 'avy-zap)
(when (require 'avy-zap nil t)
  (global-set-key (kbd "C-M-z") 'avy-zap-to-char)
  (global-set-key (kbd "M-z") 'avy-zap-to-char))



;;; window の分割が多くなったときの移動を楽にする
(package-install 'ace-window)
(when (require 'ace-window nil t)
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))



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
  ;; 最後の変換候補を動的に表示
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
        (append skk-rom-kana-rule-list skk-my-unnecessary-rule-list)))



;;; Magit
(package-install 'magit)
(when (require 'magit nil t)
  (global-unset-key (kbd "C-x m"))
  (global-set-key (kbd "C-x m b") 'magit-blame)
  (global-set-key (kbd "C-x m d") 'magit-diff)
  (global-set-key (kbd "C-x m l") 'magit-log)
  (global-set-key (kbd "C-x m s") 'magit-status))



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

  (add-hook 'sh-mode-hook
            (lambda ()
              (setq sh-basic-offset 2
                    sh-indentation 2)

              ;; $ sudo apt-get install shellcheck
              (when (executable-find "shellcheck")
                (setq flycheck-checker 'sh-shellcheck)))))



;;; undohist
;; ファイルを閉じた後も履歴を保持する
(package-install 'undohist)
(require 'undohist nil t)


;;; undo-tree
;; undoの履歴を可視化する
(package-install 'undo-tree)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-M-u") 'undo-tree-visualize)
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
(when (and (require 'smartrep nil t)
           (require 'git-gutter nil t))
  (global-git-gutter-mode +1)

  (smartrep-define-key
      global-map "C-c" '(("C-n" . 'git-gutter:next-hunk)
                         ("C-p" . 'git-gutter:previous-hunk))))



;;; カーソル位置の履歴管理
;; point-undo
(package-install 'point-undo)
(when (require 'point-undo nil t)
  (global-set-key [f7] 'point-undo)
  (global-set-key [M-f7] 'point-redo))



;;; sequential-command
;; C-a C-a でバッファの先頭、C-e C-e でバッファの末尾に移動
(package-install 'sequential-command)
(when (require 'sequential-command-config nil t)
  (sequential-command-setup-keys))



;;; RubyとRailsの設定
;; modeの設定
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))

(with-eval-after-load "ruby-mode"
  (bind-key* (kbd "C-M-p") 'ruby-beginning-of-block ruby-mode-map)
  (bind-key* (kbd "C-M-n") 'ruby-end-of-block ruby-mode-map))

(setq ruby-insert-encoding-magic-comment nil)

;; endに対応する行のハイライト
(package-install 'ruby-block)
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

(package-install 'projectile)
(package-install 'projectile-rails)
(when (and (require 'projectile nil t)
           (require 'projectile-rails nil t))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)

  (projectile-global-mode)
  (projectile-rails-global-mode)

  (global-set-key (kbd "C-x C-M-f") 'projectile-find-file)

  (push "*projectile-rails-generate*" popwin:special-display-config)
  (push "*projectile-rails-compilation*" popwin:special-display-config))

;; Rubyのリファクタリング支援
;; https://github.com/ajvargo/ruby-refactor
(package-install 'ruby-refactor)
(when (require 'ruby-refactor nil t)
  ;; C-c C-r e は直感的ではないため、キーバインドを追加。
  (define-key ruby-refactor-mode-map (kbd "C-c C-r m") 'ruby-refactor-extract-to-method)
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(package-install 'rbenv)
(when (require 'rbenv nil t)
  (global-rbenv-mode))

(package-install 'rspec-mode)
(when (require 'rspec-mode nil t)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  (push '("*rspec-compilation*" :height 25) popwin:special-display-config))

;; Ruby のREPL
;; $ gem i pry
;; $ gem i pry-doc
;; $ gem i method_source
(package-install 'inf-ruby)
(when (require 'inf-ruby nil t)
    (setq inf-ruby-default-implementation "pry")
    (setq inf-ruby-eval-binding "Pry.toplevel_binding")

    ;; riなどのエスケープシーケンスを処理し、色付けする
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

;; RuboCop
(package-install 'rubocop)
(when (require 'rubocop nil t)
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(package-install 'robe)
(package-install 'helm-robe)
(when (require 'robe nil t)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  (define-key ruby-mode-map (kbd "C-c C-.") 'robe-start)

  (when (require 'helm-robe nil t)
    (custom-set-variables
     '(robe-completing-read-func 'helm-robe-completing-read))))

(package-install 'yard-mode)
(when (require 'yard-mode nil t)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode))

(package-install 'slim-mode)
(when (require 'slim-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.slim?\\'" . slim-mode)))

;;; yaml-mode
(package-install 'yaml-mode)
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode)))

;;; scss-mode
(package-install 'scss-mode)
(when (require 'scss-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

  (add-hook 'scss-mode-hook
            (lambda()
              (custom-set-variables
               '(css-indent-offset 2)
               '(scss-compile-at-save nil)))))

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
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))



;;; org-mode
;; タスクの状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "LATER(l)" "|" "DONE(d)")))

;; 見出し間の移動、タスクの状態の変更を楽に行えるキーバインドを有効化
;; 参考サイト: https://qiita.com/takaxp/items/a5a3383d7358c58240d0
(setq org-use-speed-commands t)
;; 見出しで t を押下したときにタスクのステータスを 'TODO' に変更
(add-to-list 'org-speed-commands-user '("t" org-todo "TODO"))
;; 見出しで s を押下したときにタスクのステータスを 'STARTED' に変更
(add-to-list 'org-speed-commands-user '("s" org-todo "STARTED"))
;; 見出しで l を押下したときにタスクのステータスを 'LATER' に変更
(add-to-list 'org-speed-commands-user '("l" org-todo "LATER"))
;; 見出しで d を押下したときにタスクのステータスを 'DONE' に変更
(add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))

;; 見出しのアスタリスクの表示に関する設定
(setq org-indent-mode-turns-on-hiding-stars nil)

;; LOGBOOK に計測時間を格納する(折り畳み可能なセクション)
(setq org-clock-into-drawer t)

;; DONEにステータス変更時、メモを記載したい場合はtimeをnoteに変更
(setq org-log-done 'time)

;; コードブロックのシンタックスハイライト
(setq org-src-fontify-natively t)

;; 現在開いているファイルの行へのリンクをコピー
;; コピー後、orgファイルを開き C-c C-l でリンクをペーストできる
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c a") 'org-agenda)

;; org-modeでGFM形式のMarkdownをexportできるようにする
;; C-c C-e のメニューに「g」が追加される。
(package-install 'ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; 時間計測を開始後、自動でSTARTED状態に変更
(setq org-clock-in-switch-to-state "STARTED")

;; 予定の一覧を閲覧
(defvar org-agenda-directory (expand-file-name "~/org/agenda/"))
(setq org-agenda-files
      (directory-files-recursively
       org-agenda-directory "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

;; capture templates
(defvar org-capture-task-file (expand-file-name "~/org/agenda/capture_task.org"))
(defvar org-capture-memo-file (expand-file-name "~/org/agenda/capture_memo.org"))
(defvar org-interrupt-task (expand-file-name "~/org/agenda/interrupt_task.org"))
(setq org-capture-templates
      '(("p" "Project Task" entry (file org-capture-task-file)
         "** TODO %?   %T\n%a")
        ("m" "Memo" entry (file org-capture-memo-file)
         "* %?   %T\n%a")
        ("i" "Interrupt Task" entry
         (file org-interrupt-task)
         "* %?\n" :clock-in t :clock-resume t)
        ))

(global-set-key (kbd "C-c c") 'org-capture)

;; outlineの移動を楽にする
(when (require 'smartrep nil t)
  ;; 編集履歴によるカーソル位置の記憶
  (package-install 'goto-chg)
  (when (require 'goto-chg nil t)
    (smartrep-define-key
        global-map "C-c" '(("C-/" . goto-last-change)
                           ("M-/" . goto-last-change-reverse)
                           ("C-M-/" . goto-last-change-reverse)))))

;; 時間計測の設定
;; 参考サイト: https://qiita.com/takaxp/items/6b2d1e05e7ce4517274d
(package-install 'org-tree-slide)

;; 1分未満を記録しない
(setq org-clock-out-remove-zero-time-clocks t)

;; Emacs終了時にorg-clock-inしているタスクの時間計測を終了
(defun my:org-clock-out-and-save-when-exit ()
  "Save buffers and stop clocking when kill emacs."
  (when (org-clocking-p)
    (org-clock-out)
    (save-some-buffers t)))
(add-hook 'kill-emacs-hook #'my:org-clock-out-and-save-when-exit)

;; 記録中のタスク名をフレームタイトルに表示するように変更
(setq org-clock-clocked-in-display 'frame-title)

(when (require 'org-tree-slide nil t)
  (with-eval-after-load "org-tree-slide"
    ;; ナローイング用基本設定の適用
    (org-tree-slide-narrowing-control-profile)
    ;; 高速動作用(推奨)
    (setq org-tree-slide-modeline-display 'outside)
    ;; DONEなタスクも表示する
    (setq org-tree-slide-skip-done nil)

    ;; タスクの時間計測を開始
    (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)

    ;; タスクの切り替え
    (define-key org-tree-slide-mode-map (kbd "<f9>")
      'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f10>")
      'org-tree-slide-move-next-tree)

    (when (require 'org-clock nil t)
      ;; org-clock-in を拡張
      ;; 発動条件1）タスクが DONE になっていないこと（変更可）
      ;; 発動条件2）アウトラインレベルが4まで．それ以上に深いレベルでは計測しない（変更可）
      (defun my:org-clock-in ()
        (setq vc-display-status nil) ;; モードライン節約
        (when (and (looking-at (concat "^\\*+ " org-not-done-regexp))
                   (memq (org-outline-level) '(1 2 3 4)))
          (org-clock-in)))

      ;; org-clock-out を拡張
      (defun my:org-clock-out ()
        (setq vc-display-status t) ;; モードライン節約解除
        (when (org-clocking-p)
          (org-clock-out)))

      ;; org-clock-in をナローイング時に呼び出す．
      (add-hook 'org-tree-slide-after-narrow-hook #'my:org-clock-in)

      ;; org-clock-out を適切なタイミングで呼び出す．
      (add-hook 'org-tree-slide-before-move-next-hook #'my:org-clock-out)
      (add-hook 'org-tree-slide-before-move-previous-hook #'my:org-clock-out)
      (add-hook 'org-tree-slide-mode-stop-hook #'my:org-clock-out)

      ;; 一時的にナローイングを解く時にも計測を止めたい人向け
      (add-hook 'org-tree-slide-before-content-view-hook #'my:org-clock-out))

    (package-install 'org-clock-today)

    (when (require 'org-clock-today nil t)
      (with-eval-after-load "org-clock-today"
        (defun advice:org-clock-today-update-mode-line ()
          "Calculate the total clocked time of today and update the mode line."
          (setq org-clock-today-string
                (if (org-clock-is-active)
                    (save-excursion
                      (save-restriction
                        (with-current-buffer (org-clock-is-active)
                          (widen)
                          (let* ((current-sum (org-clock-sum-today))
                                 (open-time-difference (time-subtract
                                                        (float-time)
                                                        (float-time org-clock-start-time)))
                                 (open-seconds (time-to-seconds open-time-difference))
                                 (open-minutes (/ open-seconds 60))
                                 (total-minutes (+ current-sum
                                                   open-minutes)))
                            (concat " " (org-minutes-to-clocksum-string total-minutes))))))
                  ""))
          (force-mode-line-update))
        (advice-add 'org-clock-today-update-mode-line :override
                    #'advice:org-clock-today-update-mode-line)

        (defun advice:org-clock-sum-today (&optional headline-filter)
          "Sum the times for each subtree for today."
          (let ((range (org-clock-special-range 'today nil t)))
            (org-clock-sum (car range) (cadr range)
                           headline-filter :org-clock-minutes-today)))
        (advice-add 'org-clock-sum-today :override #'advice:org-clock-sum-today)))))

;; 祝日をorg-modeのカレンダーに表示
;; 参考サイト: http://emacs.rubikitch.com/japanese-holidays/
(package-install 'japanese-holidays)
(with-eval-after-load "calendar"
  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays holiday-other-holidays))
    ;; 祝日をカレンダーに表示
    (setq calendar-mark-holidays-flag t)
    (setq japanese-holiday-weekend '(0 6)    ; 土日を祝日として表示
          japanese-holiday-weekend-marker    ; 土曜日を水色で表示
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    ;; org-agendaで祝日を表示する
    (setq org-agenda-include-diary t)))



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

  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)


  (defun my-web-mode-hook ()
    ;; クリップボードからペーストした時に勝手にインデントされるのを防ぐ
    (setq web-mode-enable-auto-indentation nil)
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
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-mode))

  ;; インデントを2に設定
  (add-hook 'js2-mode-hook
            (lambda () (setq js2-basic-offset 2)))

  ;; デフォルトのもの以外でチェックから除外するキーワード
  (setq-default js2-global-externs
                '("module" "require" "buster" "sinon" "assert" "refute"
                  "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                  "JSON")))

;; JavaScriptのリファクタリング支援
(package-install 'js2-refactor)
(when (require 'js2-refactor nil t)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))



;;; golang
;; $ go get -u github.com/nsf/gocode
;; $ go get -u github.com/rogpeppe/godef
;; 必要な import を自動で追加
;; $ go get -u golang.org/x/tools/cmd/goimports
;; 静的解析
;; $ go get -u github.com/golang/lint/golint
;; $ go get -u github.com/kisielk/errcheck
;; デバッガ
;; $ go get -u github.com/derekparker/delve/cmd/dlv
(package-install 'go-mode)
(package-install 'go-autocomplete)
(package-install 'go-eldoc)
(package-install 'go-dlv)
(when (and (require 'go-mode nil t)
           (require 'go-autocomplete nil t)
           (require 'go-eldoc nil t)
           (require 'go-dlv nil t))
  (add-hook 'go-mode-hook
            (lambda ()
              (go-eldoc-setup)
              (local-set-key (kbd "M-.") 'godef-jump)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save)
              (push '("*Gofmt Errors*" :height 15 :stick t) popwin:special-display-config))))



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



;;; keychain(SSHの秘密鍵のパスワード入力を省略する)の設定
(package-install 'keychain-environment)
(when (require 'keychain-environment nil t)
  (keychain-refresh-environment))



;; ダブルクォートとシングルクォートをトグルする
(package-install 'toggle-quotes)
(when (require 'toggle-quotes nil t)
  (global-set-key (kbd "C-x C-:") 'toggle-quotes))


;;; Google検索
;; 参考サイト: https://konbu13.hatenablog.com/entry/2014/11/14/215224
(package-install 'google-this)
(when (require 'google-this)
  (setq google-this-location-suffix "co.jp")
  (defun google-this-url () "URL for google searches."
         (concat google-this-base-url google-this-location-suffix
                 "/search?q=%s&hl=ja&num=10&as_qdr=y5&lr=lang_ja"))
  (global-set-key (kbd "C-c g") 'google-this))

;;; Google翻訳の設定
;; 参考サイト: http://blog.shibayu36.org/entry/2016/05/29/123342
(package-install 'google-translate)
(when (and (require 'google-translate nil t)
           (require 'google-translate-default-ui nil t))
  (setq google-translate-backend-method 'curl)

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

  (global-set-key (kbd "C-M-s") 'google-translate-enja-or-jaen)

  (defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
    (list 427110 1469889687)))



;;; C言語の設定
(when (require 'cc-mode nil t)
  ;; c-mode-common-hook は C/C++ の設定
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-default-style "linux")
              (setq c-basic-offset 4))))

(package-install 'auto-complete-c-headers)
(when (require 'auto-complete-c-headers nil t)
  (add-hook 'c-mode-hook (lambda () (setq ac-sources '(ac-source-c-headers)))))

(package-install 'function-args)
(when (require 'function-args nil t)
  (fa-config-default))

(package-install 'c-eldoc)
(when (require 'c-eldoc nil t)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (setq c-eldoc-buffer-regenerate-time 60))

;; Makefileと判別するための拡張子の追加
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-mode))



;;; SQLの設定
;; M-x sql-mysql でMySQLのプロセスを起動する
(package-install 'sql)
(package-install 'sql-indent)
(when (and (require 'sql nil t)
           (require 'sql-indent nil t))
  (defun sql-mode-hooks()
    (setq sql-indent-offset 2))

  (add-hook 'sql-mode-hook 'sql-mode-hooks)

  (add-hook 'sql-interactive-mode-hook
          (lambda ()
            ;; 「;」をタイプしたら SQL 文を実行
            (setq sql-electric-stuff 'semicolon))))



;;; Haskellの設定
(package-install 'haskell-mode)
(package-install 'ghc)
(when (and (require 'haskell-mode nil t)
           (require 'ghc nil t))
  (defun my-haskell-mode-hook ()
    (interactive)

    ;; インデントの設定
    (turn-on-haskell-indentation)

    (turn-on-haskell-doc-mode)
    (font-lock-mode)
    (imenu-add-menubar-index)

    ;; ghci のコマンドを設定
    (setq haskell-program-name "/usr/bin/stack ghci")

    (ghc-init))

  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (define-key haskell-indentation-mode-map (kbd "C-j") 'haskell-indentation-newline-and-indent)
  (define-key haskell-indentation-mode-map (kbd "RET") 'newline)

  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)))



;;; 繰り返し特定のキーを入力したときの挙動を設定
(package-install 'key-combo)
(when (require 'key-combo nil t)
  (global-key-combo-mode t)

  (setq my-key-combos-common
        '((","   . (", " ","))
          ("="   . (" = " "="))
          ("=;"  . " == ")
          ("*="  . " *= ")
          ("+"   . ("+" " + " "+"))
          ("+="  . " += ")
          ("-"   . ("-" " - " "-"))
          ("-="  . " -= ")
          (">"   . (">" " > " ">"))
          (">;"  . " >> ")
          (">="  . " >= ")
          ("&"   . ("&" " & " "&"))
          ("&;"   . " && ")
          ("*="  . " *= " )
          ("<"   . ("<" " < " "<"))
          ("<;"  . " << ")
          ("<="  . " <= ")
          ("|;"  . " || ")
          ("|="  . " |= ")
          ("|;=" . " ||= ")
          ("/="  . " /= ")
          ("%="  . " %= ")
          ("!="  . " != " )
          ("&="  . " &= ")
          ("&&=" . " &&= ")))

  (defvar my-c-mode-hooks
    '(c-mode-hook))

  (setq my-key-combos-for-c
        (append my-key-combos-common
                '(("+;"  . "++")
                  ("-;"  . "--")
                  ("%"   . (" % " "%")))))

  (key-combo-define-hook my-c-mode-hooks
                         'my-key-combo-c-hook
                         my-key-combos-for-c)

  (defvar my-haskell-mode-hooks
    '(haskell-mode-hook haskell-interactive-mode-hook))

  (defvar my-key-combos-for-haskell
    '(("+"   . (" + " "+"))
      ("+;"  . " ++ ")
      ("-"   . ("-" " - " "-"))
      ("-;"  . "--")
      ("*"   . (" * " "*"))
      ("*;"  . "**")
      ("/"   . (" / " "/"))
      ("/="  . " /= ")
      ("->"  . " -> ")
      ("=>"  . " => ")
      ("<"   . (" < " " << " "<"))
      ("<;"  . " << ")
      ("<="  . " <= ")
      ("<-"  . " <- ")
      ("<$"  . " <$> ")
      (">"   . (" > " " >> " ">"))
      (">;"  . " >> ")
      (">="  . " >= ")
      (">>=" . " >>= ")
      (">;=" . " >>= ")
      ("="   . (" = " " == " "="))
      ("=;"  . " == ")
      (":"   . (" : " ":"))
      (":;"  . " :: ")
      ("$"   . (" $ " "$"))
      ("$!"  . " $! ")
      (";"   . (" . " ";"))
      ("."   . ("." " . " "."))
      (".;"  . "..")
      (".:"  . "...")
      (","   . (", " ","))
      ("';"  . " '`!!''")
      ("\";" . " \"`!!'\"")
      ("`;"  . " ``!!'`")
      ("(;"  . " (`!!')")
      ("[;"  . " [`!!']")
      ("{;"  . " {`!!' }")
      ("{-"  . " {-\n`!!'\n-}")
      ("&"   . (" & " " && " "&"))
      ("&;"  . " && ")
      ("|"   . (" | " " || " "|"))
      ("|;"  . " || ")
      ("!!"  . " !! ")
      ("!;"  . " !! ")
      ("_;"  . " _")))

  (key-combo-define-hook my-haskell-mode-hooks
                         'my-key-combo-haskell-hook
                         my-key-combos-for-haskell)

  (defvar my-ruby-mode-hooks
    '(ruby-mode-hook))

  (setq my-key-combos-for-ruby
        (append my-key-combos-common
                '(("<"   . (" < " "<"))
                  (">"   . (" > " ">"))
                  ("=:"  . " === ")
                  ("=>"  . " => ")
                  ("->"  . " ->")
                  ("%"   . ("%" " % " "%"))
                  ("=~"  . " =~ ")
                  ("!~"  . " !~ ")
                  ("*"   . ("*" " * " "*"))
                  ("*;"  . "**")
                  ("<-"  . " <<-")
                  ("<~"  . " <<~")
                  ("|"   . (" |`!!'|" " | " "|"))
                  ("{"   . (" { `!!'}" "{}")))))

  (key-combo-define-hook my-ruby-mode-hooks
                         'my-key-combo-ruby-hook
                         my-key-combos-for-ruby)

  (defvar my-js2-mode-hooks
    '(js2-mode-hook))

  (setq my-key-combos-for-javascript
        (append my-key-combos-common
                '(("<"   . (" < " "<"))
                  (">"   . (" > " ">"))
                  ("=:"  . " === ")
                  ("=>"  . " => ")
                  ("+;"  . "++")
                  ("-;"  . "--")
                  ("%"   . (" % " "%"))
                  ("!==" . " !== " )
                  ("&"   . (" & " "&"))
                  ("*"   . (" * " "*"))
                  ("*;"  . "**")
                  ("/"   . ( " / " "/"))
                  ("/;"  . "/`!!'/")
                  ("\""  . ("\"`!!'\"" "\""))
                  ("(;"  . "(`!!');")
                  ("[;"  . "[`!!'];"))))

  (key-combo-define-hook my-js2-mode-hooks
                         'my-key-combo-js2-hook
                         my-key-combos-for-javascript)

  (defvar my-sh-mode-hooks
    '(sh-mode-hook))

  (setq my-key-combos-for-sh
      '(("-eq"  . " -eq ")
        ("-ne"  . " -ne ")
        ("-gt"  . " -gt ")
        ("-lt"  . " -lt ")
        ("-ge"  . " -ge ")
        ("-le"  . " -le ")
        ("-nt"  . " -nt ")
        ("-ot"  . " -ot ")))

  (key-combo-define-hook my-sh-mode-hooks
                         'my-key-combo-sh-hook
                         my-key-combos-for-sh)

  (defvar my-go-mode-hooks
    '(go-mode-hook))

  (setq my-key-combos-for-go
        (append my-key-combos-common
                '((":=" . " := ")
                  ("+;" . "++")
                  ("-;" . "--"))))

  (key-combo-define-hook my-go-mode-hooks
                         'my-key-combo-go-hoo
                         my-key-combos-for-go))



;;; ssh接続をするためのパッケージ
(package-install 'ssh)
(when (require 'ssh nil t)
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq dirtrackp nil))))



;;; シンボルをハイライト
(package-install 'symbol-overlay)
(when (require 'symbol-overlay nil t)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all))


;;; yankした箇所をハイライト
(package-install 'volatile-highlights)
(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t)

  (when (require 'undo-tree nil t)
    (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
    (vhl/install-extension 'undo-tree)))



;;; 画面分割したバッファの位置を入れ替える
(package-install 'swap-buffers)
(when (require 'swap-buffers nil t)
  (global-set-key (kbd "C-x 4") 'swap-buffers))



;;; 開発中のプログラムを実行
(package-install 'quickrun)
(when (require 'quickrun nil t)
  (global-set-key (kbd "C-c C-c") 'quickrun-with-arg))
