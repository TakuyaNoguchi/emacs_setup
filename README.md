# emacs_setup

## 前提条件

OSはUbuntuを想定

## セットアップ手順

1. init.elを ~/.emacs.d に配置する
2. Emacs で init.elを開き、package-install の行を `C-x C-e`で評価し実行する
  * 対象行は `$ sed -n '/;; (package-install/p' ~/.emacs.d/init.el` で確認可能
3. init.el 内の M-x で始まるコマンドを Emacsで実行
  * 対象行は `$ sed -n '/;; M-x/p' ~/.emacs.d/init.el` で確認可能
4. init.el に記述されたコマンドを端末で実行
  * 対象行は `$ sed -n '/;; \$/p' ~/.emacs.d/init.el` で確認可能