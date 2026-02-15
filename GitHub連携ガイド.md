# Cursor と GitHub 連携ガイド — semi_lesson 教材配布

このガイドでは、**先生（あなた）**が教材を GitHub にアップロード・更新し、**学生**がダウンロードして自分のパソコンで自由に編集・実行する流れを説明します。学生は GitHub 上のオリジナルを編集しません。

---

## 1. Cursor と GitHub の連携（初回のみ）

### 1-1. GitHub アカウント

- [GitHub](https://github.com) にアカウントがある前提です（既に `nakanenaruhisa/semi_lesson` を使っているので問題ないはずです）。

### 1-2. Cursor で Git 認証（HTTPS の場合）

**方法A: GitHub で「Personal Access Token」を使う（推奨）**

1. GitHub → 右上アイコン → **Settings** → 左メニュー最下部 **Developer settings** → **Personal access tokens** → **Tokens (classic)**  
2. **Generate new token (classic)** で、名前（例: `cursor-semi_lesson`）と有効期限を設定。  
   - **Scope**: 少なくとも `repo` にチェック。  
3. 表示されたトークンをコピー（再表示されないので保管しておく）。

4. Cursor で `semi_lesson` フォルダを開いた状態で、**ターミナル**（`` Ctrl+` `` または メニュー **Terminal → New Terminal**）を開く。

5. 次のコマンドでプッシュなどを行うと、ユーザー名とパスワードを聞かれることがあります。
   - **Username**: あなたの GitHub のユーザー名（例: `nakanenaruhisa`）
   - **Password**: ここには **通常のパスワードではなく、先ほど作成した Personal Access Token** を入力する。

**方法B: GitHub CLI でログイン（別の選択肢）**

- ターミナルで `gh auth login` を実行し、ブラウザで GitHub にログインすると、以降は Cursor のターミナルから `git push` などがしやすくなります。

### 1-3. Cursor で Git を使う場所

- **ソース管理アイコン**（左サイドバーの分岐マーク）でコミット・プッシュ・プルができます。  
- または **ターミナル**で `git add` / `git commit` / `git push` を実行しても同じです。

---

## 2. 先生側：教材のアップロード・更新の流れ

### 2-1. リポジトリの状態（いま）

- リモート: `https://github.com/nakanenaruhisa/semi_lesson`
- 現在の教材（Lesson0〜15、`folder_format`、`sample_dataset`、`応用編` など）はまだ Git に追加されていない可能性があります。

### 2-2. 初回：教材を GitHub に上げる

1. **Cursor で `semi_lesson` フォルダを開く**（ファイル → フォルダーを開く）。

2. **除外したいファイルを `.gitignore` で指定する（推奨）**  
   リポジトリの**ルート**（`semi_lesson` 直下）に `.gitignore` があるか確認し、なければ作成。次のような内容を入れておくと安全です。

   ```gitignore
   # R の一時ファイル・個人用
   .RData
   .RDataTmp
   .Rhistory
   .Rproj.user/
   *.Rproj.user/
   
   # macOS
   .DS_Store
   
   # 秘密のキーワードなど、公開したくないフォルダがあれば追加
   # Lesson0_秘密のキーワード/
   ```

   - **「Lesson0_秘密のキーワード」を公開したくない場合は**、上記の `#` を外してその行を有効にしてください。

3. **ターミナルで実行**（`semi_lesson` フォルダがカレントであることを確認）：

   ```bash
   git add .
   git status
   ```
   - `git status` で、上げたくないファイルが含まれていないか確認。

4. 問題なければコミットとプッシュ：

   ```bash
   git commit -m "教材一式を追加（Lesson0-15, sample_dataset, 応用編など）"
   git push -u origin main
   ```
   - ブランチ名が `main` でない場合は、`git branch` で確認し、`main` の部分を実際のブランチ名に置き換えてください（例: `master` → `origin master`）。

### 2-3. 教材を改変したときの更新

1. Cursor で該当ファイルを編集・保存。
2. ソース管理パネルまたはターミナルで：
   ```bash
   git add .
   git commit -m "〇〇の説明を更新 / データを差し替え など"
   git push origin main
   ```
3. これで GitHub 上の「オリジナル」が更新されます。学生は後述の方法で最新を取得できます。

---

## 3. 学生側：ダウンロードとローカルでの利用

学生には次のように案内できます。

### 3-1. 教材の取得（2つの方法）

**方法1: クローン（推奨 — 後から更新を取得しやすい）**

1. [Git for Windows](https://git-scm.com/download/win) または Mac では Xcode コマンドラインツールなどで Git をインストール。
2. コマンドラインまたは Cursor のターミナルで、**教材を置きたいフォルダ**に移動してから：
   ```bash
   git clone https://github.com/nakanenaruhisa/semi_lesson.git
   ```
3. `semi_lesson` フォルダができるので、その中で R や RStudio / Cursor を使って編集・実行。

**方法2: ZIP でダウンロード（Git を使わない場合）**

1. ブラウザで `https://github.com/nakanenaruhisa/semi_lesson` を開く。
2. 緑の **Code** ボタン → **Download ZIP** で ZIP を保存。
3. 解凍したフォルダを任意の場所に置き、その中で R を実行。

### 3-2. 学生のルール

- **GitHub 上のオリジナルは編集しない**（学生に書き込み権限を渡していないため、通常はプッシュできません）。
- 教材は**自分のパソコン（ローカル）**で自由に編集・実行してよい。
- 先生が教材を更新した場合：
  - **clone した人**は、教材フォルダで `git pull origin main` を実行すると最新版を取得できる（ローカルで編集したファイルと競合する場合は、別フォルダに clone し直すか、バックアップを取ってから pull するよう案内する）。
  - **ZIP で取得した人**は、再度 GitHub から ZIP をダウンロードして、必要な部分だけ上書きするか、新しいフォルダに解凍して使う。

---

## 4. よくある操作まとめ（Cursor 内）

| やりたいこと           | 操作 |
|------------------------|------|
| 変更を GitHub に送る   | `git add .` → `git commit -m "メッセージ"` → `git push origin main` |
| GitHub の最新を取る    | `git pull origin main` |
| 状態確認               | `git status` |
| リモート確認           | `git remote -v` |

---

## 5. トラブルシューティング

- **プッシュ時に認証エラー**  
  - HTTPS の場合は、パスワード欄に **Personal Access Token** を入力しているか確認。  
  - または `gh auth login` で GitHub CLI にログインしてみる。

- **「ブランチ main がない」**  
  - `git branch` で現在のブランチを確認。`master` の場合は `git push -u origin master` のようにブランチ名を合わせる。

- **学生が「更新を取れない」**  
  - clone したフォルダで `git pull origin main` を実行するよう案内。  
  - ローカルでたくさん編集している場合は、別フォルダに新しく clone して「最新版」として使う方法を案内する。

---

このガイドは `semi_lesson` フォルダ用です。リポジトリ名やブランチ名を変えた場合は、文中の `nakanenaruhisa/semi_lesson` と `main` を読み替えてください。
