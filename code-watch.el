;;; code-watch.el --- Emacs frontend for cw (code-watch) CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Wasu

;; Author: Wasu
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, project
;; URL: https://github.com/wasuken/dotfiles

;;; Commentary:

;; Emacs frontend for the `cw` CLI tool.
;; Provides commands to initialize, scan, show details, add notes, and find files.

;;; Code:

(defun cw--find-project-root ()
  "カレントバッファのディレクトリから.codewatch/を探索して返す。"
  (let ((dir (locate-dominating-file default-directory ".codewatch")))
    (if dir
        (expand-file-name dir)
      (error "Could not find .codewatch/ directory in this or any parent directory"))))

(defun cw--find-project-root-for-init ()
  "Find the directory where `cw init` should be run."
  (expand-file-name
   (or (locate-dominating-file default-directory ".git")
       (and (fboundp 'project-current)
            (when-let ((proj (project-current)))
              (project-root proj)))
       (and (fboundp 'vc-root-dir)
            (vc-root-dir))
       default-directory)))

(defun cw--get-file-list ()
  "cw listの出力をリストで返す。"
  (let* ((default-directory (cw--find-project-root))
         (output (shell-command-to-string "cw list"))
         (lines (split-string output "\n" t)))
    (if (member "No files in index." lines)
        nil
      lines)))

(defun cw--reorder-with-current (candidates)
  "現在のバッファのファイルをCANDIDATESの先頭に移動する。"
  (if-let* ((file (buffer-file-name))
            (root (ignore-errors (cw--find-project-root)))
            (rel (file-relative-name file root))
            ((member rel candidates)))
      (cons rel (delete rel candidates))
    candidates))

(defun cw--select-file (&optional candidates)
  "Verticoでファイルを選択して返す。
CANDIDATES が指定されていればそれを使用し、指定されていなければ `cw--get-file-list` から取得する。"
  (let ((files (or candidates (cw--get-file-list))))
    (if (null files)
        (error "No files in code-watch index. Run M-x cw-scan first")
      (completing-read "File: " files nil t))))

(defun cw--extract-hash (show-output)
  "cw showの出力からハッシュ値を抽出する。"
  (if (string-match "\\(?:\\`\\|[\n\r]\\)Hash:[ \t]*\\([a-f0-9]+\\)" show-output)
      (match-string 1 show-output)
    (error "Could not extract hash from cw show output:\n%s" show-output)))

(defun cw--display-output (buf-name content)
  "BUF-NAMEバッファにCONTENTを表示する。"
  (with-current-buffer (get-buffer-create buf-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert content)
    (read-only-mode 1)
    (display-buffer (current-buffer))))

;;;###autoload
(defun cw-init ()
  "M-x cw-init: cw init を実行し、結果を *code-watch* バッファに表示。"
  (interactive)
  (let* ((root (cw--find-project-root-for-init))
         (default-directory root)
         (output (shell-command-to-string "cw init")))
    (cw--display-output "*code-watch*" output)))

;;;###autoload
(defun cw-scan ()
  "M-x cw-scan: cw scan を非同期で実行。実行中は *code-watch* バッファにプログレスを流す。"
  (interactive)
  (let* ((root (cw--find-project-root))
         (default-directory root)
         (buf (get-buffer-create "*code-watch*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "Starting cw scan...\n")
      (read-only-mode 1))
    (display-buffer buf)
    (let ((process (start-process "cw-scan" buf "cw" "scan")))
      (set-process-filter
       process
       (lambda (proc string)
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (save-excursion
                 (goto-char (process-mark proc))
                 (let ((first t))
                   (dolist (chunk (split-string string "\r"))
                     (if first
                         (setq first nil)
                       (goto-char (line-beginning-position)))
                     (when (not (string= chunk ""))
                       (when (= (point) (line-beginning-position))
                         (delete-region (point) (line-end-position)))
                       (insert chunk)))
                 (set-marker (process-mark proc) (point)))))))))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (eq (process-status proc) 'exit)
           (let ((code (process-exit-status proc)))
             (with-current-buffer (process-buffer proc)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (if (zerop code)
                     (progn
                       (insert "\nScan complete successfully.\n")
                       (message "code-watch scan complete."))
                   (insert (format "\nScan failed with exit code %d.\n" code))
                   (message "code-watch scan failed.")))))))))))

;;;###autoload
(defun cw-find-file ()
  "M-x cw-find-file: cw list の出力をVertico経由で選択し、そのファイルを `find-file` で開く。"
  (interactive)
  (let* ((file (cw--select-file))
         (root (cw--find-project-root))
         (abs-path (expand-file-name file root)))
    (find-file abs-path)))

;;;###autoload
(defun cw-note ()
  "M-x cw-note: cw note <file> を実行し、ソースファイルとノートファイルを左右に並べて開く。"
  (interactive)
  (let* ((file (cw--select-file (cw--reorder-with-current (cw--get-file-list))))
         (root (cw--find-project-root))
         (source-path (expand-file-name file root))
         (show-output (let ((default-directory root))
                        (shell-command-to-string (format "cw show %s" (shell-quote-argument file)))))
         (hash (cw--extract-hash show-output))
         (note-dir (expand-file-name ".codewatch/notes" root))
         (note-path (expand-file-name (format "%s.md" hash) note-dir)))
    (unless (file-directory-p note-dir)
      (make-directory note-dir t))
    (find-file source-path)
    (let ((right-window (split-window-right)))
      (select-window right-window)
      (find-file note-path)
      (when (= (buffer-size) 0)
	(insert (format "# %s\n\n理解度: 0/5\n最終確認: %s\n\n<!-- ここにメモを書く -->\n\n# 役割\n# 構造\n# 疑問・気になった点"
			file
			(format-time-string "%Y-%m-%d"))))
      (goto-char (point-min)))))

;;;###autoload
(defun cw-show ()
  "M-x cw-show: cw list の出力をVertico経由で選択し、cw show <file> の結果を *code-watch-show* バッファに表示。"
  (interactive)
  (let* ((file (cw--select-file))
         (root (cw--find-project-root))
         (output (let ((default-directory root))
                   (shell-command-to-string (format "cw show %s" (shell-quote-argument file))))))
    (cw--display-output "*code-watch-show*" output)))

;;; 追加分 — code-watch.el に追記してください
;;; (provide 'code-watch) の直前に挿入)

(defun cw--get-file-list-opts (&optional noted sort)
  "cw list を呼ぶ。NOTED が non-nil なら --noted、SORT は \"recent\" 等。"
  (let* ((default-directory (cw--find-project-root))
         (args (append '("list")
                       (when noted '("--noted"))
                       (when sort (list (format "--sort=%s" sort)))))
         (output (apply #'shell-command-to-string
                        (list (mapconcat #'shell-quote-argument
                                         (cons "cw" args) " "))))
         (lines (split-string output "\n" t)))
    (if (member "No files in index." lines) nil lines)))

(defun cw--select-file-opts (&optional noted sort)
  "フィルタ付きでファイルを選択する。"
  (let ((files (cw--get-file-list-opts noted sort)))
    (if (null files)
        (error "No files found. Run M-x cw-scan first")
      (completing-read "File: " files nil t))))

;;;###autoload
(defun cw-find-file-noted ()
  "ノート済みファイルのみ一覧表示して開く。"
  (interactive)
  (let* ((file (cw--select-file-opts t nil))
         (root (cw--find-project-root))
         (abs-path (expand-file-name file root)))
    (find-file abs-path)))

;;;###autoload
(defun cw-find-file-recent ()
  "ノート更新順でファイルを選択して開く。"
  (interactive)
  (let* ((file (cw--select-file-opts nil "recent"))
         (root (cw--find-project-root))
         (abs-path (expand-file-name file root)))
    (find-file abs-path)))

;;;###autoload
(defun cw-search (query)
  "QUERY でノートファイルを rgrep 検索する。"
  (interactive "sSearch notes: ")
  (let* ((root (cw--find-project-root))
         (notes-dir (expand-file-name ".codewatch/notes" root)))
    (unless (file-directory-p notes-dir)
      (error "No notes directory found. Run cw-scan and add some notes first"))
    (rgrep query "*.md" notes-dir)))

(defun cw--open-note-for-file (file root)
  "FILE のノートを find-file で開く（ウィンドウ分割なし）。"
  (let* ((show-output (let ((default-directory root))
                        (shell-command-to-string
                         (format "cw show %s" (shell-quote-argument file)))))
         (hash (condition-case nil
                   (cw--extract-hash show-output)
                 (error nil)))
         (note-dir (expand-file-name ".codewatch/notes" root)))
    (when hash
      (let ((note-path (expand-file-name (format "%s.md" hash) note-dir)))
        (when (file-exists-p note-path)
          note-path)))))

(defun cw--auto-show-note ()
  "find-file-hook 用: 対応ノートがあれば右側に開く。"
  (when (and buffer-file-name
             (not (string-match-p "\.codewatch" buffer-file-name)))
    (condition-case nil
        (let* ((root (cw--find-project-root))
               (rel (file-relative-name buffer-file-name root))
               (note-path (cw--open-note-for-file rel root)))
          (when note-path
            (let ((src-win (selected-window)))
              (let ((right-win (split-window-right)))
                (select-window right-win)
                (find-file note-path)
                (select-window src-win)))))
      (error nil))))

(add-hook 'find-file-hook #'cw--auto-show-note)

(defvar-local cw-top--offset 0
  "Current offset in `cw-top-mode'.")

(defvar-local cw-top--n 10
  "Number of items per page in `cw-top-mode'.")

(defun cw-top--has-items-p (output)
  "Return non-nil if OUTPUT contains any file items."
  (let ((lines (split-string output "\n" t))
        (has-items nil))
    (dolist (line lines)
      (when (string-match "^[ \t]*[0-9]+\\.[ \t]+[0-9]+\\.[0-9]+" line)
        (setq has-items t)))
    has-items))

(defun cw-top--refresh ()
  "Refresh the `cw-top` buffer with the current offset and limit."
  (let* ((root default-directory)
         (n cw-top--n)
         (offset cw-top--offset)
         (cmd (format "cw top --n=%d --offset=%d" n offset))
         (output (let ((default-directory root))
                   (shell-command-to-string cmd))))
    (if (and (> offset 0) (not (cw-top--has-items-p output)))
        (progn
          (message "No more files.")
          nil)
      (let ((inhibit-read-only t)
            (page (+ (/ offset n) 1)))
        (erase-buffer)
        (insert (format "[Page %d / offset %d]\n" page offset))
        (insert output)
        (goto-char (point-min))
        t))))

;;;###autoload
(defun cw-show-top (&optional n)
  "cw top の結果を *code-watch* バッファに表示する。"
  (interactive "P")
  (let* ((root (cw--find-project-root))
         (n-val (if n (prefix-numeric-value n) 10))
         (buf (get-buffer-create "*code-watch*")))
    (with-current-buffer buf
      (cw-top-mode)
      (setq-local default-directory root)
      (setq cw-top--n n-val)
      (setq cw-top--offset 0)
      (cw-top--refresh))
    (display-buffer buf)))

(defun cw-top-next-page ()
  "Go to the next page of top files."
  (interactive)
  (let ((old-offset cw-top--offset))
    (setq cw-top--offset (+ cw-top--offset cw-top--n))
    (unless (cw-top--refresh)
      (setq cw-top--offset old-offset))))

(defun cw-top-prev-page ()
  "Go to the previous page of top files."
  (interactive)
  (if (<= cw-top--offset 0)
      (message "Already at the first page.")
    (let ((old-offset cw-top--offset))
      (setq cw-top--offset (max 0 (- cw-top--offset cw-top--n)))
      (unless (cw-top--refresh)
        (setq cw-top--offset old-offset)))))

(defvar cw-top-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cw-top-open-file)
    (define-key map (kbd "n") #'cw-top-next-page)
    (define-key map (kbd "p") #'cw-top-prev-page)
    map)
  "Keymap for `cw-top-mode'.")

(define-derived-mode cw-top-mode special-mode "cw-top"
  "Major mode for displaying code-watch top results.")

(defun cw-top-open-file ()
  "Open the file on the current line in `cw-top-mode`."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (string-match "[0-9]+\\.[0-9]+[ \t]+\\([^ \t\n]+\\)[ \t]+(\\([^ \t\n]+\\)" line)
        (let* ((file (match-string 1 line))
               (root (cw--find-project-root))
               (abs-path (expand-file-name file root)))
          (if (file-exists-p abs-path)
              (find-file abs-path)
            (error "File does not exist: %s" abs-path)))
      (message "No file path found on this line"))))

;;;###autoload
(defun cw-note-open ()
  "現在開いているバッファのファイルに対応するノートを右側に開く。"
  (interactive)
  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))
  (let* ((root (cw--find-project-root))
         (file (file-relative-name buffer-file-name root))
         (show-output (let ((default-directory root))
                        (shell-command-to-string (format "cw show %s" (shell-quote-argument file)))))
         (hash (cw--extract-hash show-output))
         (note-dir (expand-file-name ".codewatch/notes" root))
         (note-path (expand-file-name (format "%s.md" hash) note-dir)))
    (unless (file-directory-p note-dir)
      (make-directory note-dir t))
    (let ((right-window (split-window-right)))
      (select-window right-window)
      (find-file note-path)
      (when (= (buffer-size) 0)
        (insert (format "# %s\n\n理解度: 0/5\n最終確認: %s\n\n<!-- ここにメモを書く -->\n\n# 役割\n# 構造\n# 疑問・気になった点"
                        file
                        (format-time-string "%Y-%m-%d"))))
      (goto-char (point-min)))))

;;;###autoload
(defun cw-report (&optional n)
  "cw report の結果を *code-watch-report* バッファに表示する。"
  (interactive "P")
  (let* ((root (cw--find-project-root))
         (default-directory root)
         (arg (if n (format "--n=%d" (prefix-numeric-value n)) "--n=10"))
         (output (shell-command-to-string (format "cw report %s" arg))))
    (cw--display-output "*code-watch-report*" output)))

(provide 'code-watch)
;;; code-watch.el ends here
