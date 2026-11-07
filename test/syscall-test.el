;;; syscall-test.el --- Tests for syscall.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; The API is replaced by `syscall-test--server', a table of canned
;; responses per URL, so these tests never touch the network.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'syscall)

(defconst syscall-test--kernels
  "[{\"version\":\"3.3\",\"arches\":[\"x64\"]},{\"version\":\"3.6\",\"arches\":[\"arm64\",\"x64\"]},{\"version\":\"3.10\",\"arches\":[\"arm64\",\"x64\"]},{\"version\":\"7.3\",\"arches\":[\"arm64\",\"x64\"]}]")

(defconst syscall-test--conventions
  "[{\"arch\":\"x64\",\"instruction\":\"syscall\",\"nr\":\"rax\",\"return\":\"rax\",\"arg0\":\"rdi\",\"arg1\":\"rsi\",\"arg2\":\"rdx\",\"arg3\":\"r10\",\"arg4\":\"r8\",\"arg5\":\"r9\",\"notes\":[\"The syscall instruction clobbers rcx and r11.\"]},{\"arch\":\"arm64\",\"instruction\":\"svc #0\",\"nr\":\"x8\",\"return\":\"x0\",\"arg0\":\"x0\",\"arg1\":\"x1\",\"arg2\":\"x2\",\"arg3\":\"x3\",\"arg4\":\"x4\",\"arg5\":\"x5\"}]")

(defconst syscall-test--x64
  "[{\"arch\":\"x64\",\"nr\":0,\"name\":\"read\",\"description\":\"read from a file descriptor\",\"nargs\":3,\"arg0\":\"unsigned int fd\",\"arg1\":\"char *buf\",\"arg2\":\"size_t count\",\"arg3\":null,\"since\":\"3.3\",\"removed_in\":null},{\"arch\":\"x64\",\"nr\":59,\"name\":\"oldcall\",\"description\":\"gone in 3.10\",\"family\":\"epoll\",\"kconfig\":\"CONFIG_EPOLL\",\"nargs\":0,\"since\":\"3.3\",\"removed_in\":\"3.10\"},{\"arch\":\"x64\",\"nr\":60,\"name\":\"newcall\",\"description\":\"new in 3.10\",\"nargs\":null,\"since\":\"3.10\",\"removed_in\":null}]")

(defconst syscall-test--arm64
  "[{\"arch\":\"arm64\",\"nr\":63,\"name\":\"read\",\"since\":\"3.7\",\"removed_in\":null}]")

(defvar syscall-test--server nil
  "Alist of endpoint -> list of responses (CODE ETAG BODY), used in order.
The last response repeats.  A CODE of nil simulates a network error.")

(defvar syscall-test--requests nil
  "Requests seen, newest first, as (ENDPOINT . IF-NONE-MATCH).")

(defvar syscall-test--legacy-update t
  "Use the legacy update fixtures when non-nil.")

(defun syscall-test--fake-get (url etag callback)
  "Answer URL from `syscall-test--server', asynchronously like url.el.
ETAG is recorded; CALLBACK gets the canned response."
  (let* ((endpoint (string-remove-prefix syscall-api-url url))
         (cell (assoc endpoint syscall-test--server))
         (response (if cell
                       (if (cddr cell) (pop (cdr cell)) (cadr cell))
                     '(404 nil "{\"title\":\"Not Found\"}"))))
    (push (cons endpoint etag) syscall-test--requests)
    (pcase-let ((`(,code ,new-etag ,body) response))
      (run-at-time 0 nil callback code new-etag
                   (and body (encode-coding-string body 'utf-8))
                   (unless code "connection refused")))
    nil))

(defun syscall-test--ok-server ()
  "Responses of a healthy API."
  (list (list "/kernels" (list 200 "\"k1\"" syscall-test--kernels))
        (list "/conventions" (list 200 "\"c1\"" syscall-test--conventions))
        (list "/syscalls/x64?kernel=all" (list 200 "\"x1\"" syscall-test--x64))
        (list "/syscalls/arm64?kernel=all" (list 200 "\"a1\"" syscall-test--arm64))))

(defmacro syscall-test--with-cache (&rest body)
  "Run BODY with an empty temporary cache and the fake API."
  (declare (indent 0) (debug t))
  `(ert-with-temp-directory syscall-cache-directory
     (let ((syscall-api-url "https://api.test/v2")
           (syscall-request-delay 0)
           (syscall-retry-delay 0)
           (syscall-auto-update-interval nil)
           (syscall-default-arch "x64")
           (syscall-default-kernel nil)
           (syscall-test--requests nil)
           (syscall-test--server (syscall-test--ok-server))
           (syscall--job nil))
       (syscall--reset)
       (let ((next (symbol-function 'syscall--job-next)))
       (cl-letf (((symbol-function 'syscall--http-get) #'syscall-test--fake-get)
                 ((symbol-function 'syscall--job-next)
                  (lambda (job)
                    (when (and syscall-test--legacy-update
                               (eq (plist-get (car (syscall--job-queue job)) :kind) 'manifest))
                      (setf (syscall--job-queue job)
                            (list (syscall--resource 'kernels "/kernels" "kernels.json")
                                  (syscall--resource 'conventions "/conventions" "conventions.json"))))
                    (funcall next job)))
                 ((symbol-function 'syscall--bundled-available-p) (lambda () nil)))
         (unwind-protect (progn ,@body)
           (syscall--reset)))))))

(defun syscall-test--update (&optional force)
  "Run an update to the end, returning (OK RESULT)."
  (let (result)
    (syscall--start-update force t (lambda (ok r) (setq result (list ok r))))
    (with-timeout (5 (error "Update did not finish"))
      (while (not result) (accept-process-output nil 0.01)))
    result))

(defun syscall-test--file (file)
  "Contents of the cache-relative FILE."
  (with-temp-buffer
    (insert-file-contents (syscall--path file))
    (buffer-string)))

(ert-deftest syscall-test-bundled-data ()
  (ert-with-temp-directory syscall-cache-directory
    (let ((syscall-auto-update-interval nil))
      (syscall--reset)
      (unwind-protect
          (progn
            (should (syscall--ready-p))
            (should (syscall-ensure-data))
            (should (member "x64" (syscall-arches)))
            (should (syscall-find "x64" "read"))
            (should (syscall-conventions)))
        (syscall--reset)))))

(ert-deftest syscall-test-dataset-manifest ()
  (syscall-test--with-cache
    (let* ((syscall-test--legacy-update nil)
           (dataset (concat "{\"kernels\":[{\"version\":\"3.3\",\"arch\":\"x64\"}],"
                            "\"conventions\":" syscall-test--conventions
                            ",\"syscalls\":" syscall-test--x64 "}"))
           (hash (secure-hash 'sha256 dataset))
           (manifest (format "{\"json\":{\"size_bytes\":%d,\"sha256\":\"%s\"}}"
                             (length dataset) hash)))
      (setq syscall-test--server
            (list (list "/dataset" (list 200 nil manifest))
                  (list "/dataset/syscalls.json" (list 200 nil dataset))))
      (should (equal (syscall-test--update) '(t 1)))
      (should (equal hash (plist-get (syscall--read-meta) :dataset-sha256)))
      (should (equal "read" (alist-get 'name (car (syscall-arch-syscalls "x64")))))
      (setq syscall-test--requests nil)
      (should (equal (syscall-test--update) '(t 0)))
      (should (equal (mapcar #'car syscall-test--requests) '("/dataset")))
      (setq syscall-test--requests nil)
      (should (equal (syscall-test--update t) '(t 1)))
      (should (= 2 (length syscall-test--requests)))
      (setf (cadr (assoc "/dataset/syscalls.json" syscall-test--server))
            '(200 nil "bad"))
      (should-not (car (syscall-test--update t)))
      (should (equal hash (plist-get (syscall--read-meta) :dataset-sha256))))))

(ert-deftest syscall-test-matching-bundle-replaces-legacy-cache ()
  (syscall-test--with-cache
    (syscall-test--update)
    (should (syscall--cache-complete-p))
    (let* ((syscall-test--legacy-update nil)
           (syscall-api-url "https://api.syscall.sh/v2")
           (manifest (format "{\"json\":{\"size_bytes\":1,\"sha256\":\"%s\"}}"
                             (progn (require 'syscall-data) syscall-data--sha256))))
      (setq syscall-test--requests nil
            syscall-test--server (list (list "/dataset" (list 200 nil manifest))))
      (cl-letf (((symbol-function 'syscall--bundled-available-p) (lambda () t)))
        (should (equal (syscall-test--update) '(t 0)))
        (should (equal (mapcar #'car syscall-test--requests) '("/dataset")))
        (should (plist-get (syscall--read-meta) :use-bundled))
        (should-not (syscall--cache-complete-p))
        (should (syscall-find "x64" "read"))))))

(ert-deftest syscall-test-first-update ()
  (syscall-test--with-cache
    (should (equal (syscall-test--update) '(t 4)))
    (should (syscall--cache-complete-p))
    (should (equal (syscall-arches) '("arm64" "x64")))
    (should (equal (syscall-test--file "arch/x64.json") syscall-test--x64))
    (should (equal (alist-get 'name (car (syscall-arch-syscalls "x64"))) "read"))
    (should (null (alist-get 'removed_in (car (syscall-arch-syscalls "x64")))))
    (should (equal (cdr (assoc "arch/arm64.json" (plist-get (syscall--read-meta) :etags)))
                   "\"a1\""))
    (should-not (file-exists-p (syscall--staging-dir)))
    (should (cl-every (lambda (r) (null (cdr r))) syscall-test--requests))))

(ert-deftest syscall-test-not-modified ()
  (syscall-test--with-cache
    (syscall-test--update)
    (let ((before (plist-get (syscall--read-meta) :fetched-at)))
      (setq syscall-test--requests nil
            syscall-test--server
            (mapcar (lambda (e) (list e (list 304 nil nil)))
                    '("/kernels" "/conventions" "/syscalls/x64?kernel=all"
                      "/syscalls/arm64?kernel=all")))
      (should (equal (syscall-test--update) '(t 0)))
      (should (equal (cdr (assoc "/syscalls/x64?kernel=all" syscall-test--requests)) "\"x1\""))
      (should (equal (syscall-test--file "arch/x64.json") syscall-test--x64))
      (should (equal (cdr (assoc "kernels.json" (plist-get (syscall--read-meta) :etags))) "\"k1\""))
      (should (>= (plist-get (syscall--read-meta) :fetched-at) before)))))

(ert-deftest syscall-test-force-ignores-etags ()
  (syscall-test--with-cache
    (syscall-test--update)
    (setq syscall-test--requests nil)
    (should (equal (syscall-test--update t) '(t 4)))
    (should (cl-every (lambda (r) (null (cdr r))) syscall-test--requests))))

(ert-deftest syscall-test-retries-rate-limit ()
  (syscall-test--with-cache
    (setf (alist-get "/conventions" syscall-test--server nil nil #'equal)
          (list '(429 nil "") '(nil nil nil) '(503 nil "")
                (list 200 "\"c1\"" syscall-test--conventions)))
    (should (equal (syscall-test--update) '(t 4)))
    (should (= 4 (cl-count "/conventions" syscall-test--requests :key #'car :test #'equal)))))

(ert-deftest syscall-test-gives-up-and-keeps-cache ()
  (syscall-test--with-cache
    (syscall-test--update)
    (let ((meta (syscall--read-meta))
          (syscall-max-attempts 3))
      (setq syscall-test--requests nil)
      (setf (alist-get "/syscalls/x64?kernel=all" syscall-test--server nil nil #'equal)
            (list '(500 nil "boom")))
      (setf (alist-get "/kernels" syscall-test--server nil nil #'equal)
            (list (list 200 "\"k2\"" "[{\"version\":\"7.4\",\"arches\":[\"arm64\",\"x64\"]}]")))
      (pcase-let ((`(,ok ,reason) (syscall-test--update)))
        (should-not ok)
        (should (string-match-p "HTTP 500 boom" reason)))
      (should (= 3 (cl-count "/syscalls/x64?kernel=all" syscall-test--requests
                             :key #'car :test #'equal)))
      (should (equal (syscall--read-meta) meta))
      (should (equal (syscall-test--file "kernels.json") syscall-test--kernels))
      (should-not (file-exists-p (syscall--staging-dir)))
      (should-not syscall--job))))

(ert-deftest syscall-test-client-error-not-retried ()
  (syscall-test--with-cache
    (setf (alist-get "/conventions" syscall-test--server nil nil #'equal)
          (list '(404 nil "{\"title\":\"Not Found\"}")))
    (should-not (car (syscall-test--update)))
    (should (= 1 (cl-count "/conventions" syscall-test--requests :key #'car :test #'equal)))
    (should-not (syscall--cache-complete-p))))

(ert-deftest syscall-test-rejects-empty-arch ()
  (syscall-test--with-cache
    (setf (alist-get "/syscalls/x64?kernel=all" syscall-test--server nil nil #'equal)
          (list '(200 nil "[]")))
    (pcase-let ((`(,ok ,reason) (syscall-test--update)))
      (should-not ok)
      (should (string-match-p "empty" reason)))
    (should-not (file-exists-p (syscall--path "kernels.json")))
    (should-not (syscall--read-meta))))

(ert-deftest syscall-test-rejects-invalid-json ()
  (syscall-test--with-cache
    (setf (alist-get "/kernels" syscall-test--server nil nil #'equal)
          (list '(200 nil "<html>")))
    (should-not (car (syscall-test--update)))
    (should-not (syscall--read-meta))))

(ert-deftest syscall-test-one-update-at-a-time ()
  (syscall-test--with-cache
    (syscall--start-update nil t nil)
    (should-error (syscall--start-update nil t nil) :type 'user-error)
    (syscall-cancel-update)
    (should-not syscall--job)))

(ert-deftest syscall-test-incomplete-cache ()
  (syscall-test--with-cache
    (syscall-test--update)
    (should (syscall--cache-complete-p))
    (delete-file (syscall--path "arch/arm64.json"))
    (should-not (syscall--cache-complete-p))
    (should-error (syscall-ensure-data) :type 'user-error)))

(ert-deftest syscall-test-meta-schema-mismatch ()
  (syscall-test--with-cache
    (syscall-test--update)
    (syscall--write-meta (list :schema 0 :arches '("x64")))
    (should-not (syscall--read-meta))))

(ert-deftest syscall-test-auto-update-when-stale ()
  (syscall-test--with-cache
    (syscall-test--update)
    (let ((syscall--auto-update-tried nil)
          (syscall-auto-update-interval 30))
      (syscall--maybe-auto-update (syscall--read-meta))
      (should-not syscall--job)
      (syscall--maybe-auto-update (list :fetched-at (- (float-time) (* 31 86400))))
      (should syscall--job)
      (should syscall--auto-update-tried)
      (syscall-cancel-update))))

;;;; Milestones 2-5: data model, buffers, stubs, ElDoc

(defmacro syscall-test--with-data (&rest body)
  "Run BODY with the fixture data in the cache."
  (declare (indent 0) (debug t))
  `(syscall-test--with-cache
     (syscall-test--update)
     ,@body))

(defun syscall-test--names (table)
  "Names in TABLE."
  (mapcar (lambda (sc) (alist-get 'name sc)) table))

(ert-deftest syscall-test-version ()
  (should (< (syscall--version "3.9") (syscall--version "3.10")))
  (should (= (syscall--version "6.10") 6010))
  (should-error (syscall--version "6")))

(ert-deftest syscall-test-kernel-tables ()
  (syscall-test--with-data
    (should (equal (syscall--arch-kernels "x64") '("3.3" "3.6" "3.10" "7.3")))
    (should (equal (syscall--arch-kernels "arm64") '("3.6" "3.10" "7.3")))
    (should (equal (syscall-test--names (syscall-table "x64")) '("read" "newcall")))
    (should (equal (syscall-test--names (syscall-table "x64" "3.6")) '("read" "oldcall")))
    (should (equal (syscall-test--names (syscall-table "x64" "3.10")) '("read" "newcall")))
    (should (equal (syscall-test--names (syscall-table "x64" "all")) '("read" "oldcall" "newcall")))
    (should (null (syscall-table "arm64" "3.3")))
    (should (null (syscall-table "arm64" "3.6")))
    (should (equal (syscall-test--names (syscall-table "arm64")) '("read")))
    (should (eq (syscall-table "x64") (syscall-table "x64" "7.3")))))

(ert-deftest syscall-test-find ()
  (syscall-test--with-data
    (should (= 63 (alist-get 'nr (syscall-find "arm64" "read"))))
    (should-not (syscall-find "x64" "oldcall"))
    (should (syscall-find "x64" "oldcall" "all"))
    (should (equal "oldcall" (alist-get 'name (syscall-find-number "x64" 59 "3.6"))))
    (should-not (syscall-find-number "x64" 59))))

(ert-deftest syscall-test-find-prefers-current ()
  (let ((table '(((name . "setsockopt") (nr . 1073741878) (since . "3.4") (removed_in . "3.6"))
                 ((name . "setsockopt") (nr . 1073742365) (since . "3.6") (removed_in)))))
    (cl-letf (((symbol-function 'syscall-table) (lambda (&rest _) table)))
      (should (= 1073742365 (alist-get 'nr (syscall-find "x32" "setsockopt" "all"))))
      (should (= 1073741878 (alist-get 'nr (syscall-find-number "x32" 54 "all"))))
      (should-not (syscall-find-number "x64" 54 "all")))))

(ert-deftest syscall-test-parse-number ()
  (should (= 59 (syscall--parse-number "59")))
  (should (= 59 (syscall--parse-number " 0x3B ")))
  (should-not (syscall--parse-number "open"))
  (should-not (syscall--parse-number "0x")))

(ert-deftest syscall-test-filter ()
  (let ((read '((nr . 0) (name . "read") (description . "read from a file descriptor")
                (nargs . 3) (arg0 . "unsigned int fd")))
        (execve '((nr . 59) (name . "execve") (description . "execute program") (nargs . 0))))
    (should (funcall (syscall-filter-predicate "") read))
    (should (funcall (syscall-filter-predicate "59") execve))
    (should-not (funcall (syscall-filter-predicate "5") execve))
    (should (funcall (syscall-filter-predicate "0x3b") execve))
    (should-not (funcall (syscall-filter-predicate "0x3b") read))
    (should (funcall (syscall-filter-predicate "DESCRIPTOR") read))
    (should (funcall (syscall-filter-predicate "unsigned int") read))
    (should-not (funcall (syscall-filter-predicate "exec") read))
    (should (funcall (syscall-filter-predicate "3b") execve))
    (let ((epoll '((nr . 232) (name . "epoll_wait") (nargs . 0)
                   (family . "epoll") (kconfig . "CONFIG_EPOLL"))))
      (should (funcall (syscall-filter-predicate "config_epoll") epoll))
      (should-not (funcall (syscall-filter-predicate "config_epoll") read)))))

(ert-deftest syscall-test-strip-name ()
  (should (equal (syscall--strip-name "SYS_read") "read"))
  (should (equal (syscall--strip-name "__NR_read") "read"))
  (should (equal (syscall--strip-name "__NR3264_fcntl") "fcntl"))
  (should (equal (syscall--strip-name "__x64_sys_openat") "openat"))
  (should (equal (syscall--strip-name "sys_read") "read"))
  (should (equal (syscall--strip-name "read") "read")))

(ert-deftest syscall-test-signature ()
  (should (equal (syscall-signature '((name . "read") (nargs . 3) (arg0 . "unsigned int fd")
                                      (arg1 . "char *buf") (arg2 . "size_t count")))
                 "read(unsigned int fd, char *buf, size_t count)"))
  (should (equal (syscall-signature '((name . "getpid") (nargs . 0))) "getpid(void)"))
  (should (equal (syscall-signature '((name . "odd") (nargs . 2) (arg0 . "int a")))
                 "odd(int a, ?)"))
  (should (equal (syscall-signature '((name . "mystery"))) "mystery(?)")))

(ert-deftest syscall-test-since-label ()
  (syscall-test--with-data
    (should (string-match-p "3.3 or earlier" (syscall--since-label "x64" (syscall-find "x64" "read"))))
    (should (equal "Linux 3.10" (syscall--since-label "x64" (syscall-find "x64" "newcall"))))))

(ert-deftest syscall-test-diff ()
  (syscall-test--with-data
    (pcase-let ((`(,added . ,removed) (syscall-diff-entries "x64" "3.6" "7.3")))
      (should (equal (syscall-test--names added) '("newcall")))
      (should (equal (syscall-test--names removed) '("oldcall"))))
    (pcase-let ((`(,added . ,removed) (syscall-diff-entries "x64" "3.10" "7.3")))
      (should (null added))
      (should (null removed)))
    (syscall-diff "x64" "3.6" "7.3")
    (with-current-buffer "*syscall diff x64 3.6..7.3*"
      (should (string-match-p "Added (1)" (buffer-string)))
      (should (string-match-p "newcall" (buffer-string)))
      (kill-buffer))))

(ert-deftest syscall-test-asm-stub ()
  (syscall-test--with-data
    (should (equal (syscall-asm-stub "x64" (syscall-find "x64" "read"))
                   (concat "; read(unsigned int fd, char *buf, size_t count), x64 0\n"
                           "mov eax, 0x0  ; read\n"
                           "mov rdi, 0    ; unsigned int fd\n"
                           "mov rsi, 0    ; char *buf\n"
                           "mov rdx, 0    ; size_t count\n"
                           "syscall\n")))
    (should (equal (syscall-asm-stub "arm64" (syscall-find "arm64" "read"))
                   (concat "// read(?), arm64 63\n"
                           "mov x8, #0x3f  // read\n"
                           "svc #0\n")))
    (with-temp-buffer
      (setq-local comment-start "# ")
      (setq-local comment-end "")
      (insert "    ")
      (syscall-insert-asm-stub "x64" (syscall-find "x64" "newcall"))
      (should (equal (buffer-string)
                     "    # newcall(?), x64 60\n    mov eax, 0x3c  # newcall\n    syscall\n")))))

(ert-deftest syscall-test-arm-immediate ()
  (should (syscall--arm-immediate-p 0))
  (should (syscall--arm-immediate-p 255))
  (should (syscall--arm-immediate-p #x3fc))
  (should (syscall--arm-immediate-p #xf000000f))
  (should-not (syscall--arm-immediate-p 281))
  (should-not (syscall--arm-immediate-p 257))
  (let ((arm '((arch . "arm") (instruction . "svc #0") (nr . "r7") (return . "r0") (arg0 . "r0"))))
    (cl-letf (((symbol-function 'syscall-convention) (lambda (_) arm)))
      (should (equal (syscall-asm-stub "arm" '((name . "socket") (nr . 281) (nargs . 0)))
                     "@ socket(void), arm 281\nldr r7, =0x119  @ socket\nsvc #0\n"))
      (should (equal (syscall-asm-stub "arm" '((name . "write") (nr . 4) (nargs . 0)))
                     "@ write(void), arm 4\nmov r7, #0x4  @ write\nsvc #0\n")))))

(ert-deftest syscall-test-detail-buffer ()
  (syscall-test--with-data
    (syscall-lookup "x64" (syscall-find "x64" "read"))
    (with-current-buffer "*syscall*"
      (should (eq major-mode 'syscall-detail-mode))
      (let ((text (buffer-string)))
        (should (string-match-p "read(unsigned int fd, char \\*buf, size_t count)" text))
        (should (string-match-p "rdi +unsigned int fd" text))
        (should (string-match-p "clobbers rcx" text))
        (should (string-match-p "arm64 +63 +0x3f" text))
        (should (string-match-p "or earlier" text)))
      (syscall-detail-next)
      (should (equal "newcall" (alist-get 'name syscall--entry)))
      (should-error (syscall-detail-next) :type 'user-error)
      (should-error (syscall-detail-kernel "3.6") :type 'user-error)
      (syscall-detail-previous)
      (should (equal "read" (alist-get 'name syscall--entry)))
      (syscall-detail-kernel "3.6")
      (should (equal "3.6" syscall--kernel))
      (syscall-detail-next)
      (should (equal "oldcall" (alist-get 'name syscall--entry)))
      (syscall-detail-previous)
      (syscall-copy-hex-number)
      (should (equal "0x0" (current-kill 0)))
      (revert-buffer)
      (should (equal "read" (alist-get 'name syscall--entry)))
      (kill-buffer))))

(ert-deftest syscall-test-lookup-number ()
  (syscall-test--with-data
    (syscall-lookup-number "x64" 60)
    (with-current-buffer "*syscall*"
      (should (equal "newcall" (alist-get 'name syscall--entry)))
      (kill-buffer))
    (should-error (syscall-lookup-number "x64" 59) :type 'user-error)))

(ert-deftest syscall-test-list-buffer ()
  (syscall-test--with-data
    (syscall-list "x64")
    (with-current-buffer "*syscalls x64*"
      (should (eq major-mode 'syscall-list-mode))
      (should (= 2 (length tabulated-list-entries)))
      (syscall--list-apply-filter "0x3c")
      (should (equal '("newcall") (mapcar (lambda (e) (alist-get 'name (car e))) tabulated-list-entries)))
      (syscall--list-apply-filter "")
      (should-not syscall--filter)
      (syscall-list-kernel "all")
      (should (= 3 (length tabulated-list-entries)))
      (goto-char (point-min))
      (search-forward "oldcall")
      (should (eq (get-text-property (1- (point)) 'face) 'syscall-removed))
      (should (equal "oldcall" (alist-get 'name (tabulated-list-get-id))))
      (syscall-list-family "epoll")
      (should (equal '("oldcall") (mapcar (lambda (e) (alist-get 'name (car e))) tabulated-list-entries)))
      (should (string-match-p "family epoll" mode-line-process))
      (should (equal '("epoll") (syscall-families "x64")))
      (syscall-list-family "")
      (should (= 3 (length tabulated-list-entries)))
      (syscall-list-describe)
      (kill-buffer "*syscall*")
      (kill-buffer))))

(ert-deftest syscall-test-compare-and-conventions ()
  (syscall-test--with-data
    (syscall-compare "read")
    (with-current-buffer "*syscall compare read*"
      (should (equal '("x64" "arm64") (mapcar #'caar tabulated-list-entries)))
      (kill-buffer))
    (syscall-compare "newcall")
    (with-current-buffer "*syscall compare newcall*"
      (should (string-match-p "missing on arm64" (buffer-string)))
      (kill-buffer))
    (syscall-conventions-table)
    (with-current-buffer "*syscall conventions*"
      (should (equal '("x64" "arm64") (mapcar #'car tabulated-list-entries)))
      (should (string-match-p "svc #0" (buffer-string)))
      (kill-buffer))))

(ert-deftest syscall-test-eldoc ()
  (syscall-test--with-data
    (with-temp-buffer
      (insert "syscall(SYS_read, fd, buf, n); read(fd);")
      (goto-char (point-min))
      (search-forward "SYS_re")
      (should (string-match-p "\\`read(unsigned int fd.*x64 0 0x0\\'" (syscall-eldoc-function)))
      (search-forward "; re")
      (should-not (syscall-eldoc-function))
      (let ((syscall-eldoc-bare-names t))
        (should (syscall-eldoc-function))))))

(ert-deftest syscall-test-eldoc-silent-without-cache ()
  (syscall-test--with-cache
    (with-temp-buffer
      (insert "SYS_read")
      (goto-char (point-min))
      (should-not (syscall-eldoc-function))
      (should-not syscall--job))))

(provide 'syscall-test)
;;; syscall-test.el ends here
