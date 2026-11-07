;;; syscall.el --- Offline Linux syscall reference  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Guilherme Thomazi Bonicontro

;; Author: Guilherme Thomazi Bonicontro <thomazi@linux.com>
;; Maintainer: Guilherme Thomazi Bonicontro <thomazi@linux.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, c, languages
;; URL: https://github.com/guitmz/syscall.el
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Look up Linux syscalls and calling conventions for every architecture
;; https://api.syscall.sh covers, without a network connection.
;;
;; A dataset is bundled for offline use.  `syscall-update' refreshes it.
;; Each architecture file holds every syscall it ever had, with `since'
;; and `removed_in', so any scanned kernel release works offline.
;;
;; `syscall-update' checks the dataset hash before downloading.  It
;; verifies fresh data before replacing the cached copy.
;;
;; Bind `syscall-command-map' to a key and start with `syscall-lookup'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'seq)
(require 'thingatpt)
(require 'tabulated-list)
(require 'button)
(require 'eldoc)

(defvar syscall-data--json)
(defvar syscall-data--sha256)

(defgroup syscall nil
  "Offline Linux syscall reference."
  :group 'tools
  :prefix "syscall-")

(defcustom syscall-api-url "https://api.syscall.sh/v2"
  "Base URL of the syscall API, without a trailing slash."
  :type 'string)

(defcustom syscall-cache-directory (locate-user-emacs-file "syscall/")
  "Directory holding the downloaded syscall data."
  :type 'directory)

(defcustom syscall-auto-update-interval nil
  "Days after which cached data is refreshed in the background.
The check runs at most once per session, the first time data is
needed.  nil disables it; `syscall-update' still works."
  :type '(choice (const :tag "Never" nil) (natnum :tag "Days")))

(defcustom syscall-request-delay 0.25
  "Minimum seconds between two API requests, to stay under its rate limit."
  :type 'number)

(defcustom syscall-max-attempts 5
  "Attempts per request when the API answers 429, 5xx or the network fails."
  :type 'natnum)

(defcustom syscall-retry-delay 2
  "Seconds to wait before a retry, multiplied by the attempt number."
  :type 'number)

(defcustom syscall-request-timeout 30
  "Seconds after which a single request is abandoned and retried."
  :type 'number)

(defconst syscall--schema 1
  "Version of the cache layout.  A cache with another version is refetched.")

(defconst syscall--user-agent "syscall.el"
  "User-Agent sent to the API.")

;;;; Cache files

(defun syscall--path (file)
  "Absolute path of FILE inside `syscall-cache-directory'."
  (expand-file-name file syscall-cache-directory))

(defun syscall--arch-file (arch)
  "Cache file, relative to the cache directory, of ARCH's syscalls."
  (format "arch/%s.json" arch))

(defun syscall--read-meta ()
  "Return the cache metadata plist, or nil when missing or unusable."
  (let ((file (syscall--path "meta.eld")))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((meta (read (current-buffer))))
              (and (plistp meta)
                   (eql (plist-get meta :schema) syscall--schema)
                   meta)))
        (error nil)))))

(defun syscall--write-meta (meta)
  "Write the metadata plist META to the cache directory."
  (make-directory syscall-cache-directory t)
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file (syscall--path "meta.eld")
      (insert ";;; syscall.el cache metadata. Generated, do not edit.\n")
      (prin1 meta (current-buffer))
      (insert "\n"))))

(defun syscall--parse-json (string)
  "Parse the JSON in STRING.  Objects become alists, arrays lists, null nil."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun syscall--read-json-file (file)
  "Parse the JSON file FILE, an absolute path."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (syscall--parse-json (buffer-string))))

(defun syscall--cache-complete-p (&optional meta)
  "Non-nil when the cache described by META holds every file it needs.
META defaults to the metadata on disk."
  (let ((meta (or meta (syscall--read-meta))))
    (and meta
         (not (plist-get meta :use-bundled))
         (plist-get meta :arches)
         (or (and (plist-get meta :dataset-sha256)
                  (file-exists-p (syscall--path "dataset.json")))
             (and (file-exists-p (syscall--path "kernels.json"))
         (file-exists-p (syscall--path "conventions.json"))
         (cl-every (lambda (arch) (file-exists-p (syscall--path (syscall--arch-file arch))))
                   (plist-get meta :arches)))))))

;;;; HTTP

(defvar-local syscall--own-request nil
  "Non-nil in url.el buffers of requests made by this package.")
(put 'syscall--own-request 'permanent-local t)

(defun syscall--url-cache-extract (orig file)
  "Call ORIG with FILE, except for this package's requests.
url.el answers a 304 by replacing the response with its own disk
cache entry, which does not exist since this package keeps its own
cache.  Skipping it leaves the 304 response for us to handle."
  (unless syscall--own-request
    (funcall orig file)))

(advice-add 'url-cache-extract :around #'syscall--url-cache-extract)

(defun syscall-unload-function ()
  "Remove the advice when the package is unloaded."
  (advice-remove 'url-cache-extract #'syscall--url-cache-extract)
  nil)

(defun syscall--kill-url-buffer (buffer)
  "Kill BUFFER and stop its network process, if any."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (when proc
        (set-process-sentinel proc #'ignore)
        (delete-process proc)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun syscall--http-get (url etag callback)
  "GET URL, sending ETAG as If-None-Match when non-nil.
CALLBACK is called once with (CODE ETAG BODY ERROR): the HTTP status
or nil when no response arrived, the response ETag, the raw body as
a unibyte string, and an error message or nil.  Return the request
buffer, or nil when the request could not be started."
  (let* ((done nil)
         (buffer nil)
         (timer nil)
         (finish (lambda (&rest args)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (apply callback args))))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Accept" . "application/json")
            ,@(when etag `(("If-None-Match" . ,etag)))))
         (url-user-agent syscall--user-agent)
         (url-privacy-level 'none))
    (condition-case err
        (setq buffer
              (url-retrieve url
                            (lambda (status)
                              (let ((response (syscall--parse-response status)))
                                (syscall--kill-url-buffer (current-buffer))
                                (apply finish response)))
                            nil t t))
      (error (funcall finish nil nil nil (error-message-string err))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq syscall--own-request t)))
    (when (and buffer (not done))
      (setq timer (run-at-time syscall-request-timeout nil
                               (lambda ()
                                 (syscall--kill-url-buffer buffer)
                                 (funcall finish nil nil nil "timed out")))))
    buffer))

(defun syscall--parse-response (status)
  "Return (CODE ETAG BODY ERROR) for the url.el response in this buffer.
STATUS is the plist `url-retrieve' passes to its callback."
  (let ((code (bound-and-true-p url-http-response-status))
        (end (and (boundp 'url-http-end-of-headers)
                  (markerp url-http-end-of-headers)
                  (marker-position url-http-end-of-headers)))
        etag body)
    (when end
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward "^etag:[ \t]*\\([^\r\n]*?\\)[ \t]*\r?$" end t)
            (setq etag (match-string-no-properties 1))))
        (goto-char end)
        (skip-chars-forward "\r\n")
        (setq body (buffer-substring-no-properties (point) (point-max)))))
    (list code etag body
          (cond ((not code)
                 (let ((err (plist-get status :error)))
                   (if err (format "%S" err) "no response")))
                ((not end) "malformed response")))))

;;;; Update

(cl-defstruct (syscall--job (:constructor syscall--job-create))
  queue            ; resources still to request
  (staged nil)     ; cache-relative files written to the staging directory
  (old-etags nil)  ; alist of file -> ETag from the previous update
  (etags nil)      ; alist of file -> ETag for the new cache
  (arches nil)
  (fetched 0)
  (unchanged 0)
  (last-request 0.0)
  dataset-sha256 dataset-size
  force quiet callback timer buffer cancelled)

(defvar syscall--job nil
  "The running update, or nil.")

(defvar syscall--auto-update-tried nil
  "Non-nil once this session started an automatic update.")

(defun syscall--staging-dir ()
  "Directory where a running update writes its downloads."
  (syscall--path ".staging/"))

(defun syscall--resource (kind endpoint file)
  "A request of KIND for ENDPOINT, saved to the cache-relative FILE."
  (list :kind kind :endpoint endpoint :file file))

(defun syscall--valid-p (kind data)
  "Non-nil when DATA looks like a well formed response of KIND."
  (and (consp data)
       (cl-every #'consp data)
       (pcase kind
         ('kernels (cl-every (lambda (k) (stringp (alist-get 'version k))) data))
         ('conventions (cl-every (lambda (c) (stringp (alist-get 'arch c))) data))
         ('arch (cl-every (lambda (s) (and (stringp (alist-get 'name s))
                                           (integerp (alist-get 'nr s))
                                           (stringp (alist-get 'since s))))
                          data)))))

(defun syscall--kernel-arches (kernels)
  "Sorted union of the architectures in the parsed KERNELS list."
  (let (arches)
    (dolist (k kernels)
      (dolist (a (alist-get 'arches k))
        (cl-pushnew a arches :test #'equal)))
    (sort arches #'string<)))

(defun syscall--job-message (job format-string &rest args)
  "Show FORMAT-STRING with ARGS unless JOB is quiet."
  (unless (syscall--job-quiet job)
    (let ((message-log-max nil))
      (apply #'message (concat "syscall: " format-string) args))))

(defun syscall--job-schedule (job seconds fn)
  "Run FN after SECONDS as JOB's next step."
  (setf (syscall--job-timer job)
        (run-at-time seconds nil
                     (lambda ()
                       (unless (syscall--job-cancelled job)
                         (funcall fn))))))

(defun syscall--job-next (job)
  "Request JOB's next resource, or commit when none is left."
  (if-let* ((res (pop (syscall--job-queue job))))
      (let ((wait (max 0 (- (+ (syscall--job-last-request job) syscall-request-delay)
                            (float-time)))))
        (syscall--job-schedule job wait (lambda () (syscall--job-request job res 1))))
    (syscall--job-commit job)))

(defun syscall--job-request (job res attempt)
  "Request resource RES for JOB, ATTEMPT being 1 for the first try."
  (setf (syscall--job-last-request job) (float-time))
  (let* ((file (plist-get res :file))
         (etag (and (not (syscall--job-force job))
                    (file-exists-p (syscall--path file))
                    (cdr (assoc file (syscall--job-old-etags job)))))
         (url (concat (string-trim-right syscall-api-url "/+") (plist-get res :endpoint))))
    (syscall--job-message job "fetching %s%s..." file
                          (if (> attempt 1) (format " (attempt %d)" attempt) ""))
    (setf (syscall--job-buffer job)
          (syscall--http-get
           url etag
           (lambda (code new-etag body err)
             (unless (syscall--job-cancelled job)
               (setf (syscall--job-buffer job) nil)
               (condition-case e
                   (syscall--job-response job res attempt url etag code new-etag body err)
                 (error (syscall--job-fail
                         job (format "%s: %s" url (error-message-string e)))))))))))

(cl-defun syscall--job-response (job res attempt url etag code new-etag body err)
  "Handle the answer to RES for JOB.
ATTEMPT, URL and ETAG describe the request; CODE, NEW-ETAG, BODY and
ERR the response, as passed by `syscall--http-get'."
  (let ((kind (plist-get res :kind))
        (file (plist-get res :file)))
    (cond
     ((eql code 200)
      (let ((data (condition-case nil
                      (syscall--parse-json (decode-coding-string body 'utf-8))
                    (error nil))))
        (unless (or (memq kind '(manifest dataset)) (syscall--valid-p kind data))
          (error "Response is empty or not in the expected format"))
        (when (eq kind 'manifest)
          (let* ((info (alist-get 'json data))
                 (sha (alist-get 'sha256 info))
                 (size (alist-get 'size_bytes info))
                 (meta (syscall--read-meta))
                 (cached-sha (and (syscall--cache-complete-p meta)
                                  (equal (plist-get meta :source) syscall-api-url)
                                  (plist-get meta :dataset-sha256)))
                 (bundled-sha (when (and (not cached-sha)
                                         (equal syscall-api-url "https://api.syscall.sh/v2")
                                         (syscall--bundled-available-p))
                                (require 'syscall-data)
                                syscall-data--sha256))
                 (local (or cached-sha bundled-sha)))
            (unless (and (stringp sha) (string-match-p "\\`[a-f0-9]\\{64\\}\\'" sha)
                         (integerp size) (> size 0))
              (error "Invalid dataset manifest"))
            (setf (syscall--job-dataset-sha256 job) sha
                  (syscall--job-dataset-size job) size)
            (if (and (not (syscall--job-force job)) (equal sha local))
                (progn
                  (when meta
                    (when (and bundled-sha (syscall--cache-complete-p meta))
                      (setq meta (plist-put meta :use-bundled t))
                      (syscall--reset))
                    (syscall--write-meta (plist-put meta :fetched-at (float-time))))
                  (syscall--job-message job "data is up to date")
                  (syscall--job-finish job t 0))
              (setf (syscall--job-queue job)
                    (list (syscall--resource 'dataset "/dataset/syscalls.json" "dataset.json")))
              (syscall--job-next job)))
          (cl-return-from syscall--job-response nil))
        (when (eq kind 'dataset)
          (unless (and (= (length body) (syscall--job-dataset-size job))
                       (equal (secure-hash 'sha256 body) (syscall--job-dataset-sha256 job)))
            (error "Dataset size or checksum does not match manifest"))
          (setf (syscall--job-arches job)
                (syscall--kernel-arches (alist-get 'kernels (syscall--convert-dataset data)))))
        (let ((dest (expand-file-name file (syscall--staging-dir)))
              (coding-system-for-write 'no-conversion))
          (make-directory (file-name-directory dest) t)
          (write-region body nil dest nil 'silent))
        (push file (syscall--job-staged job))
        (when new-etag (push (cons file new-etag) (syscall--job-etags job)))
        (cl-incf (syscall--job-fetched job))
        (syscall--job-after job kind data)))
     ((and (eql code 304) etag)
      (push (cons file (or new-etag etag)) (syscall--job-etags job))
      (cl-incf (syscall--job-unchanged job))
      (syscall--job-after job kind
                          (and (eq kind 'kernels)
                               (syscall--read-json-file (syscall--path file)))))
     ((and (< attempt syscall-max-attempts)
           (or (null code) (eql code 429) (>= code 500)))
      (syscall--job-schedule job (* syscall-retry-delay attempt)
                             (lambda () (syscall--job-request job res (1+ attempt)))))
     (t
      (syscall--job-fail
       job (format "%s: %s" url
                   (or err
                       (format "HTTP %s%s" code
                               (if (and body (> (length body) 0))
                                   (concat " " (string-trim (substring body 0 (min 200 (length body)))))
                                 "")))))))))

(defun syscall--job-after (job kind data)
  "Continue JOB after a resource of KIND succeeded with parsed DATA."
  (when (eq kind 'kernels)
    (let ((arches (syscall--kernel-arches data)))
      (unless arches
        (error "The kernel list names no architectures"))
      (setf (syscall--job-arches job) arches)
      (setf (syscall--job-queue job)
            (append (syscall--job-queue job)
                    (mapcar (lambda (arch)
                              (syscall--resource
                               'arch
                               (format "/syscalls/%s?kernel=all" (url-hexify-string arch))
                               (syscall--arch-file arch)))
                            arches)))))
  (syscall--job-next job))

(defun syscall--job-finish (job ok result)
  "End JOB; OK and RESULT are passed to its callback."
  (setf (syscall--job-cancelled job) t)
  (when (syscall--job-timer job) (cancel-timer (syscall--job-timer job)))
  (syscall--kill-url-buffer (syscall--job-buffer job))
  (when (file-directory-p (syscall--staging-dir))
    (delete-directory (syscall--staging-dir) t))
  (when (eq syscall--job job) (setq syscall--job nil))
  (when (syscall--job-callback job)
    (funcall (syscall--job-callback job) ok result)))

(defun syscall--job-fail (job reason)
  "Abort JOB because of REASON, keeping the previous cache."
  (unless (syscall--job-quiet job)
    (message "syscall: update failed, cache left unchanged: %s" reason))
  (syscall--job-finish job nil reason))

(defun syscall--job-commit (job)
  "Move JOB's downloads into the cache and record the new metadata."
  (condition-case e
      (progn
        (dolist (file (syscall--job-staged job))
          (let ((dest (syscall--path file)))
            (make-directory (file-name-directory dest) t)
            (rename-file (expand-file-name file (syscall--staging-dir)) dest t)))
        (syscall--write-meta (list :schema syscall--schema
                                   :fetched-at (float-time)
                                   :source syscall-api-url
                                   :dataset-sha256 (syscall--job-dataset-sha256 job)
                                   :arches (syscall--job-arches job)
                                   :etags (syscall--job-etags job)))
        (syscall--reset)
        (unless (syscall--job-quiet job)
          (message "syscall: %d architectures, %d files updated, %d unchanged"
                   (length (syscall--job-arches job))
                   (syscall--job-fetched job) (syscall--job-unchanged job)))
        (syscall--job-finish job t (syscall--job-fetched job)))
    (error (syscall--job-fail job (format "writing the cache: %s" (error-message-string e))))))

(defun syscall--start-update (&optional force quiet callback)
  "Start an update and return its job.
FORCE ignores stored ETags.  QUIET suppresses messages.  CALLBACK is
called with (OK RESULT) when the update ends: RESULT is the number of
files downloaded on success, the failure reason otherwise."
  (when syscall--job
    (user-error "A syscall update is already running"))
  (when (file-directory-p (syscall--staging-dir))
    (delete-directory (syscall--staging-dir) t))
  (make-directory (syscall--staging-dir) t)
  (let* ((meta (syscall--read-meta))
         (job (syscall--job-create
               :queue (list (syscall--resource 'manifest "/dataset" "manifest.json"))
               :old-etags (plist-get meta :etags)
               :force force :quiet quiet :callback callback)))
    (setq syscall--job job)
    (syscall--job-next job)
    job))

(defun syscall--update-sync (&optional force)
  "Update the cache and wait for it.  FORCE ignores stored ETags.
Return non-nil on success.  \\[keyboard-quit] cancels the update."
  (let* ((done nil) (ok nil)
         (job (syscall--start-update force nil
                                     (lambda (success _) (setq done t ok success)))))
    (condition-case nil
        (while (not done)
          (accept-process-output nil 0.05))
      (quit (syscall--job-fail job "cancelled")
            (signal 'quit nil)))
    ok))

;;;###autoload
(defun syscall-update (&optional force)
  "Refresh the syscall cache from `syscall-api-url' in the background.
With prefix argument FORCE, download the dataset even if it has not changed."
  (interactive "P")
  (syscall--start-update force)
  (message "syscall: update started"))

;;;###autoload
(defun syscall-cancel-update ()
  "Cancel the running update, leaving the previous cache in place."
  (interactive)
  (if syscall--job
      (syscall--job-fail syscall--job "cancelled")
    (message "syscall: no update running")))

;;;###autoload
(defun syscall-clear-cache ()
  "Delete every downloaded syscall file."
  (interactive)
  (when syscall--job (syscall--job-fail syscall--job "cache cleared"))
  (when (and (file-directory-p syscall-cache-directory)
             (yes-or-no-p (format "Delete %s? " syscall-cache-directory)))
    (delete-directory syscall-cache-directory t)
    (syscall--reset)
    (message "syscall: cache deleted")))

;;;###autoload
(defun syscall-cache-info ()
  "Describe the cached data."
  (interactive)
  (let ((meta (syscall--read-meta)))
    (if (not (syscall--cache-complete-p meta))
        (if (syscall--bundled-available-p)
            (message "syscall: using bundled data")
          (message "syscall: no complete cache in %s, run M-x syscall-update"
                   syscall-cache-directory))
      (let ((kernels (syscall-kernels)))
        (message "syscall: %d architectures, kernels %s to %s, checked %s (%s)"
                 (length (plist-get meta :arches))
                 (alist-get 'version (car kernels))
                 (alist-get 'version (car (last kernels)))
                 (format-time-string "%F %R" (plist-get meta :fetched-at))
                 (plist-get meta :source))))))

(defun syscall--maybe-auto-update (meta)
  "Start a quiet background update when META is older than the interval."
  (when (and syscall-auto-update-interval
             (not syscall--auto-update-tried)
             (not syscall--job)
             (> (- (float-time) (or (plist-get meta :fetched-at) 0))
                (* syscall-auto-update-interval 86400)))
    (setq syscall--auto-update-tried t)
    (syscall--start-update nil t nil)))

(defun syscall--bundled-data ()
  "Return the bundled dataset, parsed once."
  (syscall--memo 'bundled
                 (lambda ()
                   (when (require 'syscall-data nil t)
                     (syscall--parse-json
                      (decode-coding-string
                       (base64-decode-string syscall-data--json) 'utf-8))))))

(defun syscall--convert-dataset (data)
  "Convert and validate the full API DATA for offline lookup."
  (let ((kernel-rows (alist-get 'kernels data))
        (conventions (alist-get 'conventions data))
        (rows (alist-get 'syscalls data))
        (versions (make-hash-table :test #'equal))
        (tables (make-hash-table :test #'equal)) kernels syscalls)
    (unless (and (consp kernel-rows) (syscall--valid-p 'conventions conventions)
                 (syscall--valid-p 'arch rows))
      (error "Invalid dataset"))
    (dolist (row kernel-rows)
      (let ((version (alist-get 'version row)) (arch (alist-get 'arch row)))
        (unless (and (stringp version) (stringp arch)) (error "Invalid kernel"))
        (unless (gethash version versions) (push version kernels))
        (puthash version (cons arch (gethash version versions)) versions)))
    (dolist (row rows)
      (let ((arch (alist-get 'arch row)))
        (unless (stringp arch) (error "Invalid architecture"))
        (puthash arch (cons row (gethash arch tables)) tables)))
    (setq kernels
          (mapcar (lambda (version)
                    (cons (cons 'version version)
                          (list (cons 'arches (nreverse (gethash version versions))))))
                  (nreverse kernels)))
    (dolist (arch (syscall--kernel-arches kernels))
      (unless (gethash arch tables) (error "Missing architecture: %s" arch)))
    (maphash (lambda (arch table) (push (cons (intern arch) (nreverse table)) syscalls)) tables)
    `((kernels . ,kernels) (conventions . ,conventions) (syscalls . ,syscalls))))

(defun syscall--cached-dataset ()
  "Return the converted full dataset from the cache."
  (syscall--memo 'dataset
                 (lambda () (syscall--convert-dataset
                             (syscall--read-json-file (syscall--path "dataset.json"))))))

(defun syscall--bundled-available-p ()
  "Return non-nil when the bundled dataset is available."
  (or (featurep 'syscall-data) (locate-library "syscall-data")))

(defun syscall-ensure-data ()
  "Make sure the cache is usable, offering to download it when missing.
Signal a `user-error' when there is no data to work with."
  (let ((meta (syscall--read-meta)))
    (cond
     ((syscall--cache-complete-p meta)
      (syscall--maybe-auto-update meta))
     ((syscall--bundled-available-p) t)
     ((and (not noninteractive)
           (y-or-n-p "Download the syscall data from api.syscall.sh (about 6MB)? "))
      (unless (syscall--update-sync)
        (user-error "Could not download the syscall data, see *Messages*")))
     (t (user-error "No syscall data yet, run M-x syscall-update")))
    t))

;;;; Data access

(defvar syscall--data (make-hash-table :test #'equal)
  "Parsed cache files and derived tables, keyed by file name or list.")

(defvar syscall--ready nil
  "Non-nil once the cache was found complete since the last reset.")

(defun syscall--reset ()
  "Forget the parsed data, so it is read again from the cache."
  (clrhash syscall--data)
  (setq syscall--ready nil))

(defun syscall--ready-p ()
  "Non-nil when the cache can be used without downloading anything."
  (or syscall--ready
      (setq syscall--ready (and (or (syscall--cache-complete-p)
                                   (syscall--bundled-available-p)) t))))

(defun syscall--memo (key fn)
  "Value cached under KEY, computed by calling FN the first time."
  (let ((value (gethash key syscall--data 'syscall--none)))
    (if (eq value 'syscall--none)
        (puthash key (funcall fn) syscall--data)
      value)))

(defun syscall--load (file)
  "Parsed contents of the cache-relative FILE, read once."
  (syscall--memo
   file (lambda ()
          (if (and (syscall--cache-complete-p)
                   (not (plist-get (syscall--read-meta) :dataset-sha256)))
              (syscall--read-json-file (syscall--path file))
            (let ((data (if (syscall--cache-complete-p)
                            (syscall--cached-dataset)
                          (syscall--bundled-data))))
              (cond
               ((equal file "kernels.json") (alist-get 'kernels data))
               ((equal file "conventions.json") (alist-get 'conventions data))
               ((string-match "\\`arch/\\(.+\\)\\.json\\'" file)
                (alist-get (intern (match-string 1 file))
                           (alist-get 'syscalls data)))))))))

(defun syscall-arches ()
  "Architectures in the cache."
  (syscall--memo 'arches (lambda ()
                          (if (syscall--cache-complete-p)
                              (plist-get (syscall--read-meta) :arches)
                            (syscall--kernel-arches (syscall-kernels))))))

(defun syscall-kernels ()
  "Scanned kernel releases, oldest first, as alists of version and arches."
  (syscall--load "kernels.json"))

(defun syscall-conventions ()
  "Calling conventions of every architecture, as alists."
  (syscall--load "conventions.json"))

(defun syscall-arch-syscalls (arch)
  "Every syscall ARCH ever had, sorted by number.
Each is an alist with `since' and `removed_in', among other fields."
  (syscall--memo (list 'all arch)
                 (lambda ()
                   (sort (copy-sequence (syscall--load (syscall--arch-file arch)))
                         (lambda (a b) (< (alist-get 'nr a) (alist-get 'nr b)))))))

;;;; Architectures and kernels

(defcustom syscall-default-arch "x64"
  "Architecture used when a command does not ask for one.
Commands ask for another one when given a prefix argument."
  :type 'string)

(defcustom syscall-default-kernel nil
  "Kernel release shown by default.
nil shows the latest release scanned for each architecture, \"all\"
every syscall the architecture ever had, and a version such as
\"5.10\" that release."
  :type '(choice (const :tag "Latest" nil)
                 (const :tag "All releases" "all")
                 (string :tag "Release")))

(defconst syscall-arch-info
  '(("x64" "x86_64" 1)
    ("x86" "i386, 32 bit x86" 2)
    ("arm64" "AArch64" 3)
    ("arm" "ARM EABI, 32 bit" 4)
    ("riscv64" "RISC-V, 64 bit" 5)
    ("x32" "x32 ABI on x86_64" 6)
    ("ia32" "IA-32 compat on x86_64" 6
     "This compat ABI is only available when the kernel is built with CONFIG_IA32_EMULATION.")
    ("loongarch64" "LoongArch, 64 bit" 7)
    ("ppc64" "PowerPC, 64 bit" 8)
    ("ppc" "PowerPC, 32 bit" 9)
    ("s390x" "IBM Z" 10)
    ("mips64" "MIPS n64 ABI" 11)
    ("mipsn32" "MIPS n32 ABI" 12)
    ("mipso32" "MIPS o32 ABI" 13)
    ("alpha" "DEC Alpha" 100)
    ("arc" "Synopsys ARC" 100)
    ("csky" "C-SKY" 100)
    ("hexagon" "Qualcomm Hexagon" 100)
    ("m68k" "Motorola 68000" 100)
    ("microblaze" "Xilinx MicroBlaze" 100)
    ("nios2" "Intel Nios II" 100)
    ("openrisc" "OpenRISC" 100)
    ("parisc" "HP PA-RISC" 100)
    ("sh" "SuperH" 100)
    ("sparc" "SPARC, 32 bit" 100)
    ("sparc64" "SPARC, 64 bit" 100)
    ("xtensa" "Tensilica Xtensa" 100))
  "How architectures are shown, as in syscall.sh.
Each element is (ARCH TITLE RANK . NOTES).  Architectures sort by
RANK, then by name.  NOTES are shown when the API has none for ARCH.
Architectures missing here show their own name and sort last.")

(defun syscall--arch-title (arch)
  "Display name of ARCH."
  (or (nth 1 (assoc arch syscall-arch-info)) arch))

(defun syscall--sorted-arches ()
  "Cached architectures in display order."
  (let ((rank (lambda (a) (or (nth 2 (assoc a syscall-arch-info)) 100))))
    (sort (copy-sequence (syscall-arches))
          (lambda (a b)
            (let ((ra (funcall rank a)) (rb (funcall rank b)))
              (if (= ra rb) (string< a b) (< ra rb)))))))

(defun syscall--version (version)
  "VERSION such as \"6.10\" as a number that sorts correctly, 6010."
  (if (string-match "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\'" version)
      (+ (* 1000 (string-to-number (match-string 1 version)))
         (string-to-number (match-string 2 version)))
    (error "Bad kernel version %S" version)))

(defun syscall-exists-in-p (syscall version)
  "Non-nil when SYSCALL is present in the release VERSION, a number.
This is the rule syscall.sh uses: since <= VERSION < removed_in."
  (and (<= (syscall--version (alist-get 'since syscall)) version)
       (let ((removed (alist-get 'removed_in syscall)))
         (or (null removed) (> (syscall--version removed) version)))))

(defun syscall--arch-kernels (arch)
  "Releases scanned for ARCH, oldest first."
  (syscall--memo (list 'kernels arch)
                 (lambda ()
                   (cl-loop for k in (syscall-kernels)
                            when (member arch (alist-get 'arches k))
                            collect (alist-get 'version k)))))

(defun syscall--resolve-kernel (arch kernel)
  "The release KERNEL stands for on ARCH: a version, or \"all\"."
  (cond ((equal kernel "all") "all")
        (kernel)
        (t (car (last (syscall--arch-kernels arch))))))

(defun syscall-table (arch &optional kernel)
  "Syscalls of ARCH in KERNEL, sorted by number.
KERNEL is as in `syscall-default-kernel'.  The result is nil when
ARCH was not scanned for that release."
  (let ((k (syscall--resolve-kernel arch kernel)))
    (syscall--memo (list 'table arch k)
                   (lambda ()
                     (cond ((equal k "all") (syscall-arch-syscalls arch))
                           ((not (member k (syscall--arch-kernels arch))) nil)
                           (t (let ((v (syscall--version k)))
                                (cl-remove-if-not (lambda (sc) (syscall-exists-in-p sc v))
                                                  (syscall-arch-syscalls arch)))))))))

(defun syscall-find (arch name &optional kernel)
  "The syscall called NAME on ARCH in KERNEL, or nil.
With KERNEL \"all\" a name can have several numbers over time, and the
current one wins."
  (let ((matches (cl-remove-if-not (lambda (sc) (equal (alist-get 'name sc) name))
                                   (syscall-table arch kernel))))
    (or (cl-find-if-not (lambda (sc) (alist-get 'removed_in sc)) matches)
        (car (last matches)))))

(defun syscall-find-number (arch nr &optional kernel)
  "The syscall number NR on ARCH in KERNEL, or nil.
On x32 a number without the x32 bit (0x40000000) is also accepted."
  (let* ((table (syscall-table arch kernel))
         (find (lambda (n)
                 (let ((matches (cl-remove-if-not (lambda (sc) (= (alist-get 'nr sc) n)) table)))
                   (or (cl-find-if-not (lambda (sc) (alist-get 'removed_in sc)) matches)
                       (car (last matches)))))))
    (or (funcall find nr)
        (and (equal arch "x32") (< nr #x40000000)
             (funcall find (logior nr #x40000000))))))

(defun syscall-convention (arch)
  "Calling convention of ARCH, an alist, or nil."
  (cl-find arch (syscall-conventions) :key (lambda (c) (alist-get 'arch c)) :test #'equal))

(defun syscall--notes (arch)
  "ABI notes for ARCH."
  (or (alist-get 'notes (syscall-convention arch))
      (nthcdr 3 (assoc arch syscall-arch-info))))

;;;; Formatting

(defgroup syscall-faces nil
  "Faces used by syscall.el."
  :group 'syscall)

(defface syscall-title '((t :inherit (bold font-lock-function-name-face) :height 1.2))
  "Syscall name at the top of a detail buffer.")

(defface syscall-heading '((t :inherit bold))
  "Section headings.")

(defface syscall-name '((t :inherit font-lock-function-name-face))
  "Syscall names in tables.")

(defface syscall-number '((t :inherit font-lock-constant-face))
  "Syscall numbers.")

(defface syscall-register '((t :inherit font-lock-variable-name-face))
  "Register names.")

(defface syscall-dim '((t :inherit shadow))
  "Secondary text.")

(defface syscall-removed '((t :inherit shadow :strike-through t))
  "Syscalls no longer present in the latest release.")

(defun syscall--hex (nr)
  "NR in hexadecimal, as 0x3b."
  (format "0x%x" nr))

(defun syscall--parse-number (string)
  "The number written in STRING, decimal or 0x hexadecimal, or nil."
  (let ((s (downcase (string-trim string))))
    (cond ((string-match-p "\\`0x[0-9a-f]+\\'" s) (string-to-number (substring s 2) 16))
          ((string-match-p "\\`[0-9]+\\'" s) (string-to-number s)))))

(defun syscall--args (syscall)
  "Argument declarations of SYSCALL.  A nil element is unknown."
  (let ((args (mapcar (lambda (i) (alist-get (intern (format "arg%d" i)) syscall))
                      (number-sequence 0 5)))
        (n (alist-get 'nargs syscall)))
    (if n
        (cl-subseq args 0 (min n 6))
      (seq-take-while #'identity args))))

(defun syscall-signature (syscall)
  "C style signature of SYSCALL, such as read(unsigned int fd, ...)."
  (let ((args (syscall--args syscall)))
    (format "%s(%s)" (alist-get 'name syscall)
            (cond (args (mapconcat (lambda (a) (or a "?")) args ", "))
                  ((eql (alist-get 'nargs syscall) 0) "void")
                  (t "?")))))

(defun syscall--kernel-label (arch kernel)
  "Describe what KERNEL shows for ARCH."
  (cond ((equal kernel "all") "all releases")
        (kernel (format "Linux %s" kernel))
        (t (format "Linux %s, latest" (syscall--resolve-kernel arch nil)))))

(defun syscall--since-label (arch syscall)
  "When SYSCALL appeared on ARCH.
The first release scanned for an architecture is reported as \"or
earlier\", since the syscall may be much older."
  (let ((since (alist-get 'since syscall)))
    (if (equal since (car (syscall--arch-kernels arch)))
        (format "Linux %s or earlier (%s is the first release scanned for %s)" since since arch)
      (format "Linux %s" since))))

(defun syscall--pad (string width)
  "STRING followed by spaces up to WIDTH columns."
  (concat string (make-string (max 1 (- width (string-width string))) ?\s)))

;;;; Reading from the minibuffer

(defvar syscall--history nil "Minibuffer history of syscall names.")
(defvar syscall--arch-history nil "Minibuffer history of architectures.")
(defvar syscall--kernel-history nil "Minibuffer history of kernel releases.")

(defun syscall--completion-table (candidates annotate category)
  "Completion table over CANDIDATES, kept in order.
ANNOTATE is the annotation function, CATEGORY the completion category."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (annotation-function . ,annotate)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun syscall--read-arch (&optional prompt default)
  "Read an architecture with PROMPT, defaulting to DEFAULT."
  (let ((default (or default syscall-default-arch)))
    (completing-read
     (format-prompt (or prompt "Architecture") default)
     (syscall--completion-table
      (syscall--sorted-arches)
      (lambda (a) (concat (make-string (max 1 (- 14 (length a))) ?\s)
                          (propertize (syscall--arch-title a) 'face 'completions-annotations)))
      'syscall-arch)
     nil t nil 'syscall--arch-history default)))

(defun syscall--read-kernel (arch &optional prompt versions-only default)
  "Read a release of ARCH with PROMPT.  nil stands for the latest.
VERSIONS-ONLY leaves out \"latest\" and \"all\".  DEFAULT is the
initial choice."
  (let* ((versions (reverse (syscall--arch-kernels arch)))
         (choices (if versions-only versions (append '("latest" "all") versions)))
         (default (or default (car choices)))
         (answer (completing-read (format-prompt (or prompt "Kernel") default)
                                  (syscall--completion-table choices nil 'syscall-kernel)
                                  nil t nil 'syscall--kernel-history default)))
    (unless (equal answer "latest") answer)))

(defun syscall--strip-name (symbol)
  "Syscall name in SYMBOL, without prefixes such as SYS_ or __NR_."
  (let ((case-fold-search nil))
    (if (string-match "\\`\\(?:SYS_\\|__NR3264_\\|__NR_\\|__[a-z0-9]+_sys_\\|compat_sys_\\|sys_\\)\\(.+\\)\\'"
                      symbol)
        (match-string 1 symbol)
      symbol)))

(defun syscall--name-at-point (arch kernel &optional prefixed-only)
  "Name of a syscall ARCH has in KERNEL at point, or nil.
With PREFIXED-ONLY, only symbols such as SYS_read or __NR_read count."
  (when-let* ((symbol (thing-at-point 'symbol t))
              (name (syscall--strip-name symbol)))
    (and (or (not prefixed-only) (not (equal name symbol)))
         (syscall-find arch name kernel)
         name)))

(defun syscall--read-syscall (arch kernel &optional prompt)
  "Read a syscall name of ARCH in KERNEL with PROMPT, return the syscall."
  (let* ((table (syscall-table arch kernel))
         (by-name (make-hash-table :test #'equal))
         names)
    (unless table
      (user-error "No syscalls for %s in %s" arch (syscall--kernel-label arch kernel)))
    (dolist (sc table)
      (let ((name (alist-get 'name sc)))
        (unless (and (gethash name by-name) (alist-get 'removed_in sc))
          (unless (gethash name by-name) (push name names))
          (puthash name sc by-name))))
    (setq names (nreverse names))
    (let* ((width (+ 2 (apply #'max (mapcar #'length names))))
           (default (syscall--name-at-point arch kernel))
           (annotate
            (lambda (name)
              (when-let* ((sc (gethash name by-name)))
                (concat (make-string (max 1 (- width (length name))) ?\s)
                        (propertize (format "%5d %-7s %s" (alist-get 'nr sc)
                                            (syscall--hex (alist-get 'nr sc))
                                            (or (alist-get 'description sc) ""))
                                    'face 'completions-annotations)))))
           (name (completing-read
                  (format-prompt (or prompt (format "Syscall (%s, %s)" arch
                                                    (syscall--kernel-label arch kernel)))
                                 default)
                  (syscall--completion-table names annotate 'syscall)
                  nil t nil 'syscall--history default)))
      (or (gethash name by-name) (user-error "No syscall %s on %s" name arch)))))

(defun syscall--arch-arg (arg)
  "Architecture for a command: asked for with prefix ARG, else the default."
  (if arg (syscall--read-arch) syscall-default-arch))

;;;; Detail buffer

(defvar-local syscall--arch nil "Architecture shown in this buffer.")
(defvar-local syscall--kernel nil "Kernel shown in this buffer, as in `syscall-default-kernel'.")
(defvar-local syscall--entry nil "Syscall shown in this detail buffer.")
(defvar-local syscall--filter nil "Filter of this list buffer, or nil.")
(defvar-local syscall--family nil "Family this list buffer is narrowed to, or nil.")

(defvar-keymap syscall-detail-mode-map
  :doc "Keymap for `syscall-detail-mode'."
  "n" #'syscall-detail-next
  "p" #'syscall-detail-previous
  "a" #'syscall-detail-other-arch
  "k" #'syscall-detail-kernel
  "l" #'syscall-detail-list
  "c" #'syscall-detail-compare
  "m" #'syscall-browse-man-page
  "s" #'syscall-browse-source
  "w" #'syscall-copy-number
  "W" #'syscall-copy-hex-number
  "i" #'syscall-copy-asm-stub
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode syscall-detail-mode special-mode "Syscall"
  "Major mode describing one syscall.

\\{syscall-detail-mode-map}"
  (setq-local revert-buffer-function #'syscall--detail-revert)
  (setq truncate-lines t))

(defun syscall--field (label value &optional face)
  "Insert a LABEL: VALUE line when VALUE is non-empty, VALUE in FACE."
  (when (and value (not (equal value "")))
    (insert "  " (propertize (syscall--pad label 13) 'face 'syscall-dim)
            (if face (propertize value 'face face) value) "\n")))

(defun syscall--heading (text)
  "Insert the section heading TEXT."
  (insert "\n" (propertize text 'face 'syscall-heading) "\n"))

(defun syscall--insert-registers (arch syscall)
  "Insert the register table of SYSCALL on ARCH."
  (when-let* ((conv (syscall-convention arch)))
    (let* ((nr (alist-get 'nr syscall))
           (args (syscall--args syscall))
           (rows (append
                  (list (list (alist-get 'nr conv)
                              (propertize (format "%d  %s" nr (syscall--hex nr)) 'face 'syscall-number)
                              "syscall number"))
                  (cl-loop for arg in args
                           for i from 0
                           collect (list (alist-get (intern (format "arg%d" i)) conv)
                                         (or arg "?") (format "argument %d" i)))
                  (list (list (alist-get 'return conv) "" "return value"))))
           (width (+ 2 (apply #'max (mapcar (lambda (r) (length (or (car r) ""))) rows)))))
      (syscall--heading
       (concat "Registers"
               (when-let* ((insn (alist-get 'instruction conv)))
                 (concat "  " (propertize insn 'face 'syscall-number)))))
      (dolist (row rows)
        (insert "  " (propertize (syscall--pad (or (nth 0 row) "?") width) 'face 'syscall-register)
                (syscall--pad (nth 1 row) 30)
                (propertize (nth 2 row) 'face 'syscall-dim) "\n")))))

(defun syscall--insert-other-arches (arch syscall kernel)
  "Insert where else SYSCALL exists, for KERNEL, excluding ARCH."
  (let ((name (alist-get 'name syscall))
        (kernel (if (equal kernel "all") nil kernel))
        missing)
    (syscall--heading "Other architectures")
    (dolist (other (syscall--sorted-arches))
      (unless (equal other arch)
        (if-let* ((sc (syscall-find other name kernel)))
            (let ((nr (alist-get 'nr sc)))
              (insert "  ")
              (insert-text-button (syscall--pad other 14)
                                  'action (lambda (_) (syscall--show other sc kernel))
                                  'follow-link t
                                  'help-echo (format "Show %s on %s" name other))
              (insert (propertize (format "%10d  %s" nr (syscall--hex nr)) 'face 'syscall-number)
                      "\n"))
          (push other missing))))
    (when missing
      (insert "  " (propertize "missing on " 'face 'syscall-dim)
              (string-join (nreverse missing) ", ") "\n"))))

(defun syscall--insert-link (label url)
  "Insert a button LABEL opening URL, when URL is non-nil."
  (when url
    (insert-text-button label 'action (lambda (_) (browse-url url))
                        'follow-link t 'help-echo url)
    (insert "  ")))

(defun syscall--render-detail ()
  "Fill the current detail buffer."
  (let* ((inhibit-read-only t)
         (arch syscall--arch)
         (sc syscall--entry)
         (kernel syscall--kernel)
         (nr (alist-get 'nr sc))
         (notes (syscall--notes arch)))
    (erase-buffer)
    (insert (propertize (alist-get 'name sc) 'face 'syscall-title) "  "
            (propertize (format "%s, %s" arch (syscall--arch-title arch)) 'face 'syscall-dim) "\n")
    (when-let* ((desc (alist-get 'description sc)))
      (insert desc "\n"))
    (insert "\n")
    (syscall--field "number" (format "%d  %s" nr (syscall--hex nr)) 'syscall-number)
    (syscall--field "signature" (syscall-signature sc))
    (syscall--field "entry point" (alist-get 'entry_point sc))
    (syscall--field "abi" (alist-get 'abi sc))
    (syscall--field "since" (syscall--since-label arch sc))
    (when-let* ((removed (alist-get 'removed_in sc)))
      (syscall--field "removed in" (format "Linux %s" removed) 'syscall-removed))
    (syscall--field "kconfig" (alist-get 'kconfig sc))
    (syscall--field "family" (alist-get 'family sc))
    (syscall--field "showing" (syscall--kernel-label arch kernel))
    (syscall--insert-registers arch sc)
    (when notes
      (syscall--heading "Notes")
      (dolist (note notes)
        (let ((start (point)))
          (insert "  - " note "\n")
          (let ((fill-prefix "    ") (fill-column 80))
            (fill-region start (point))))))
    (syscall--insert-other-arches arch sc kernel)
    (when (or (alist-get 'man_page sc) (alist-get 'source sc))
      (insert "\n  ")
      (syscall--insert-link "[man page]" (alist-get 'man_page sc))
      (syscall--insert-link "[source]" (alist-get 'source sc))
      (insert "\n"))))

(defun syscall--show (arch syscall &optional kernel)
  "Describe SYSCALL of ARCH in KERNEL in a named buffer."
  (let ((buffer (get-buffer-create (format "*syscall %s*" (alist-get 'name syscall)))))
    (with-current-buffer buffer
      (syscall-detail-mode)
      (setq syscall--arch arch
            syscall--kernel kernel
            syscall--entry syscall)
      (syscall--render-detail)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun syscall--detail-revert (&rest _)
  "Redraw the detail buffer from the cache."
  (let* ((old syscall--entry)
         (table (syscall-table syscall--arch syscall--kernel))
         (sc (or (cl-find-if (lambda (s) (and (equal (alist-get 'name s) (alist-get 'name old))
                                              (= (alist-get 'nr s) (alist-get 'nr old))))
                             table)
                 (syscall-find syscall--arch (alist-get 'name old) syscall--kernel)
                 old))
         (pos (point)))
    (setq syscall--entry sc)
    (syscall--render-detail)
    (goto-char (min pos (point-max)))))

;;;###autoload
(defun syscall-lookup (arch syscall)
  "Describe SYSCALL of ARCH, read with completion.
The default is the syscall named at point, such as read, SYS_read or
__NR_read.  With a prefix argument, ask for the architecture."
  (interactive
   (progn (syscall-ensure-data)
          (let ((arch (syscall--arch-arg current-prefix-arg)))
            (list arch (syscall--read-syscall arch syscall-default-kernel)))))
  (syscall--show arch syscall syscall-default-kernel))

;;;###autoload
(defun syscall-lookup-number (arch nr)
  "Describe the syscall number NR of ARCH.
NR is read as decimal or 0x hexadecimal.  With a prefix argument, ask
for the architecture."
  (interactive
   (progn (syscall-ensure-data)
          (let* ((arch (syscall--arch-arg current-prefix-arg))
                 (input (read-string (format "Syscall number on %s (decimal or 0x hex): " arch)))
                 (nr (or (syscall--parse-number input)
                         (user-error "Not a syscall number: %s" input))))
            (list arch nr))))
  (if-let* ((sc (syscall-find-number arch nr syscall-default-kernel)))
      (syscall--show arch sc syscall-default-kernel)
    (user-error "No syscall %d (%s) on %s in %s" nr (syscall--hex nr) arch
                (syscall--kernel-label arch syscall-default-kernel))))

(defun syscall--current ()
  "The syscall at point, as (ARCH SYSCALL KERNEL), or signal an error."
  (cond ((derived-mode-p 'syscall-detail-mode)
         (list syscall--arch syscall--entry syscall--kernel))
        ((and (derived-mode-p 'syscall-list-mode) (tabulated-list-get-id))
         (list syscall--arch (tabulated-list-get-id) syscall--kernel))
        ((and (derived-mode-p 'syscall-compare-mode) (tabulated-list-get-id))
         (let ((id (tabulated-list-get-id)))
           (list (car id) (cdr id) syscall--kernel)))
        (t (user-error "No syscall here"))))

(defun syscall--detail-step (step)
  "Show the syscall STEP positions away in the current table."
  (let* ((table (syscall-table syscall--arch syscall--kernel))
         (pos (or (cl-position syscall--entry table :test #'eq)
                  (cl-position-if (lambda (s) (>= (alist-get 'nr s) (alist-get 'nr syscall--entry)))
                                  table)
                  0))
         (target (+ pos step)))
    (unless (and (>= target 0) (< target (length table)))
      (user-error "No %s syscall" (if (> step 0) "next" "previous")))
    (syscall--show syscall--arch (nth target table) syscall--kernel)))

(defun syscall-detail-next ()
  "Show the syscall with the next number."
  (interactive)
  (syscall--detail-step 1))

(defun syscall-detail-previous ()
  "Show the syscall with the previous number."
  (interactive)
  (syscall--detail-step -1))

(defun syscall-detail-other-arch (arch)
  "Show the same syscall on ARCH."
  (interactive (list (syscall--read-arch "Show on architecture")))
  (let* ((name (alist-get 'name syscall--entry))
         (kernel (if (equal syscall--kernel "all") "all" syscall--kernel)))
    (if-let* ((sc (syscall-find arch name kernel)))
        (syscall--show arch sc kernel)
      (user-error "%s has no %s in %s" arch name (syscall--kernel-label arch kernel)))))

(defun syscall-detail-kernel (kernel)
  "Show the same syscall as it is in KERNEL."
  (interactive (list (syscall--read-kernel syscall--arch)))
  (let ((name (alist-get 'name syscall--entry)))
    (if-let* ((sc (syscall-find syscall--arch name kernel)))
        (syscall--show syscall--arch sc kernel)
      (user-error "%s has no %s in %s" syscall--arch name
                  (syscall--kernel-label syscall--arch kernel)))))

(defun syscall-detail-list ()
  "List the syscalls of this architecture, with point on this one."
  (interactive)
  (let ((sc syscall--entry))
    (syscall-list syscall--arch syscall--kernel)
    (syscall--list-goto sc)))

(defun syscall-detail-compare ()
  "Compare this syscall across architectures."
  (interactive)
  (syscall-compare (alist-get 'name syscall--entry) syscall--kernel))

(defun syscall-browse-man-page ()
  "Open the man page of the syscall at point."
  (interactive)
  (pcase-let ((`(,_ ,sc ,_) (syscall--current)))
    (browse-url (or (alist-get 'man_page sc) (user-error "No man page for %s" (alist-get 'name sc))))))

(defun syscall-browse-source ()
  "Open the kernel source of the syscall at point."
  (interactive)
  (pcase-let ((`(,_ ,sc ,_) (syscall--current)))
    (browse-url (or (alist-get 'source sc) (user-error "No source link for %s" (alist-get 'name sc))))))

(defun syscall--copy (string)
  "Put STRING in the kill ring and say so."
  (kill-new string)
  (message "Copied %s" string))

(defun syscall-copy-number ()
  "Copy the number of the syscall at point, in decimal."
  (interactive)
  (syscall--copy (number-to-string (alist-get 'nr (nth 1 (syscall--current))))))

(defun syscall-copy-hex-number ()
  "Copy the number of the syscall at point, in hexadecimal."
  (interactive)
  (syscall--copy (syscall--hex (alist-get 'nr (nth 1 (syscall--current))))))

(defun syscall-copy-asm-stub ()
  "Copy an assembly stub calling the syscall at point."
  (interactive)
  (pcase-let ((`(,arch ,sc ,_) (syscall--current)))
    (kill-new (syscall-asm-stub arch sc))
    (message "Copied the %s stub for %s" arch (alist-get 'name sc))))

;;;; List buffer

(defvar-keymap syscall-list-mode-map
  :doc "Keymap for `syscall-list-mode'."
  "RET" #'syscall-list-describe
  "/" #'syscall-list-filter
  "f" #'syscall-list-family
  "a" #'syscall-list-arch
  "k" #'syscall-list-kernel
  "c" #'syscall-list-compare
  "d" #'syscall-list-diff
  "m" #'syscall-browse-man-page
  "s" #'syscall-browse-source
  "w" #'syscall-copy-number
  "W" #'syscall-copy-hex-number
  "i" #'syscall-copy-asm-stub)

(defun syscall--sort-by-nr (a b)
  "Non-nil when entry A has a lower number than entry B."
  (< (alist-get 'nr (car a)) (alist-get 'nr (car b))))

(defun syscall--sort-by-nargs (a b)
  "Non-nil when entry A takes fewer arguments than entry B."
  (< (or (alist-get 'nargs (car a)) -1) (or (alist-get 'nargs (car b)) -1)))

(define-derived-mode syscall-list-mode tabulated-list-mode "Syscalls"
  "Major mode listing the syscalls of one architecture.

\\{syscall-list-mode-map}"
  (setq tabulated-list-format
        [("NR" 6 syscall--sort-by-nr :right-align t)
         ("HEX" 11 syscall--sort-by-nr)
         ("NAME" 24 t)
         ("ARGS" 4 syscall--sort-by-nargs :right-align t)
         ("DESCRIPTION" 0 t)])
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'syscall--list-refresh nil t)
  (tabulated-list-init-header))

(defun syscall--row-text (syscall)
  "Text a filter matches against for SYSCALL.
The number columns, as on syscall.sh, plus what the API's q filter
covers: name, description, arguments, family and kconfig option."
  (downcase (string-join (delq nil (list (number-to-string (alist-get 'nr syscall))
                                         (syscall--hex (alist-get 'nr syscall))
                                         (syscall-signature syscall)
                                         (alist-get 'description syscall)
                                         (alist-get 'family syscall)
                                         (alist-get 'kconfig syscall)))
                         " ")))

(defun syscall-filter-predicate (query)
  "Predicate for syscalls matching QUERY, as in the syscall.sh filter.
A number such as 59 or 0x3b matches that syscall exactly, anything
else matches any part of the row, ignoring case."
  (let ((q (downcase (string-trim (or query "")))))
    (cond ((string-empty-p q) #'always)
          ((syscall--parse-number q)
           (let ((n (syscall--parse-number q)))
             (lambda (sc) (= (alist-get 'nr sc) n))))
          (t (lambda (sc) (string-search q (syscall--row-text sc)))))))

(defun syscall--list-row (syscall all)
  "Table row of SYSCALL; ALL is non-nil when every release is shown."
  (let* ((nr (alist-get 'nr syscall))
         (removed (alist-get 'removed_in syscall))
         (nargs (alist-get 'nargs syscall))
         (desc (or (alist-get 'description syscall) "")))
    (vector (propertize (number-to-string nr) 'face 'syscall-number)
            (propertize (syscall--hex nr) 'face 'syscall-number)
            (propertize (alist-get 'name syscall)
                        'face (if (and removed all) 'syscall-removed 'syscall-name))
            (if nargs (number-to-string nargs) "?")
            (if removed
                (concat desc (propertize (format " (removed in %s)" removed) 'face 'syscall-dim))
              desc))))

(defun syscall--list-refresh ()
  "Recompute the entries of the current list buffer."
  (let ((pred (syscall-filter-predicate syscall--filter))
        (all (equal syscall--kernel "all")))
    (setq tabulated-list-entries
          (cl-loop for sc in (syscall-table syscall--arch syscall--kernel)
                   when (and (funcall pred sc)
                             (or (null syscall--family)
                                 (equal syscall--family (alist-get 'family sc))))
                   collect (list sc (syscall--list-row sc all))))
    (setq mode-line-process
          (concat " " syscall--arch ", " (syscall--kernel-label syscall--arch syscall--kernel)
                  (when syscall--family (format ", family %s" syscall--family))
                  (when syscall--filter (format ", filter %S" syscall--filter))
                  (format ", %d" (length tabulated-list-entries))))))

(defun syscall--list-goto (syscall)
  "Move point to SYSCALL's row, if shown."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (eq (tabulated-list-get-id) syscall)))
    (forward-line 1))
  (when (eobp) (goto-char (point-min))))

;;;###autoload
(defun syscall-list (arch &optional kernel)
  "List the syscalls of ARCH in KERNEL.
KERNEL defaults to `syscall-default-kernel'."
  (interactive (progn (syscall-ensure-data) (list (syscall--read-arch))))
  (syscall-ensure-data)
  (let ((buffer (get-buffer-create (format "*syscalls %s*" arch))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'syscall-list-mode)
        (syscall-list-mode))
      (setq syscall--arch arch
            syscall--kernel (or kernel syscall-default-kernel))
      (syscall--list-refresh)
      (unless (syscall-table arch syscall--kernel)
        (message "%s was not scanned for %s" arch (syscall--kernel-label arch syscall--kernel)))
      (tabulated-list-print t))
    (pop-to-buffer-same-window buffer)))

(defun syscall-list-describe ()
  "Describe the syscall at point."
  (interactive)
  (pcase-let ((`(,arch ,sc ,kernel) (syscall--current)))
    (syscall--show arch sc kernel)))

(defun syscall--list-apply-filter (query)
  "Show only rows matching QUERY in the current list buffer."
  (setq syscall--filter (unless (string-empty-p (string-trim query)) (string-trim query)))
  (syscall--list-refresh)
  (tabulated-list-print t))

(defun syscall-list-filter (query)
  "Show only syscalls matching QUERY.  An empty QUERY shows all.
A number such as 59 or 0x3b matches that syscall, anything else any
part of the row.  The list updates while typing."
  (interactive
   (let ((buffer (current-buffer))
         (old syscall--filter))
     (condition-case nil
         (list (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'after-change-functions
                               (lambda (&rest _)
                                 (let ((q (minibuffer-contents-no-properties)))
                                   (with-current-buffer buffer
                                     (syscall--list-apply-filter q))))
                               nil t))
                 (read-string "Filter (number, 0x hex or text): " old)))
       (quit (with-current-buffer buffer (syscall--list-apply-filter (or old "")))
             (signal 'quit nil)))))
  (syscall--list-apply-filter query))

(defun syscall-list-arch (arch)
  "List the syscalls of ARCH instead."
  (interactive (list (syscall--read-arch nil syscall--arch)))
  (syscall-list arch syscall--kernel))

(defun syscall-list-kernel (kernel)
  "Show the syscalls of KERNEL instead."
  (interactive (list (syscall--read-kernel syscall--arch)))
  (setq syscall--kernel kernel)
  (syscall--list-refresh)
  (tabulated-list-print t))

(defun syscall-families (arch)
  "Families of the syscalls ARCH ever had, such as epoll or io_uring."
  (sort (delete-dups (delq nil (mapcar (lambda (sc) (alist-get 'family sc))
                                       (syscall-arch-syscalls arch))))
        #'string<))

(defun syscall-list-family (family)
  "Show only syscalls of FAMILY, such as epoll.  An empty FAMILY shows all."
  (interactive
   (list (completing-read (format-prompt "Family" "all")
                          (syscall-families syscall--arch) nil t)))
  (setq syscall--family (unless (string-empty-p family) family))
  (syscall--list-refresh)
  (tabulated-list-print t))

(defun syscall-list-compare ()
  "Compare the syscall at point across architectures."
  (interactive)
  (pcase-let ((`(,_ ,sc ,kernel) (syscall--current)))
    (syscall-compare (alist-get 'name sc) kernel)))

(defun syscall-list-diff (from)
  "Show what changed on this architecture since the release FROM."
  (interactive (list (syscall--read-kernel syscall--arch "Compare from" t)))
  (let ((to (if (member syscall--kernel '(nil "all"))
                (syscall--resolve-kernel syscall--arch nil)
              syscall--kernel)))
    (syscall-diff syscall--arch from to)))

;;;; Conventions

(defvar-keymap syscall-conventions-mode-map
  :doc "Keymap for `syscall-conventions-mode'."
  "RET" #'syscall-conventions-list
  "i" #'syscall-conventions-notes)

(define-derived-mode syscall-conventions-mode tabulated-list-mode "Syscall conventions"
  "Major mode showing the calling convention of every architecture.

\\{syscall-conventions-mode-map}"
  (setq tabulated-list-format
        (vconcat [("ARCH" 12 t) ("NAME" 24 t) ("INSN" 12 t) ("NR" 5 t) ("RET" 5 t)]
                 (mapcar (lambda (i) (list (format "ARG%d" i) 5 nil)) (number-sequence 0 5))
                 [("NOTES" 0 nil)]))
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'syscall--conventions-refresh nil t)
  (tabulated-list-init-header))

(defun syscall--conventions-refresh ()
  "Recompute the rows of the conventions buffer."
  (setq tabulated-list-entries
        (cl-loop for arch in (syscall--sorted-arches)
                 for conv = (syscall-convention arch)
                 when conv
                 collect
                 (list arch
                       (vconcat
                        (list (propertize arch 'face 'syscall-name)
                              (syscall--arch-title arch)
                              (propertize (or (alist-get 'instruction conv) "") 'face 'syscall-number)
                              (propertize (or (alist-get 'nr conv) "") 'face 'syscall-register)
                              (propertize (or (alist-get 'return conv) "") 'face 'syscall-register))
                        (mapcar (lambda (i)
                                  (propertize (or (alist-get (intern (format "arg%d" i)) conv) "")
                                              'face 'syscall-register))
                                (number-sequence 0 5))
                        (list (let ((n (length (syscall--notes arch))))
                                (if (> n 0)
                                    (propertize (format "%d, press i" n) 'face 'syscall-dim)
                                  ""))))))))

;;;###autoload
(defun syscall-conventions-table ()
  "Show the calling convention of every architecture."
  (interactive)
  (syscall-ensure-data)
  (let ((buffer (get-buffer-create "*syscall conventions*")))
    (with-current-buffer buffer
      (syscall-conventions-mode)
      (syscall--conventions-refresh)
      (tabulated-list-print))
    (pop-to-buffer-same-window buffer)))

(defun syscall-conventions-list ()
  "List the syscalls of the architecture at point."
  (interactive)
  (syscall-list (or (tabulated-list-get-id) (user-error "No architecture here"))))

(defun syscall-conventions-notes ()
  "Show the ABI notes of the architecture at point."
  (interactive)
  (let* ((arch (or (tabulated-list-get-id) (user-error "No architecture here")))
         (notes (syscall--notes arch)))
    (if notes
        (message "%s: %s" arch (string-join notes "\n"))
      (message "No notes for %s" arch))))

;;;; Compare across architectures

(define-derived-mode syscall-compare-mode tabulated-list-mode "Syscall compare"
  "Major mode comparing one syscall across architectures."
  (setq tabulated-list-format
        [("ARCH" 12 t) ("NAME" 24 t) ("NR" 10 syscall--sort-compare :right-align t)
         ("HEX" 11 syscall--sort-compare) ("SIGNATURE" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(keymap-set syscall-compare-mode-map "RET" #'syscall-list-describe)
(keymap-set syscall-compare-mode-map "m" #'syscall-browse-man-page)
(keymap-set syscall-compare-mode-map "w" #'syscall-copy-number)
(keymap-set syscall-compare-mode-map "i" #'syscall-copy-asm-stub)

(defun syscall--sort-compare (a b)
  "Order compare entries A and B by number."
  (< (alist-get 'nr (cdar a)) (alist-get 'nr (cdar b))))

;;;###autoload
(defun syscall-compare (name &optional kernel)
  "Show the syscall NAME on every architecture that has it, in KERNEL."
  (interactive
   (progn (syscall-ensure-data)
          (list (alist-get 'name (syscall--read-syscall
                                  (syscall--arch-arg current-prefix-arg) syscall-default-kernel))
                syscall-default-kernel)))
  (let* ((kernel (if (equal kernel "all") nil kernel))
         (buffer (get-buffer-create (format "*syscall compare %s*" name)))
         missing entries)
    (dolist (arch (syscall--sorted-arches))
      (if-let* ((sc (syscall-find arch name kernel)))
          (push (list (cons arch sc)
                      (vector (propertize arch 'face 'syscall-name)
                              (syscall--arch-title arch)
                              (propertize (number-to-string (alist-get 'nr sc)) 'face 'syscall-number)
                              (propertize (syscall--hex (alist-get 'nr sc)) 'face 'syscall-number)
                              (syscall-signature sc)))
                entries)
        (push arch missing)))
    (with-current-buffer buffer
      (syscall-compare-mode)
      (setq syscall--kernel kernel
            tabulated-list-entries (nreverse entries)
            mode-line-process (format " %s, %s" name
                                      (if kernel (format "Linux %s" kernel) "latest")))
      (tabulated-list-print)
      (when missing
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n " (propertize (concat "missing on " (string-join (nreverse missing) ", "))
                                    'face 'syscall-dim)
                  "\n"))
        (goto-char (point-min))))
    (pop-to-buffer-same-window buffer)))

;;;; Kernel diff

(define-derived-mode syscall-diff-mode special-mode "Syscall diff"
  "Major mode showing the syscalls added and removed between releases.")

(defun syscall-diff-entries (arch from to)
  "Syscalls of ARCH added and removed from release FROM to release TO.
Return (ADDED . REMOVED).  A syscall that changed number is in both."
  (let* ((key (lambda (sc) (cons (alist-get 'name sc) (alist-get 'nr sc))))
         (index (lambda (table)
                  (let ((h (make-hash-table :test #'equal)))
                    (dolist (sc table h) (puthash (funcall key sc) t h)))))
         (old (syscall-table arch from))
         (new (syscall-table arch to))
         (in-old (funcall index old))
         (in-new (funcall index new)))
    (cons (cl-remove-if (lambda (sc) (gethash (funcall key sc) in-old)) new)
          (cl-remove-if (lambda (sc) (gethash (funcall key sc) in-new)) old))))

(defun syscall--insert-diff-section (title arch syscalls kernel)
  "Insert section TITLE listing SYSCALLS of ARCH, linked to KERNEL."
  (syscall--heading (format "%s (%d)" title (length syscalls)))
  (if (null syscalls)
      (insert "  " (propertize "none" 'face 'syscall-dim) "\n")
    (let ((width (+ 2 (apply #'max (mapcar (lambda (sc) (length (alist-get 'name sc))) syscalls)))))
      (dolist (sc syscalls)
        (let ((nr (alist-get 'nr sc)))
          (insert (propertize (format "  %10d %-11s " nr (syscall--hex nr)) 'face 'syscall-number))
          (insert-text-button (alist-get 'name sc)
                              'action (lambda (_) (syscall--show arch sc kernel))
                              'follow-link t)
          (insert (make-string (- width (length (alist-get 'name sc))) ?\s)
                  (or (alist-get 'description sc) "") "\n"))))))

;;;###autoload
(defun syscall-diff (arch from to)
  "Show the syscalls ARCH gained and lost from release FROM to release TO.
Computed from the cache, so it works offline."
  (interactive
   (progn (syscall-ensure-data)
          (let* ((arch (syscall--read-arch))
                 (versions (syscall--arch-kernels arch))
                 (to (syscall--read-kernel arch "To release" t))
                 (prev (cadr (member to (reverse versions))))
                 (from (syscall--read-kernel arch "From release" t prev)))
            (list arch from to))))
  (pcase-let ((`(,added . ,removed) (syscall-diff-entries arch from to))
              (buffer (get-buffer-create (format "*syscall diff %s %s..%s*" arch from to))))
    (with-current-buffer buffer
      (syscall-diff-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s: Linux %s to %s" arch from to) 'face 'syscall-title) "\n")
        (dolist (k (list from to))
          (unless (member k (syscall--arch-kernels arch))
            (insert (propertize (format "%s was not scanned for %s\n" arch k) 'face 'syscall-dim))))
        (syscall--insert-diff-section "Added" arch added to)
        (syscall--insert-diff-section "Removed" arch removed from)
        (goto-char (point-min))))
    (pop-to-buffer-same-window buffer)))

;;;; Assembly stubs

(defun syscall--asm-style (arch)
  "Plist describing assembly syntax for ARCH.
:mov formats a register load, :comment starts a comment, and :wide,
when present, loads values that `syscall--arm-immediate-p' rejects."
  (pcase arch
    ((or "x64" "x86" "x32" "ia32") '(:mov "mov %s, %s" :comment ";"))
    ("arm" '(:mov "mov %s, #%s" :wide "ldr %s, =%s" :comment "@"))
    ("arm64" '(:mov "mov %s, #%s" :comment "//"))
    ((or "riscv64" "mips64" "mipsn32" "mipso32") '(:mov "li %s, %s" :comment "#"))
    (_ '(:mov nil :comment "#"))))

(defun syscall--arm-immediate-p (n)
  "Non-nil when N fits an ARM data processing immediate.
That is an 8 bit value rotated right by an even amount."
  (cl-loop for rot from 0 below 32 by 2
           thereis (< (logand (logior (ash n rot) (ash n (- rot 32))) #xffffffff) 256)))

(defun syscall-asm-stub (arch syscall &optional comment)
  "Assembly that calls SYSCALL on ARCH, as a string.
Arguments are set to 0, with their declaration as a comment.  COMMENT
is the comment prefix, by default the usual one for ARCH.
Architectures without a known syntax get the register assignments as
comments only."
  (let* ((conv (or (syscall-convention arch) (error "No calling convention for %s" arch)))
         (style (syscall--asm-style arch))
         (comment (or comment (plist-get style :comment)))
         (mov (plist-get style :mov))
         (nr (alist-get 'nr syscall))
         (nr-reg (alist-get 'nr conv))
         (insn (alist-get 'instruction conv))
         (lines nil))
    ;; mov eax, N clears the upper half of rax, which kernels before 5.14 need.
    (when (and (member arch '("x64" "x32")) (equal nr-reg "rax"))
      (setq nr-reg "eax"))
    (cl-flet ((line (reg value text note)
                (let ((fmt (if (and (plist-get style :wide)
                                    (not (syscall--arm-immediate-p value)))
                               (plist-get style :wide)
                             mov)))
                  (push (cons (if fmt
                                  (format fmt reg text)
                                (format "%s %s = %s" comment reg text))
                              note)
                        lines))))
      (line nr-reg nr (syscall--hex nr) (alist-get 'name syscall))
      (cl-loop for arg in (syscall--args syscall)
               for i from 0
               do (line (or (alist-get (intern (format "arg%d" i)) conv) "?") 0 "0" (or arg "?"))))
    (setq lines (nreverse lines))
    (when insn
      (setq lines (append lines (list (cons (if mov insn (concat comment " " insn)) nil)))))
    (let ((width (+ 2 (apply #'max (mapcar (lambda (l) (length (car l))) lines)))))
      (concat comment " " (syscall-signature syscall)
              (format ", %s %d\n" arch nr)
              (mapconcat (lambda (l)
                           (if (cdr l)
                               (concat (syscall--pad (car l) width) comment " " (cdr l))
                             (car l)))
                         lines "\n")
              "\n"))))

(defun syscall--buffer-comment ()
  "Line comment prefix of the current buffer, or nil."
  (when (and (stringp comment-start)
             (not (string-empty-p (string-trim comment-start)))
             (or (null comment-end) (string-empty-p comment-end)))
    (string-trim comment-start)))

;;;###autoload
(defun syscall-insert-asm-stub (arch syscall)
  "Insert assembly that calls SYSCALL on ARCH at point.
With a prefix argument, ask for the architecture."
  (interactive
   (progn (syscall-ensure-data)
          (let ((arch (syscall--arch-arg current-prefix-arg)))
            (list arch (syscall--read-syscall arch syscall-default-kernel)))))
  (let* ((indent (make-string (current-column) ?\s))
         (stub (syscall-asm-stub arch syscall (syscall--buffer-comment)))
         (lines (split-string (string-trim-right stub) "\n")))
    (insert (car lines))
    (dolist (l (cdr lines))
      (insert "\n" indent l))
    (insert "\n")))

;;;; ElDoc

(defcustom syscall-eldoc-bare-names nil
  "When non-nil, `syscall-eldoc-mode' also describes unprefixed names.
By default only names such as SYS_read, __NR_read or sys_read count,
since bare words like read or open are common identifiers."
  :type 'boolean)

(defun syscall-eldoc-function (&rest _)
  "ElDoc documentation for the syscall named at point, or nil.
Never downloads anything: it stays silent until the cache exists."
  (when (syscall--ready-p)
    (ignore-errors
      (when-let* ((name (syscall--name-at-point syscall-default-arch syscall-default-kernel
                                                (not syscall-eldoc-bare-names)))
                  (sc (syscall-find syscall-default-arch name syscall-default-kernel)))
        (let ((nr (alist-get 'nr sc)))
          (concat (propertize (alist-get 'name sc) 'face 'syscall-name)
                  (substring (syscall-signature sc) (length (alist-get 'name sc)))
                  "  "
                  (propertize (format "%s %d %s" syscall-default-arch nr (syscall--hex nr))
                              'face 'syscall-dim)))))))

;;;###autoload
(define-minor-mode syscall-eldoc-mode
  "Show the signature of the syscall at point with ElDoc.
Recognizes SYS_name, __NR_name, sys_name and, with
`syscall-eldoc-bare-names', plain names, on `syscall-default-arch'."
  :lighter " Sys"
  (if syscall-eldoc-mode
      (progn
        (add-hook 'eldoc-documentation-functions #'syscall-eldoc-function nil t)
        (eldoc-mode 1))
    (remove-hook 'eldoc-documentation-functions #'syscall-eldoc-function t)))

;;;; Settings and key map

;;;###autoload
(defun syscall-set-arch (arch)
  "Make ARCH the default architecture for this session."
  (interactive (progn (syscall-ensure-data) (list (syscall--read-arch "Default architecture"))))
  (setq syscall-default-arch arch)
  (message "Syscall architecture: %s, %s" arch (syscall--arch-title arch)))

;;;###autoload
(defun syscall-set-kernel (kernel)
  "Make KERNEL the default release for this session.  nil is the latest."
  (interactive (progn (syscall-ensure-data)
                      (list (syscall--read-kernel syscall-default-arch "Default kernel"))))
  (setq syscall-default-kernel kernel)
  (message "Syscall kernel: %s" (syscall--kernel-label syscall-default-arch kernel)))

;;;###autoload
(defvar-keymap syscall-command-map
  :doc "Prefix map of syscall commands, to bind to a key of your choice."
  "l" #'syscall-lookup
  "n" #'syscall-lookup-number
  "L" #'syscall-list
  "c" #'syscall-compare
  "C" #'syscall-conventions-table
  "d" #'syscall-diff
  "i" #'syscall-insert-asm-stub
  "a" #'syscall-set-arch
  "k" #'syscall-set-kernel
  "u" #'syscall-update)

(provide 'syscall)
;;; syscall.el ends here
