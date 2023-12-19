;;; org-toggl.el --- A simple Org-mode interface to Toggl  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Marcin Borkowski

;; Author: Marcin Borkowski <mbork@mbork.pl>
;; Keywords: calendar
;; Package-Requires: ((request "0.2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple Org-mode interface to Toggl, a time-tracking service.
;; Hooks into the Org-mode's clocking mechanism.

;;; Code:

(require 'json)
(require 'request)

(defcustom toggl-auth-token ""
  "Authentication token for Toggl."
  :type 'string
  :group 'toggl)

(defcustom toggl-default-timeout 20
  "Default timeout for HTTP requests."
  :type 'integer
  :group 'toggl)

;; FIXME change this to name and get the id in `toggl-get-projects'
(defcustom toggl-workspace-id nil
  "Toggl workspace id.
Can be looked up in the URL.  Click \"manage workspaces\" and
select the workspace - the number after `/workspaces/` is the
workspace id."
  :type 'integer
  :group 'toggl)

(defcustom toggl-case-insensitive case-fold-search
  "Should org-toggl ignore string case when looking for projects/tags?

If non-nil, ignore string case when trying to find a matching project (in
`toggl-projects') or tag (in `toggl-tags'),"
  :type 'boolean
  :group 'toggl)

(defcustom toggl-default-start-time "-mon 8am"
  "Default start time for `toggl-set-time-entry'.

Value should be:
  - A string that `org-read-date' can parse (e.g. \"-mon\" for \"last Monday\").
  - A list/integer/etc understandable by `format-time-string'."
  :type '(choice
          (string :tag "`org-read-date' string")
          (nil  :tag "nil for \"current time\"")
          (integer :tag "seconds since epoch")
          ((cons integer integer) :tag "An integer pair (TICKS . HZ) for TICKS/HZ seconds")
          ((list integer integer integer integer) :tag "An integer list (HI LO US PS) for HI*2**16 + LO + US/10**6 + PS/10**12 second"))
  :group 'toggl)

(defun toggl-parse-duration (input)
  "Parse INPUT, return duration as integer number of seconds or nil."
  (let ((input (string-trim input)))
    ;; Maybe Org can parse it?
    (condition-case nil
        (floor ; integer
         (* 60 ; minutes -> seconds
            (org-duration-to-minutes input)))
      ;; `org-duration-to-minutes' raises `error' when it doesn't understand its input.
      (error
       ;; Maybe it's an ISO-8601 duration?
       (condition-case nil
           (let ((duration (iso8601-parse-duration input)))
             ;; Convert to (integer) seconds.
             (floor
              (+ (decoded-time-second duration)
                 (* (decoded-time-minute duration) 60)
                 (* (decoded-time-hour   duration) 60 60)
                 (* (decoded-time-day    duration) 60 60 24)
                 ;; Why bother supporting more than days? Months & years are
                 ;; messy to calculate duration of and this is for timecards.
                 )))
         ;; `iso-8601-parse-duration' raises `wrong-type-argument' when it doesn't understand its input.
         (wrong-type-argument
          ;; IDK... Ignore error and return something invalid?
          nil))))))

(defun toggl-prompt-duration (prompt &optional default-input)
  "Prompt user for a duration, return an integer number of seconds.

PROMPT string will be used in minibuffer prompt.

DEFAULT-INPUT, if non-nil, will be provided as the already-filled-in input to
the prompt."
  (toggl-parse-duration (read-string prompt nil nil default-input)))

(defun toggl--duration-display (seconds)
  "Convert integer number of SECONDS to a display string."
  (org-duration-from-minutes (/ seconds 60.0) 'h:mm:ss))

(defun toggl-api-duration (duration)
  "Get DURATION as an integer number of seconds for Toggl's API.

Use `org-duration-to-minutes', which uses `org-duration-units' for some of the
units it understands.

Special Cases:
  - \"running\" / \"ongoing\":
    - DURATION of `:running' will return -1.
    - A negative number as DURATION will return -1.

examples:
  (toggl-api-duration \"80h\")
    -> 288000
  (toggl-api-duration \"80:00:00\")
    -> 288000
  (toggl-api-duration 1)
    -> 60
  (toggl-api-duration :now)
    -> -1"
  (cond
   ;;------------------------------
   ;; Special Cases
   ;;------------------------------
   ((or (and (keywordp duration)
             (eq :running duration))
        (and (numberp duration)
             ;; Any negative is accepted by API as a "running time entry",
             ;; but -1 is "preferable".
             (< duration 0)))
    -1)
   ((integerp duration)
    ;; Assume this integer is the correct units (seconds).
    duration)
   ;;------------------------------
   ;; Standard Duration Parsing
   ;;------------------------------
   ((stringp duration)
    (toggl-parse-duration duration))))

(defun toggl-prompt-time (prompt &optional default-input)
  "Prompt user for a datetime, return an Emacs time cons.

PROMPT string will be used in minibuffer prompt.

DEFAULT-INPUT, if non-nil, will be provided as the already-filled-in input to
the prompt."
  (org-read-date :with-time ; Want date & time.
                 :to-time   ; Want raw/internal-emacs time so we can format ourself.
                 nil
                 prompt
                 nil
                 default-input))

(defun toggl-api-timestamp (time)
  "Get TIME as a timestamp string in proper format for Toggl's API.

Value should be:
  - An already valid timestamp string (e.g. \"2023-11-30T11:32:20Z\").
  - A string that `org-read-date' can parse (e.g. \"-mon\" for \"last Monday\").
  - A list/integer/etc understandable by `format-time-string'.

If TIME is nil or empty string, return timestamp for now.

Toggl API expects ISO-8601 UTC strings. Format: 2023-11-30T11:32:20Z"
  ;; Is it already properly formatted?
  ;; `org-read-date' doesn't work 100% with ISO-8601 datetimes-
  ;; it loses the time half of the thing- so check for and
  ;; pass along those valid ISO-8601 timestamps.
  (if (and (stringp time)
           (iso8601-valid-p time))
      ;; Return ISO-8601 string as-is.
      time
    ;; Figure out the time and then format correctly.
    (format-time-string "%FT%TZ"
                        (cond
                         ;; "NOW"?
                         ((or (null time)
                              (and (stringp time)
                                   (string= ""
                                            (string-trim time))))
                          nil)
                         ;; User-friendly/permissive time strings.
                         ((stringp time)
                          (org-read-date :with-time ; Want date & time.
                                         :to-time   ; Want raw/internal-emacs time so we can format ourself.
                                         time))
                         (t
                          ;; Not a string, so it should be something that `format-time-string'
                          ;; understands?
                          time))
                        ;; Want UTC timestamp.
                        t)))

(defun toggl-default-start-timestamp ()
  "Get `toggl-default-start-time' value normalized to ISO-8061 string.

Return value is formatted by `toggl-api-timestamp' and can be fed into Toggl's
APIs."
  (toggl-api-timestamp toggl-default-start-time))

(defvar toggl-api-url "https://api.track.toggl.com/api/v9/"
  "The URL for making API calls.")

(defun toggl-create-api-url (string)
  "Prepend Toogl API URL to STRING."
  (concat toggl-api-url string))

(defun toggl-prepare-auth-header ()
  "Return a cons to be put into headers for authentication."
  (cons "Authorization"
        (format "Basic %s" (base64-encode-string (concat toggl-auth-token ":api_token")))))

(defun toggl-request-get (request &optional sync success-fun error-fun timeout)
  "Send a GET REQUEST to toggl.com, with TIMEOUT.
Add the auth token."
  (request (toggl-create-api-url request)
    :parser #'json-read
    :headers (list (toggl-prepare-auth-header))
    :success success-fun
    :error error-fun
    :sync sync
    :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-post (request data &optional sync success-fun error-fun timeout)
  "Send a POST REQUEST to toggl.com, with TIMEOUT.
Add the auth token."
  (request (toggl-create-api-url request)
    :type "POST"
    :data data
    :parser #'json-read
    :headers (list (toggl-prepare-auth-header)
                   '("Content-Type" . "application/json"))
    :success success-fun
    :error error-fun
    :sync sync
    :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-put (request data &optional sync success-fun error-fun timeout)
  "Send a PUT REQUEST to toggl.com, with TIMEOUT.
Add the auth token."
  (request (toggl-create-api-url request)
    :type "PUT"
    :data data
    :parser #'json-read
    :headers (list (toggl-prepare-auth-header)
                   '("Content-Type" . "application/json"))
    :success success-fun
    :error error-fun
    :sync sync
    :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-patch (request data &optional sync success-fun error-fun timeout)
  "Send a PATCH REQUEST to toggl.com, with TIMEOUT.
Add the auth token."
  (request (toggl-create-api-url request)
    :type "PATCH"
    :data data
    :parser #'json-read
    :headers (list (toggl-prepare-auth-header)
                   '("Content-Type" . "application/json"))
    :success success-fun
    :error error-fun
    :sync sync
    :timeout (or timeout toggl-default-timeout)))

(defun toggl-request-delete (request &optional sync success-fun error-fun timeout)
  "Send a DELETE REQUEST to toggl.com, with TIMEOUT.
Add the auth token."
  (request (toggl-create-api-url request)
    :type "DELETE"
    ;; :parser #'buffer-string
    :headers (list (toggl-prepare-auth-header))
    :success success-fun
    :error error-fun
    :sync sync
    :timeout (or timeout toggl-default-timeout)))

(defvar toggl-projects nil
  "A list of available projects.
Each project is a cons cell with car equal to its name and cdr to
its id.")

(defun toggl-get-projects (&optional sync)
  "Fill in `toggl-projects'.

Asynchronous if SYNC is nil."
  (interactive)
  (toggl-request-get
   "me?with_related_data=true"
   sync
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-projects
            (mapcar (lambda (project)
                      (cons (substring-no-properties (alist-get 'name project))
                            (alist-get 'id project)))
                    (alist-get 'projects data)))
      (message "Toggl projects successfully downloaded.")))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (message "Fetching projects failed because %s" error-thrown)))))

(defun toggl--project-field (project-or-id field)
  "Get project FIELD value for PROJECT-OR-ID.

PROJECT-OR-ID should be an integer project ID or a project struct (list).

FIELD should be one of the keywords:
  `:id', `:name'"
  (let* ((project-id (toggl-get-project-id project-or-id))
         (project (rassoc project-id toggl-projects)))
    (pcase field
      (:name (car project))
      (:id   (cdr project)))))

(defvar toggl-default-project nil
  "Id of the default Toggl project.")

(defun toggl-select-default-project (&optional project)
  "Make PROJECT the default.
It is assumed that no two projects have the same name."
  (interactive)
  ;; Interactively prompt for the list of tags?
  (when (and (called-interactively-p)
             (not project))
    (setq project (toggl-prompt-project "Default Project: ")))

  (setq toggl-default-project (toggl-get-project-id project)))

(defun toggl-prompt-project (prompt &optional default-input)
  "Prompt user for a project from `toggl-projects', return the project's ID.

PROMPT string will be used in minibuffer prompt.

DEFAULT-INPUT, if non-nil, will be provided as the already-filled-in input to
the prompt."
  (when (null toggl-projects)
    (toggl-get-projects :sync))

  (toggl-get-project-id
   ;; Prompt user for project name, w/ completion help.
   (completing-read prompt
                    toggl-projects
                    nil
                    t
                    (if (numberp default-input)
                        (car (rassoc default-input toggl-projects))
                      default-input)
             toggl-projects
             nil
             nil
             #'toggl-string=))
;; (toggl-prompt-project "hi: " "hello")

(defvar toggl-tags nil
  "A list of available tags.
Each tag is a cons cell with car equal to its name and cdr to
its id.")

(defun toggl-get-tags (&optional sync)
  "Fill in `toggl-tags'.

Asynchronous if SYNC is nil."
  (interactive)
  ;; NOTE: Using same API endpoint as `toggl-get-projects'. Could combine both
  ;; into one REST call instead of 2 if we want?
  (toggl-request-get
   "me?with_related_data=true" ; https://developers.track.toggl.com/docs/api/me/index.html
   sync
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-tags
            ;; Save `name' and `id' of each tag in the `tags' field.
            (mapcar (lambda (tag)
                      (cons (substring-no-properties (alist-get 'name tag))
                            (alist-get 'id tag)))
                    (alist-get 'tags data)))
      (message "Toggl tags successfully downloaded.")))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (message "Fetching tags failed because %s" error-thrown)))))

(defun toggl-string= (str1 str2)
  "Compare strings STR1 and STR2 based on `toggl-case-insensitive'."
  (if toggl-case-insensitive
      (string= (upcase str1)
               (upcase str2))
    (string= str1 str2)))

(defun toggl-get-tag-id (tag)
  "Get the integer Tag ID given TAG's ID or name.

If TAG is an integer, assume it is a Tag ID and return it.
If TAG is a string, search in `toggl-tags' and return an integer or nil."
  (cond ((integerp tag)
         ;; Could still search (`rassoc' or something) in order to verify this
         ;; is a known Tag ID, but for now just assume the best and return it.
         tag)
        ((stringp tag)
         ;; Search for tag in known tags and return its ID.
         (alist-get tag
                    toggl-tags
                    nil
                    nil
                    #'toggl-string=))
        (t
         ;; Unknown/invalid input TAG.
         nil)))

(defvar toggl-default-tags nil
  "List of integer IDs of the default Toggl tags or nil.")

(defun toggl-select-default-tags (&rest tags)
  "Make TAGS the default.

It is assumed that no two tags have the same name."
  (interactive)
  ;; Interactively prompt for the list of tags?
  (when (and (called-interactively-p)
             (not tags))
    ;; `completing-read-multiple' is exactly what we want but it doesn't say
    ;; what it's separator (`crm-separator') is or how it works, which makes it
    ;; a bit noob-hostile...
    (setq tags (toggl-prompt-tags "Default Tags: ")))

  ;; Make sure they're tag IDs.
  (setq toggl-default-tags (seq-remove #'null (seq-map #'toggl-get-tag-id tags)))

  (if toggl-default-tags
      (message "Set default tags to: %s"
               (seq-map (lambda (tag) (car (rassoc tag toggl-tags)))
                        toggl-default-tags))
    (message "Cleared default tags: %s" toggl-default-tags)))

(defun toggl-prompt-tags (prompt &optional default-input)
  "Prompt user for a list of tag from `toggl-tags', return list of tag IDs.

PROMPT string or \"Toggl Tags: \" will be used in minibuffer prompt.

DEFAULT-INPUT, if non-nil, will be provided as the already-filled-in input to
the prompt."
  (when (null toggl-tags)
    (toggl-get-tags))

  ;; `completing-read-multiple' is exactly what we want but it doesn't say
  ;; what it's separator (`crm-separator') is or how it works, which makes it
  ;; a bit noob-hostile...
  (let ((tag-names (completing-read-multiple (or prompt "Toggl Tags: ")
                                             toggl-tags
                                             nil
                                             nil
                                             (if (proper-list-p default-input)
                                                 ;; Convert list of tag IDs to CSV of tag names.
                                                 (mapconcat #'identity
                                                            (seq-remove #'null
                                                                        (seq-map (lambda (tag-id)
                                                                                   (car (rassoc tag-id toggl-projects)))
                                                                                 default-input))
                                                            ", ")
                                               default-input))))
    ;; Convert names to IDs and delete any invalids.
    (seq-remove #'null (seq-map #'toggl-get-tag-id tag-names))))

(defun toggl-start-time-entry (description &optional project-id show-message)
  "Start Toggl time entry."
  (interactive "MDescription: \ni\np")
  (setq project-id (or project-id toggl-default-project))
  (toggl-request-post
   (format "workspaces/%s/time_entries" toggl-workspace-id)
   (json-encode `(("description"  . ,description)
                  ("duration"     . ,(toggl-api-duration :now))
                  ("project_id"   . ,project-id)
                  ("created_with" . "mbork's Emacs toggl client")
                  ("start"        . ,(toggl-api-timestamp nil)) ; "UTC now"
                  ("workspace_id" . ,toggl-workspace-id)))
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-current-time-entry data)
      (when show-message (message "Toggl time entry started."))))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (when show-message (message "Starting time entry failed because %s" error-thrown))))))

(defun toggl-stop-time-entry (&optional show-message)
  "Stop running Toggl time entry."
  (interactive "p")
  (when toggl-current-time-entry
    (let ((time-entry-id (alist-get 'id toggl-current-time-entry)))
      (toggl-request-patch
       (format "workspaces/%s/time_entries/%s/stop"
               toggl-workspace-id
               time-entry-id)
       (json-encode `(("time_entry_id" . ,time-entry-id)
                      ("workspace_id"  . ,toggl-workspace-id)))
       nil
       (cl-function
        (lambda (&key data &allow-other-keys)
          (when show-message (message "Toggl time entry stopped."))))
       (cl-function
        (lambda (&key error-thrown &allow-other-keys)
          (when show-message (message "Stopping time entry failed because %s" error-thrown))))))
    (setq toggl-current-time-entry nil)))

(defun toggl-delete-time-entry (&optional tid show-message)
  "Delete a Toggl time entry.
By default, delete the current one."
  (interactive "ip")
  (when toggl-current-time-entry
    (setq tid (or tid (alist-get 'id toggl-current-time-entry)))
    (toggl-request-delete
     (format "workspaces/%s/time_entries/%s" toggl-workspace-id tid)
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (when (= tid (alist-get 'id toggl-current-time-entry))
          (setq toggl-current-time-entry nil))
        (when show-message (message "Toggl time entry deleted."))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (when show-message (message "Deleting time entry failed because %s" error-thrown)))))))

(defun toggl-create-time-entry (description start duration &optional project tags show-message)
  "Create a Toggl time entry of a specific duration.

DESCRIPTION should be a string describing the time entry.

DURATION should be an integer:
  - -1 to create & start a running entry.
  - Else a positive integer for number of seconds the entry should be.

START should be:
  1. nil or empty string for \"use the default start time (`toggl-default-start-time')\"
  2. a string that `org-read-date' understands

PROJECT should be nil, an integer, or a string:
  - nil: the default project (`toggl-default-project')
  - integer: the ID of a known project (in `toggl-projects')
  - string: the name of a known project (in `toggl-projects')

TAGS should be nil, an integer, a string, or a list of integers _or_ strings:
  - nil for the default tag (`toggl-default-tags'), which can also be nil for
    'no tags'.
  - A list of just nil for 'no tags'.
  - The integer ID of an existing tag in `toggl-tags' (or list of such)
  - The tag name string of an existing tag in `toggl-tags' (or list of such)

SHOW-MESSAGE, if non-nil, will display a success/failure message based on the
status of the Toggl API call.

https://developers.track.toggl.com/docs/api/time_entries#post-timeentries"
  (interactive
   (list
    (read-string "Description: ")
    (toggl-prompt-time     "Start Time: " toggl-default-start-time)
    (toggl-prompt-duration "Duration: ")
    (toggl-prompt-project  "Project: "    toggl-default-project)
    (toggl-prompt-tags     "Tags: "       toggl-default-tags)
    ;; Always show messages unless prefix arg is present.
    (not (null current-prefix-arg))))

  (let* ((request-params (list ("workspace_id" . toggl-workspace-id))
                               ("project_id"   . (toggl-get-project-id (or project toggl-default-project)))
                               ("created_with" . "mbork's Emacs toggl client")
                               ("description"  . description)
                               ("start"        . (if (or (null start)
                                                         (and (stringp start)
                                                              (string= "" (string-trim start))))
                                                     (toggl-default-start-timestamp)
                                                   (toggl-api-timestamp start)))
                               ("duration"     . (toggl-api-duration duration)))
         (success t))

    ;;------------------------------
    ;; Normalize & add tag(s) to request if present.
    ;;------------------------------
    (cond
     ;;---
     ;; Null Cases
     ;;---
     ((eq nil tags)
      ;; No tags? If interactive, that means the user has no default tags or
      ;; deleted the default from the prompt. So they actively asked for
      ;; "nothing". So give them nothing.
      (when (and (not (called-interactively-p))
                 ;; Add the default tags if the exist.
                 (proper-list-p toggl-default-tags))
        (push `("tag_ids" . ,toggl-default-tags) request-params)))
     ((and (proper-list-p tags)
           (= 1 (length tags))
           (null (car tags)))
      ;; Explicitly no tags; nothing to do.
      )
     ;;---
     ;; (integer) Tag ID Cases
     ;;---
     ((integerp tags)
      ;; Convert to a list of tags.
      (push `("tag_ids" . (,tags)) request-params))
     ((and (proper-list-p tags)
           (seq-every-p #'integerp tags))
      (push `("tag_ids" . ,tags) request-params))
     ;;---
     ;; (string) Tag Name Cases
     ;;---
     ((stringp tags)
      ;; Convert to a list of tags.
      (if-let ((tag-id (toggl-get-tag-id tags)))
          (push `("tag_ids" . (,tag-id)) request-params)
        (setq success nil)
        (when show-message
          (message "Cannot start time entry; %S is not in the list of known tags (`toggl-tags')."
                   tags))))
     ((and (proper-list-p tags)
           (seq-every-p #'stringp tags))
      (let ((tag-ids (seq-map #'toggl-get-tag-id tags))) ; name -> id
        (if (seq-some #'null tag-ids)
            ;; Something didn't convert to an ID.
            (progn
              (setq success nil)
              (when show-message
                (message "Cannot start time entry; unknown tag (for known tags see `toggl-tags') in: %S -> %S"
                         tags
                         tag-ids)))
          ;; Ok; all valid IDs.
          (push `("tag_ids" . ,tag-ids) request-params))))
     ;;---
     ;; (error) INVALID!
     ;;---
     (t
      ;; Something invalid so don't add the tag ID(s) and message if asked.
      ;; No tags I guess?
      (setq success nil)
      (when show-message
        (message "Cannot start time entry; invalid tag(s) probably? (for known tags see `toggl-tags'): %S"
                 tags))))

    ;;------------------------------
    ;; REST API Call
    ;;------------------------------
    (when success
      (toggl-request-post
       (format "workspaces/%s/time_entries" toggl-workspace-id)
       (json-encode request-params)
       nil
       (cl-function
        (lambda (&key data &allow-other-keys)
          (setq toggl-current-time-entry data)
          (when show-message (message "Time entry created."))))
       (cl-function
        (lambda (&key error-thrown &allow-other-keys)
          (when show-message (message "[FAILURE] Creating a time entry failed because: %s" error-thrown))))))))

(defvar toggl-current-time-entry nil
  "Data of the current Toggl time entry.")

(defvar toggl-time-entries nil
  "List of data of the latest time entries.")

(defun toggl--time-entry (time-entry)
  "Get a time entry plist from `toggl-time-entries'.

TIME-ENTRY should be an integer key in `toggl-time-entries' alist,
or else it should be the actual time entry plist already."
  (cond ((plistp time-entry)
         ;; Assume this plist is a time-entry already.
         time-entry)
        ((and (proper-list-p time-entry)
              (integerp (car time-entry))
              (plistp (cdr time-entry)))
         ;; (id . plist) is the format for `toggl-time-entries', so return the time-entry's plist.
         (cdr time-entry))
        ((integerp time-entry)
         ;; Get by ID.
         (alist-get time-entry
                    toggl-time-entries))))
;; (toggl--time-entry 3228200185)
;; (toggl--time-entry (car toggl-time-entries))

(defun toggl--time-entry-field (time-entry-or-id field)
  "Get time entry FIELD value for TIME-ENTRY-OR-ID.

TIME-ENTRY-OR-ID should be an integer key in `toggl-time-entries' alist.

FIELD should be one of the keywords:
  `:description', `:start', `:stop', `:duration',
  `:project-id', `:tag-names', `:tag-ids'"
  (plist-get (cond ((plistp time-entry-or-id)
                    time-entry-or-id)
                   ((integerp time-entry-or-id)
                    (toggl--time-entry time-entry-or-id)))
             field))

;; (defun toggl--time-entry-display (time-entry-or-id)
;;   "Format TIME-ENTRY-OR-ID as a display string.

;; TIME-ENTRY-OR-ID should be an integer key in `toggl-time-entries' alist.

;; Return formatted string or nil."
;;   ;; If time entry doesn't exist, return nil.
;;   (when-let ((time-entry (cond ((plistp time-entry-or-id)
;;                                 time-entry-or-id)
;;                                ((integerp time-entry-or-id)
;;                                 (toggl--time-entry time-entry-or-id)))))

;;     (format "%s/t%s/t%s/t%s - %s/t%s"
;;             (toggl--time-entry-field time-entry :description)
;;             (toggl--time-entry-field time-entry :project-id)
;;             (toggl--time-entry-field time-entry :tag-names)
;;             (toggl--time-entry-field time-entry :start)
;;             (toggl--time-entry-field time-entry :stop)
;;             (toggl--time-entry-field time-entry :duration))
;;     )
;;   )

(defun toggl--time-entries-display (time-entries)
  "Format TIME-ENTRIES as a display strings.

TIME-ENTRIES should be a list of either:
  - integers (i.e. keys in `toggl-time-entries' alist)
  - time entry plists (i.e. values in `toggl-time-entries' alist)

Return list of formatted strings."
  (setq time-entries (seq-remove #'null (seq-map #'toggl--time-entry time-entries)))

  ;; Toggl website displays time entries sort of tabulated, like:
  ;;   > |-------------+--------------------------+------+---------------+----------|
  ;;   > | Description | Project                  | Tags | Start - End   | Duration |
  ;;   > |-------------+--------------------------+------+---------------+----------|
  ;;   > | SICK        | Sick Day                 | OOO  | 10:54 - 18:54 |  8:00:00 |
  ;;
  ;; So do something similar.

  ;;------------------------------
  ;; Figure out all the widths for the formatting.
  ;;------------------------------
  (let ((width-description 0)
        (width-project     0)
        (width-tags        0)
        (width-time        0)
        (width-duration    0)
        time-entries-display)

    (dolist (time-entry time-entries)
      ;; Build plist of display strings.
      (let ((display-fields
             (list :description (toggl--time-entry-field time-entry :description)
                   :project     (toggl--project-field (toggl--time-entry-field time-entry :project-id)
                                                      :name)
                   :tags        (string-join (toggl--time-entry-field time-entry :tag-names) ", ")
                   :start       (toggl--timestamp-local (toggl--time-entry-field time-entry :start))
                   :stop        (toggl--timestamp-local (toggl--time-entry-field time-entry :stop))
                   :duration    (toggl--duration-display (toggl--time-entry-field time-entry :duration)))))
        ;; Figure out table cell widths.
        (setq width-description (max (length (plist-get display-fields :description)) width-description)
              width-project     (max (length (plist-get display-fields :project))     width-project)
              width-tags        (max (length (plist-get display-fields :tags))        width-tags)
              width-time        (max (length (plist-get display-fields :start))       width-time)
              width-duration    (max (length (plist-get display-fields :duration))    width-duration))

        (push display-fields time-entries-display)))

    ;;------------------------------
    ;; Actually do the formatting.
    ;;------------------------------
    ;; Build our formatting string.
    (let ((time-entry-format (concat "%-" (format "%d" width-description) "s"
                                     " | "
                                     "%-" (format "%d" width-project) "s"
                                     " | "
                                     "%-" (format "%d" width-tags) "s"
                                     " | "
                                     "%" (format "%d" width-time) "s - %" (format "%d" width-time) "s"
                                     " | "
                                     "%" (format "%d" width-duration) "s"))
          formatted-display-strings)
      (dolist (display-entry time-entries-display)
        (push
         (format time-entry-format
                 (plist-get display-entry :description)
                 (plist-get display-entry :project)
                 (plist-get display-entry :tags)
                 (plist-get display-entry :start)
                 (plist-get display-entry :stop)
                 (plist-get display-entry :duration))
         formatted-display-strings))
      formatted-display-strings)))

(defun toggl-get-time-entries (&optional sync)
  "Fill in `toggl-time-entries'.

Asynchronous if SYNC is nil."
  (interactive)
  (toggl-request-get
   "me?with_related_data=true"
   sync
   (cl-function
    (lambda (&key data &allow-other-keys)
      (setq toggl-time-entries
            (mapcar (lambda (time-entry)
                      ;; Need more than just "Name & ID" for time entries.
                      (cons
                      ;; alist by ID; rest of stuff in plist?
                       (alist-get 'id time-entry)
                       (list
                        :description (substring-no-properties (alist-get 'description time-entry))
                        :start       (alist-get 'start      time-entry)
                        :stop        (alist-get 'stop       time-entry)
                        :duration    (alist-get 'duration   time-entry)
                        :project-id  (alist-get 'project_id time-entry)
                        :tag-names   (alist-get 'tags       time-entry)
                        :tag-ids     (alist-get 'tag_ids    time-entry))))
                    (alist-get 'time_entries data)))
      (message "Toggl time entries successfully downloaded.")))
   (cl-function
    (lambda (&key error-thrown &allow-other-keys)
      (message "Fetching time entries failed because %s" error-thrown)))))

(defun toggl--parse-timestamp (timestamp)
  "Parse TIMESTAMP sting into a time struct.

Return time struct like `decode-time':
  (SECS MINS HOURS DAY MONTH YEAR DOW DST UTCOFF)"
  (iso8601-parse timestamp))

(defun toggl--timestamp-local (time)
  "Return timestamp string of TIME in local time.

TIME should be a timestamp string or a time struct."
  (format-time-string "%FT%T%z"
                      (cond
                       ;; "Lisp time" like from `current-time'.
                       ((and (proper-list-p time)
                             ;; Cover both of these time "forms":
                             ;; (current-time)
                             ;;   -> (25962 14924 669578 837000)
                             ;; (encode-time (decode-time (current-time)))
                             ;;   -> (25962 14925)
                             (memq (length time) '(2 4)))
                        ;; `format-time-string' understands this input format.
                        time)
                       ;; "Calendrical information" like from `decode-time', `toggl--parse-timestamp'.
                       ((and (proper-list-p time)
                             (= 9 (length time)))
                        ;; `format-time-string' doesn't understand; convert to "lisp time".
                        (encode-time time))
                       ;; Timestamp string needs parsed so it can be timezone converted.
                       ((stringp time)
                        (encode-time (toggl--parse-timestamp time))))
                      ;; nil == "convert to local time"
                      ;; t == "convert to UTC"
                      nil))

(defun toggl-get-project-id (project)
  "Get the integer Project ID given PROJECT's ID or name.

If PROJECT is an integer, assume it is a Project ID and return it.
If PROJECT is a string, search in `toggl-projects' and return an integer or nil."
  (cond ((integerp project)
         ;; Could still search (`rassoc' or something) in order to verify this
         ;; is a known Project ID, but for now just assume the best and return it.
         project)
        ((stringp project)
         ;; Search for project in known projects and return its ID.
         (alist-get project
                    toggl-projects
                    nil
                    nil
                    #'toggl-string=))
        (t
         ;; Unknown/invalid input PROJECT.
         nil)))

(defcustom org-toggl-inherit-toggl-properties nil
  "Make org-toggl use property inheritance."
  :type 'boolean
  :group 'toggl)

(defun org-toggl-clock-in ()
  "Start a Toggl time entry based on current heading."
  (interactive)
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
         (project (org-entry-get (point) "toggl-project" org-toggl-inherit-toggl-properties))
         (pid (toggl-get-project-id project)))
    (when pid (toggl-start-time-entry heading pid t))))

(defun org-toggl-clock-out ()
  "Stop the running Toggle time entry."
  (toggl-stop-time-entry t))

(defun org-toggl-clock-cancel ()
  "Delete the running Toggle time entry."
  (toggl-delete-time-entry nil t))

(defun org-toggl-set-project (project)
  "Save PROJECT in the properties of the current Org headline."
  (interactive (list (completing-read "Toggl project for this headline: " toggl-projects nil t))) ; TODO: dry!
  (org-set-property "toggl-project" project))

(defun org-toggl-set-tag (tag)
  "Save TAG in the properties of the current Org headline."
  (interactive (list (completing-read "Toggl tag for this headline: " toggl-tags nil t))) ; TODO: dry!
  (org-set-property "toggl-tag" tag))

(defun org-toggl-submit-clock-at-point (&optional show-message)
  "Submit the clock entry at point to Toggl."
  (interactive "p")
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'clock)
        (let* ((heading (substring-no-properties (org-get-heading t t t t)))
               (project (org-entry-get (point) "toggl-project" org-toggl-inherit-toggl-properties))
               (pid (or (toggl-get-project-id project) toggl-default-project))
               (timestamp (org-element-property :value element))
               (year-start (org-element-property :year-start timestamp))
               (month-start (org-element-property :month-start timestamp))
               (day-start (org-element-property :day-start timestamp))
               (hour-start (org-element-property :hour-start timestamp))
               (minute-start (org-element-property :minute-start timestamp))
               (year-end (org-element-property :year-end timestamp))
               (month-end (org-element-property :month-end timestamp))
               (day-end (org-element-property :day-end timestamp))
               (hour-end (org-element-property :hour-end timestamp))
               (minute-end (org-element-property :minute-end timestamp))
               (start-time (time-to-seconds (encode-time
                                             0
                                             minute-start
                                             hour-start
                                             day-start
                                             month-start
                                             year-start)))
               (stop-time (time-to-seconds (encode-time
                                            0
                                            minute-end
                                            hour-end
                                            day-end
                                            month-end
                                            year-end))))
          (toggl-request-post
           (format "workspaces/%s/time_entries" toggl-workspace-id)
           (json-encode `(("description"  . ,heading)
                          ("project_id"   . ,pid)
                          ("created_with" . "mbork's Emacs toggl client")
                          ("start"        . ,(toggl-api-timestamp start-time))
                          ("stop"         . ,(toggl-api-timestamp stop-time ))
                          ("workspace_id" . ,toggl-workspace-id)))
           nil
           (cl-function
            (lambda (&key data &allow-other-keys)
              (setq toggl-current-time-entry data)
              (when show-message (message "Toggl time entry submitted."))))
           (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (when show-message (message "Starting time entry failed because %s" error-thrown))))))
      (error "No clock at point"))))

(define-minor-mode org-toggl-integration-mode
  "Toggle a (global) minor mode for Org/Toggl integration.
When on, clocking in and out starts and stops Toggl time entries
automatically."
  :init-value nil
  :global t
  :lighter " T-O"
  (if org-toggl-integration-mode
      (progn
        (add-hook 'org-clock-in-hook #'org-toggl-clock-in)
        (add-hook 'org-clock-out-hook #'org-toggl-clock-out)
        (add-hook 'org-clock-cancel-hook #'org-toggl-clock-cancel))
    (remove-hook 'org-clock-in-hook #'org-toggl-clock-in)
    (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
    (remove-hook 'org-clock-cancel-hook #'org-toggl-clock-cancel)))

(provide 'org-toggl)
;;; org-toggl.el ends here
