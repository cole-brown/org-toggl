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

(defvar toggl-current-time-entry nil
  "Data of the current Toggl time entry.")

(defun toggl-get-projects ()
  "Fill in `toggl-projects' (asynchronously)."
  (interactive)
  (toggl-request-get
   "me?with_related_data=true"
   nil
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

(defvar toggl-default-project nil
  "Id of the default Toggl project.")

(defun toggl-select-default-project (project)
  "Make PROJECT the default.
It is assumed that no two projects have the same name."
  (interactive (list (completing-read "Default project: " toggl-projects nil t)))
  (setq toggl-default-project (toggl-get-project-id project)))

(defvar toggl-tags nil
  "A list of available tags.
Each tag is a cons cell with car equal to its name and cdr to
its id.")

(defun toggl-get-tags ()
  "Fill in `toggl-tags' (asynchronously)."
  (interactive)
  ;; NOTE: Using same API endpoint as `toggl-get-projects'. Could combine both
  ;; into one REST call instead of 2 if we want?
  (toggl-request-get
   "me?with_related_data=true" ; https://developers.track.toggl.com/docs/api/me/index.html
   nil
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
    (setq tags (completing-read-multiple "Default Tags: "
                                         toggl-tags
                                         nil
                                         nil)))
  (let ((tag-ids (seq-remove #'null (seq-map #'toggl-get-tag-id tags))))
    (if tag-ids
        (message "Set default tags to: %s"
                 (seq-map (lambda (tag) (car (rassoc tag toggl-tags)))
                          tag-ids))
      (message "Cleared default tags: %s" tag-ids))
    (setq toggl-default-tags tag-ids)))

(defun toggl-start-time-entry (description &optional project-id show-message)
  "Start Toggl time entry."
  (interactive "MDescription: \ni\np")
  (setq project-id (or project-id toggl-default-project))
  (toggl-request-post
   (format "workspaces/%s/time_entries" toggl-workspace-id)
   (json-encode `(("description" . ,description)
                  ("duration" . -1)
                  ("project_id" . ,project-id)
                  ("created_with" . "mbork's Emacs toggl client")
                  ("start" . ,(format-time-string "%FT%TZ" nil t))
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
                      ("workspace_id" . ,toggl-workspace-id)))
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

(defun toggl-set-time-entry (description duration &optional project tags show-message)
  "Start Toggl time entry.

DESCRIPTION should be a string describing the time entry.

DURATION should be an integer:
  - -1 to create & start a running entry.
  - Else a positive integer for number of seconds the entry should be.

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
status of the Toggl API call."
  (interactive "MDescription: \ni\ni\np")
  (let* ((project-id (or project-id toggl-default-project))
         (request-params (list ("description" . description)
                               ("duration" . duration)
                               ("project_id" . project-id)
                               ("created_with" . "mbork's Emacs toggl client")
                               ("start" . (format-time-string "%FT%TZ" nil t))
                               ("workspace_id" . toggl-workspace-id)))
         (success t))
    ;;------------------------------
    ;; Normalize & add tag(s) to request if present.
    ;;------------------------------
    (cond
     ;;---
     ;; Null Cases
     ;;---
     ((eq nil tags)
      ;; Add the default tags if the exist.
      (unless (proper-list-p toggl-default-tags)
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
      (let ((tag-ids (seq-map #'toggl-get-tag-id tags))) ;; name -> id
        (if (seq-some #'null tag-ids)
            ;; Something didn't convert to an ID.
            (progn
              (setq success nil)
              (when show-message
                (message "Cannot start time entry; %S is not in the list of known tags (`toggl-tags')."
                         tags)))
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
        (message "Cannot start time entry because of invalid tag(s): %S" tags))))

    ;;------------------------------
    ;; REST API Call
    ;;------------------------------
    (when success
      (toggl-request-post
       (format "workspaces/%s/time_entries" toggl-workspace-id)
       (json-encode )
       nil
       (cl-function
        (lambda (&key data &allow-other-keys)
          (setq toggl-current-time-entry data)
          (when show-message (message "Toggl time entry started."))))
       (cl-function
        (lambda (&key error-thrown &allow-other-keys)
          (when show-message (message "Starting time entry failed because %s" error-thrown))))))))

(defun toggl-get-project-id (project &optional case-sensitive)
  "Get the integer Project ID given PROJECT's ID or name.

If PROJECT is an integer, assume it is a Project ID and return it.
If PROJECT is a string, search in `toggl-projects' and return an integer or nil.

If CASE-SENSITIVE is non-nil, ignore case when finding the match to PROJECT."
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
           (json-encode `(("description" . ,heading)
                          ("project_id" . ,pid)
                          ("created_with" . "mbork's Emacs toggl client")
                          ("start" . ,(format-time-string "%FT%TZ" start-time t))
                          ("stop" . ,(format-time-string "%FT%TZ" stop-time t))
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
