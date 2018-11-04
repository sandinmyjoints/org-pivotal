;;; org-pivotal.el --- Utility to sync Pivotal Tracker to org buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huy Duong

;; Author: Huy Duong <qhuyduong@hotmail.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; org-pivotal is a utility to sync Pivotal Tracker to org buffer

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'ido)
(require 'json)
(require 'org)
(require 'request)
(require 'subr-x)

;;;###autoload
(progn
  (defgroup org-pivotal nil
    "Pivotal Tracker."
    :group 'external)

  (defcustom org-pivotal-api-base-url "https://www.pivotaltracker.com/services/v5"
    "Base APIv5 URL."
    :group 'org-pivotal
    :type 'string)

  (defcustom org-pivotal-api-token nil
    "API key found on the /profile page of pivotal tracker."
    :group 'org-pivotal
    :type 'string)

  (defcustom org-pivotal-base-url "https://www.pivotaltracker.com"
    "Base URL."
    :group 'org-pivotal
    :type 'string)

  (defconst org-pivotal-transition-states
    '("Unstarted" "Started" "Finished" "Delivered" "|" "Accepted" "Rejected")
    "Story status will be one of these values."))

(defun org-pivotal-api-url-generator (&rest parts-of-url)
  "Build a Pivotal API URL from PARTS-OF-URL."
  (apply 'concat org-pivotal-api-base-url
         (-map (lambda (part) (concat "/" part)) parts-of-url)))

(defun org-pivotal-api-call (url method &optional query data)
  "Access wrapper for the Pivotal (v5) JSON API.
URL of the API endpoint
METHOD to use
QUERY params
DATA data."
  (funcall (-compose '(lambda (response)
                        (request-response-data response))
                     '(lambda (url method query data)
                        (request url
                                 :data (if data (json-encode data) nil)
                                 :headers `(("X-TrackerToken" . ,org-pivotal-api-token)
                                            ("Content-Type" . "application/json"))
                                 :params query
                                 :parser 'json-read
                                 :sync t
                                 :type method)))
           url method query data))

(defun org-pivotal-select-project (projects)
  "Prompt user to select a project from PROJECTS."
  (funcall (-compose '(lambda (projects)
                        (let ((ido-max-window-height (1+ (length projects))))
                          (cadr (assoc
                                 (ido-completing-read "Select your project?"
                                                      (-map 'car projects))
                                 projects))))
                     '(lambda (projects)
                        (-map (lambda (project)
                                (list (alist-get 'project_name project)
                                      (alist-get 'project_id project)))
                              projects)))
           projects))

(defun org-pivotal-get-project-info (project-id)
  "Get PROJECT-ID's project info."
  (org-pivotal-api-call
   (org-pivotal-api-url-generator "projects"
                                  (number-to-string project-id))
   "GET"))

(defun org-pivotal-get-my-info ()
  "Get my Pivotal User ID."
  (org-pivotal-api-call (org-pivotal-api-url-generator "me") "GET"))

(defun org-pivotal-update-buffer-with-metadata (project my-info)
  "Update org buffer with metadata from PROJECT and MY-INFO."
  (with-current-buffer (current-buffer)
    (erase-buffer)
    (org-mode)
    (org-indent-mode)
    (goto-char (point-min))
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (-map (lambda (item) (insert item "\n"))
          (list ":PROPERTIES:"
                (format "#+PROPERTY: project-name %s" (alist-get 'name project))
                (format "#+PROPERTY: project-id %d" (alist-get 'id project))
                (format "#+PROPERTY: velocity %d" (alist-get 'velocity_averaged_over project))
                (format "#+PROPERTY: url %s/n/projects/%d" org-pivotal-base-url (alist-get 'id project))
                (format "#+PROPERTY: my-id %d" (alist-get 'id my-info))
                (format "#+TODO: %s" (string-join org-pivotal-transition-states " "))
                ":END:"))
    (call-interactively 'save-buffer))
  (org-set-regexps-and-options))

;;;###autoload
(defun org-pivotal-install-project-metadata ()
  "Install selected project's metadata to buffer."
  (interactive)
  (let ((my-info (org-pivotal-get-my-info)))
    (let ((project (funcall (-compose 'org-pivotal-get-project-info
                                      'org-pivotal-select-project)
                            (alist-get 'projects my-info))))
      (org-pivotal-update-buffer-with-metadata project my-info))))

(defun org-pivotal-get-stories (project-id &optional filter)
  "Get stories from PROJECT-ID's project with FILTER."
  (org-pivotal-api-call
   (org-pivotal-api-url-generator "projects" project-id "stories")
   "GET"
   (if filter (list (cons "filter" filter)))))

(defun org-pivotal-convert-story-to-heading (story)
  "Convert STORY to org heading."
  (-map (lambda (item)
          (insert item "\n")
          (org-indent-line))
        (list (format "* %s %s"
                      (upcase-initials (alist-get 'current_state story))
                      (alist-get 'name story))
              ":PROPERTIES:"
              (format ":ID: %s" (alist-get 'id story))
              (format ":Type: %s" (upcase-initials (alist-get 'story_type story)))
              (format ":Points: %s" (alist-get 'estimate story))
              (format ":Updated: %s" (alist-get 'updated_at story))
              (format ":URL: %s" (alist-get 'url story))
              (format ":Description: %s" (alist-get 'description story))
              (format ":Labels: %s" (string-join
                                     (-map (lambda (label) (format "\"%s\""(alist-get 'name label)))
                                           (alist-get 'labels story))
                                     " "))
              ":END:")))

(defun org-pivotal-update-buffer-with-stories (stories)
  "Update org buffer with STORIES."
  (with-current-buffer (current-buffer)
    (org-mode)
    (org-indent-mode)
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (goto-char (point-min))
    (outline-next-heading)
    (kill-region (point-at-bol) (point-max))
    (-map 'org-pivotal-convert-story-to-heading stories)
    (call-interactively 'save-buffer))
  (org-set-regexps-and-options))

;;;###autoload
(defun org-pivotal-pull-stories ()
  "Pull stories to org buffer."
  (interactive)
  (org-set-regexps-and-options)
  (funcall (-compose 'org-pivotal-update-buffer-with-stories
                     'org-pivotal-get-stories)
           (cdr (assoc-string "project-id" org-file-properties))
           (cdr (assoc-string "filter" org-file-properties))))

(provide 'org-pivotal)

;;; org-pivotal.el ends here
