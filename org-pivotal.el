;;; org-pivotal.el --- Minor mode to extend org-mode with Pivotal Tracker -*- lexical-binding: t; -*-

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

;; org-pivotal is an Emacs minor mode to extend org-mode with Pivotal Tracker abilities

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'ido)
(require 'json)
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
    "Base APIv5 URL."
    :group 'org-pivotal
    :type 'string)

  (defconst org-pivotal-transition-states
    '("UNSCHEDULED" "UNSTARTED" "PLANNED" "STARTED" "FINISHED" "DELIVERED" "|" "ACCEPTED" "REJECTED")
    "Story status will be one of these values."))

(defun org-pivotal-api-url-generator (&rest parts-of-url)
  "Build a Pivotal API URL from PARTS-OF-URL."
  (apply 'concat org-pivotal-api-base-url
         (-map (lambda (part) (concat "/" part)) parts-of-url)))

(defun org-pivotal-api-call (url method)
  "Access wrapper for the Pivotal (v5) JSON API.
URL of the API endpoint
METHOD to use."
  (funcall (-compose '(lambda (response)
                        (request-response-data response))
                     '(lambda (url method)
                        (request url
                                 :sync t
                                 :type method
                                 :headers `(("X-TrackerToken" . ,org-pivotal-api-token)
                                            ("Content-Type" . "application/json"))
                                 :parser 'json-read)))
           url method))

(defun org-pivotal-get-projects ()
  "Get all of a user's active projects."
  (org-pivotal-api-call
   (org-pivotal-api-url-generator "projects") "GET"))

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
                                (list (alist-get 'name project) (alist-get 'id project)))
                              projects)))
           projects
           ))

(defun org-pivotal-get-project-info (project-id)
  "Get PROJECT-ID's project info."
  (org-pivotal-api-call
   (org-pivotal-api-url-generator "projects"
                                  (number-to-string project-id))
   "GET"))

(defun org-pivotal-update-buffer-with-metadata (project)
   "Update org buffer with metadata from PROJECT."
   (with-current-buffer (current-buffer)
     (org-mode)
     (goto-char (point-min))
     (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
     (-map (lambda (item) (insert item "\n"))
           (list ":PROPERTIES:"
                 (format "#+PROPERTY: project-name %s" (cdr (assoc 'name project)))
                 (format "#+PROPERTY: project-id %d" (cdr (assoc 'id project)))
                 (format "#+PROPERTY: velocity %d" (cdr (assoc 'velocity_averaged_over project)))
                 (format "#+PROPERTY: url %s/n/projects/%d" org-pivotal-base-url (cdr (assoc 'id project)))
                 (format "#+TODO: %s" (string-join org-pivotal-transition-states " "))
                 ":END:"
                 ))
     (goto-char (point-min))
     (org-cycle)))

;;;###autoload
(defun org-pivotal-install-project-metadata ()
  "Install selected project's metadata to buffer."
  (interactive)
  (funcall (-compose 'org-pivotal-update-buffer-with-metadata
                     'org-pivotal-get-project-info
                     'org-pivotal-select-project
                     'org-pivotal-get-projects)))

(provide 'org-pivotal)

;;; org-pivotal.el ends here
