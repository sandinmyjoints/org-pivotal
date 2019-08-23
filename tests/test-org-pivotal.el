;;; test-org-pivotal.el --- Tests for org-pivotal.el

;; Copyright (C) 2018 Huy Duong

;; Author: Huy Duong <qhuyduong@hotmail.com>

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

;; Tests for org-pivotal.el

;;; Code:

(require 'buttercup)
(require 'org-pivotal)

(describe "org-pivotal"
  (describe "org-pivotal--select-project"
    :var (project-id)
    (before-each
      (spy-on 'completing-read :and-return-value "Test project 1")
      (setq project-id
            (org-pivotal--select-project
             '[((project_id . 12345678)
                (project_name . "Test project 1"))
               ((project_id . 87654321)
                (project_name . "Test project 2"))])))

    (it "calls completing-read with proper project list"
      (expect 'completing-read :to-have-been-called-with "Select your project?" '("Test project 1" "Test project 2")))

    (it "returns correct project_id"
      (expect project-id :to-equal 12345678)))

  (describe "org-pivotal--update-buffer-with-metadata"
    :var (project my-info)
    (before-each
      (setq project '((id . 12345678) (name . "Test project 1")))
      (setq my-info '((id . 87654321)))
      (spy-on 'set-buffer-file-coding-system))

    (it "sets buffer file encoding system to utf-8"
      (with-temp-buffer
        (org-pivotal--update-buffer-with-metadata project my-info)
        (expect 'set-buffer-file-coding-system :to-have-been-called-with 'utf-8-auto)))

    (it "sets buffer's major mode to org-mode"
      (with-temp-buffer
        (org-pivotal--update-buffer-with-metadata project my-info)
        (expect major-mode :to-equal 'org-mode)))

    (it "replaces buffer content with project's metadata"
      (with-temp-buffer
        (insert "Hahahahaaha")
        (org-pivotal--update-buffer-with-metadata project my-info)
        (expect (buffer-string)
                :to-equal
":PROPERTIES:
#+PROPERTY: project-name Test project 1
#+PROPERTY: project-id 12345678
#+PROPERTY: url https://www.pivotaltracker.com/n/projects/12345678
#+PROPERTY: my-id 87654321
#+PROPERTY: filter owner:87654321 AND (-state:accepted AND -state:rejected)
#+TODO: Unstarted Started Finished Delivered | Accepted Rejected
:END:
"
                ))))

  (describe "org-pivotal-install-project-metadata"
    :var (my-info)
    (before-each
      (setq my-info '((projects . [((project_id . 12345678)
                                    (project_name . "Test project 1"))
                                   ((project_id . 87654321)
                                    (project_name . "Test project 2"))])
                      (name . "Huy Duong")
                      (age . "18")))
      (spy-on 'org-pivotal-api--get-my-info :and-return-value my-info)
      (spy-on 'org-pivotal--select-project :and-return-value 12345678)
      (spy-on 'org-pivotal-api--get-project-info
              :and-return-value '((project_id . 12345678)
                                  (project_name . "Test project 1")))
      (spy-on 'org-pivotal--update-buffer-with-metadata)
      (org-pivotal-install-project-metadata))

    (it "calls API to get user info"
      (expect 'org-pivotal-api--get-my-info :to-have-been-called))

    (it "informs user to select project"
      (expect 'org-pivotal--select-project
              :to-have-been-called-with
              (alist-get 'projects my-info)))

    (it "calls API to get project info"
      (expect 'org-pivotal-api--get-project-info :to-have-been-called-with 12345678))

    (it "updates buffer with "
      (expect 'org-pivotal--update-buffer-with-metadata
              :to-have-been-called-with
              '((project_id . 12345678)
                (project_name . "Test project 1"))
              my-info)))

  (describe "org-pivotal--convert-story-to-headline"
    :var (story)
    (before-each
      (setq story '((name . "Test story")
                    (id . 25251325)
                    (current_state . "accepted")
                    (story_type . "chore")
                    (estimate . 2)
                    (url . "https://www.pivotaltracker.com/story/show/25251325")
                    (description . "This is a test story")
                    (updated_at . "2019-08-23T08:04:53Z")
                    (labels . (((name . "label 1")) ((name . "label 2")) ((name . "label 3")))))))

    (it "appends story to buffer"
      (with-temp-buffer
        (insert ":PROPERTIES:\n#+PROPERTY: project-name Test project 1\n#+PROPERTY: project-id 12345678\n:END:\n")
        (org-pivotal--convert-story-to-headline story)
        (expect (buffer-string)
                :to-equal
":PROPERTIES:
#+PROPERTY: project-name Test project 1
#+PROPERTY: project-id 12345678
:END:
* Accepted Test story
:PROPERTIES:
:ID: 25251325
:Type: Chore
:Points: 2
:Updated: 2019-08-23T08:04:53Z
:URL: https://www.pivotaltracker.com/story/show/25251325
:Description: This is a test story
:Labels: \"label 1\" \"label 2\" \"label 3\"
:END:
"
                )))))

(provide 'test-org-pivotal)

;;; test-org-pivotal.el ends here
