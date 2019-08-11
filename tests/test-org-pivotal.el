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
      (expect project-id :to-equal 12345678))))

(provide 'test-org-pivotal)

;;; test-org-pivotal.el ends here
