;;; org-pivotal-test.el --- Tests for org-pivotal.el

;; Copyright (C) 2013 Huy Duong

;; Author: Huy Duong <qhuyduong@Huys-MBP>

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

(require 'el-mock)
(require 'ert)
(require 'org-pivotal)

(ert-deftest org-pivotal-api-url-generator-test ()
  (should (equal (org-pivotal-api-url-generator "projects" "12345678")
                 "https://www.pivotaltracker.com/services/v5/projects/12345678")))

(ert-deftest org-pivotal-api-call-test ()
  (let ((response [((id . "12345678") (name . "Test project"))]))
    (should (equal response
                   (with-mock
                    (mock (request "http://server/some-uri"
                                   :sync t
                                   :type "GET"
                                   :headers `(("X-TrackerToken" . :org-pivotal-api-token)
                                              ("Content-Type" . "application/json"))
                                   :parser 'json-read)
                          => (record 'request-response 200 nil response))
                    (let ((url "http://server/some-uri")
                          (org-pivotal-api-token :org-pivotal-api-token))
                      (org-pivotal-api-call url "GET")))))))

(ert-deftest org-pivotal-get-projects-test ()
  (should (equal '("Test project")
                 (with-mock
                  (mock (org-pivotal-api-url-generator "projects")
                        => "http://server/some-uri")
                  (mock (org-pivotal-api-call "http://server/some-uri" "GET")
                        => [((id . "12345678") (name . "Test project"))])
                  (org-pivotal-get-projects)))))

(ert-deftest org-pivotal-set-project-test ()
  (should (equal :res-with-ido
                 (let ((org-trello-input-completion-mechanism 'default))
                   (with-mock
                    (mock (ido-completing-read :prompt :choices nil 'do-match) => :res-with-ido)
                    (orgtrello-input-read-string-completion :prompt :choices))))))

(provide 'org-pivotal-test)

;;; org-pivotal-test.el ends here
