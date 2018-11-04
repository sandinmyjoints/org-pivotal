;;; org-pivotal-api-test.el --- Tests for org-pivotal-api.el

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

(require 'el-mock)
(require 'ert)
(require 'org-pivotal-api)

(ert-deftest org-pivotal-api--url-generator-test ()
  (should (equal (org-pivotal-api--url-generator "projects" "12345678")
                 "https://www.pivotaltracker.com/services/v5/projects/12345678")))

(ert-deftest org-pivotal-api--call-test ()
  (setq org-pivotal-api-token :org-pivotal-api-token)
  ;; Test API GET
  (should (equal :response
                 (with-mock
                  (mock (request "https://www.pivotaltracker.com"
                                 :data nil
                                 :headers `(("X-TrackerToken" . :org-pivotal-api-token)
                                            ("Content-Type" . "application/json"))
                                 :params '((:filter . "owner:123456"))
                                 :parser 'json-read
                                 :sync t
                                 :type "GET")
                        => (record 'request-response 200 nil :response nil))
                  (org-pivotal-api--call
                   "https://www.pivotaltracker.com"
                   "GET"
                   '((:filter . "owner:123456"))))))
  ;; Test API PUT
  (should (equal :response
                 (with-mock
                  (mock (request "https://www.pivotaltracker.com"
                                 :data "{\"key1\":\"value1\",\"key2\":\"value2\"}"
                                 :headers `(("X-TrackerToken" . :org-pivotal-api-token)
                                            ("Content-Type" . "application/json"))
                                 :params nil
                                 :parser 'json-read
                                 :sync t
                                 :type "PUT")
                        => (record 'request-response 200 nil :response nil))
                  (org-pivotal-api--call
                   "https://www.pivotaltracker.com"
                   "PUT"
                   nil
                   '((:key1 . "value1") (:key2 . "value2"))))))
  ;; Test API call failure
  (should (equal nil
                 (with-mock
                  (mock (request "https://www.pivotaltracker.com"
                                 :data nil
                                 :headers `(("X-TrackerToken" . :org-pivotal-api-token)
                                            ("Content-Type" . "application/json"))
                                 :params nil
                                 :parser 'json-read
                                 :sync t
                                 :type "GET")
                        => (record 'request-response
                                  200
                                  nil
                                  nil
                                  '((:error . "Some error."))))
                  (org-pivotal-api--call
                   "https://www.pivotaltracker.com"
                   "GET")))))

(ert-deftest org-pivotal-api--get-project-info-test ()
  (should (equal '((:id . 12345678)
                   (:name . "Test project"))
                 (with-mock
                  (mock (org-pivotal-api--url-generator "projects" "12345678")
                        => "https://www.pivotaltracker.com")
                  (mock (org-pivotal-api--call "https://www.pivotaltracker.com" "GET")
                        => '((:id . 12345678) (:name . "Test project")))
                  (org-pivotal-api--get-project-info 12345678)))))

(ert-deftest org-pivotal-api--get-my-info-test ()
  (should (equal '((:id . 12345678)
                   (:name . "Huy Duong"))
                 (with-mock
                  (mock (org-pivotal-api--url-generator "me")
                        => "https://www.pivotaltracker.com/me")
                  (mock (org-pivotal-api--call "https://www.pivotaltracker.com/me" "GET")
                        => '((:id . 12345678) (:name . "Huy Duong")))
                  (org-pivotal-api--get-my-info)))))

(ert-deftest org-pivotal-api--get-stories-test ()
  ;; Get all stories
  (should (equal '[((:id . 12345678) (:name . "Test story 1"))
                   ((:id . 87654321) (:name . "Test story 2"))
                   ((:id . 12344321) (:name . "Test story 3"))]
                 (with-mock
                  (mock (org-pivotal-api--url-generator "projects" "11111111" "stories")
                        => "https://www.pivotaltracker.com/projects/11111111/stories")
                  (mock (org-pivotal-api--call
                         "https://www.pivotaltracker.com/projects/11111111/stories"
                         "GET"
                         nil)
                        => '[((:id . 12345678) (:name . "Test story 1"))
                            ((:id . 87654321) (:name . "Test story 2"))
                            ((:id . 12344321) (:name . "Test story 3"))])
                  (org-pivotal-api--get-stories 11111111))))
  ;; Get filtered stories
  (should (equal '[((:id . 12345678) (:name . "Test story 1"))
                   ((:id . 87654321) (:name . "Test story 2"))]
                 (with-mock
                  (mock (org-pivotal-api--url-generator "projects" "11111111" "stories")
                        => "https://www.pivotaltracker.com/projects/11111111/stories")
                  (mock (org-pivotal-api--call
                         "https://www.pivotaltracker.com/projects/11111111/stories"
                         "GET"
                         '((:filter . "owner:22222222")))
                        => '[((:id . 12345678) (:name . "Test story 1"))
                            ((:id . 87654321) (:name . "Test story 2"))])
                  (org-pivotal-api--get-stories 11111111 "owner:22222222")))))

(provide 'org-pivotal-api-test)

;;; org-pivotal-api-test.el ends here
