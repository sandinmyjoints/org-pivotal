;;; test-org-pivotal-api.el --- Tests for org-pivotal-api.el

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

;; Tests for org-pivotal-api.el

;;; Code:

(require 'buttercup)
(require 'org-pivotal-api)

(describe "org-pivotal-api"
  (describe "org-pivotal-api--url-generator"
    (it "generates correct URL"
      (expect (org-pivotal-api--url-generator "projects" "12345678")
              :to-equal
              "https://www.pivotaltracker.com/services/v5/projects/12345678")))

  (describe "org-pivotal-api--call"
    :var (result)
    (before-each
      (setq org-pivotal-api-token "this-is-a-fake-token")
      (spy-on 'request :and-return-value (record 'request-response 200 nil "Hello Pivotal")))

    (describe "when data is not provided"
      (before-each
        (setq org-pivotal-api-token "this-is-a-fake-token")
        (spy-on 'request :and-return-value (record 'request-response 200 nil "Hello Pivotal"))
        (setq result (org-pivotal-api--call "https://www.pivotaltracker.com"
                                            "GET"
                                            '(("filter" . "owner:123456")))))

      (it "sends correct request"
        (expect 'request
                :to-have-been-called-with
                "https://www.pivotaltracker.com"
                :data nil
                :headers `(("X-TrackerToken" . "this-is-a-fake-token")
                           ("Content-Type" . "application/json"))
                :params '(("filter" . "owner:123456"))
                :parser 'json-read
                :sync t
                :type "GET")))

    (describe "when data is provided"
      :var (result)
      (before-each
        (setq org-pivotal-api-token "this-is-a-fake-token")
        (spy-on 'request :and-return-value (record 'request-response 200 nil "Hello Pivotal"))
        (setq result (org-pivotal-api--call "https://www.pivotaltracker.com"
                                            "GET"
                                            '(("filter" . "owner:123456"))
                                            "some-data")))

      (it "sends correct request"
        (expect 'request
                :to-have-been-called-with
                "https://www.pivotaltracker.com"
                :data "\"some-data\""
                :headers `(("X-TrackerToken" . "this-is-a-fake-token")
                           ("Content-Type" . "application/json"))
                :params '(("filter" . "owner:123456"))
                :parser 'json-read
                :sync t
                :type "GET")))

    (it "returns correct data"
      (expect result :to-equal "Hello Pivotal")))

  (describe "org-pivotal-api--get-project-info"
    (before-each
      (spy-on 'org-pivotal-api--call))

    (describe "when project-id is string"
      (before-each
        (org-pivotal-api--get-project-info "123456"))

      (it "sends correct URL and request method to API call"
        (expect 'org-pivotal-api--call
                :to-have-been-called-with
                "https://www.pivotaltracker.com/services/v5/projects/123456"
                "GET")))

    (describe "when project-id is number"
      (before-each
        (org-pivotal-api--get-project-info 123456))

      (it "sends correct URL and request method to API call"
        (expect 'org-pivotal-api--call
                :to-have-been-called-with
                "https://www.pivotaltracker.com/services/v5/projects/123456"
                "GET"))))

  (describe "org-pivotal-api--get-my-info"
    (before-each
      (spy-on 'org-pivotal-api--call)
      (org-pivotal-api--get-my-info))

    (it "sends correct URL and request method to API call"
      (expect 'org-pivotal-api--call
              :to-have-been-called-with
              "https://www.pivotaltracker.com/services/v5/me"
              "GET")))

  (describe "org-pivotal-api--fetch-stories"
    (before-each
      (spy-on 'org-pivotal-api--call))

    (describe "when filter is not provided"
      (before-each
        (org-pivotal-api--fetch-stories "123456"))

      (it "sends correct URL and request method to API call"
        (expect 'org-pivotal-api--call
                :to-have-been-called-with
                "https://www.pivotaltracker.com/services/v5/projects/123456/stories"
                "GET"
                nil)))

    (describe "when filter is provided"
      (before-each
        (org-pivotal-api--fetch-stories "123456" "not-assigned-to-me"))

      (it "sends correct URL, request method and query to API call"
        (expect 'org-pivotal-api--call
                :to-have-been-called-with
                "https://www.pivotaltracker.com/services/v5/projects/123456/stories"
                "GET"
                '(("filter" . "not-assigned-to-me"))))))

  (describe "org-pivotal-api--store-story"
    (before-each
      (spy-on 'org-pivotal-api--call)
      (org-pivotal-api--store-story "123456" '(("id" . "111111")
                                               ("title" . "write-test"))))

      (it "sends correct URL, request method and data to API call"
        (expect 'org-pivotal-api--call
                :to-have-been-called-with
                "https://www.pivotaltracker.com/services/v5/projects/123456/stories/111111"
                "PUT"
                nil
                '(("id" . "111111")
                  ("title" . "write-test"))))))

(provide 'test-org-pivotal-api)

;;; test-org-pivotal-api.el ends here
