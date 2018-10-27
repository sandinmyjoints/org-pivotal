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

(require 'ert)
(require 'org-pivotal)

(ert-deftest org-pivotal-check-sum ()
  (should (equal (+ 1 1) 2)))

(provide 'org-pivotal-test)

;;; org-pivotal-test.el ends here
