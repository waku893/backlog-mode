;;; backlog-mode.el --- Major mode for Backlog -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Hiroyuki Kubota

;; Author: Hiroyuki Kubota <kubo.web@gmail.com>
;; URL: https://github.com/purcell/emacs-hcl-mode
;; Version: 0.01
;; Package-Requires: ((emacs "26.3"))

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

;; hcl-mode provides a major-mode of hcl file.

;;; Code:

(require 'request)
(defun backlog-mode ()
  "Backlog Mode "
  (interactive)
  (switch-to-buffer "*backlog*")  
  (setq mode-name "Backlog")
  (setq majar-mode 'backlog-mode)
  (setq local-map (make-sparse-keymap))
  (define-key local-map "\M-n" 'backlog-list-summary-next)
  (define-key local-map "\M-p" 'backlog-list-summary-pre)
  (define-key local-map "\M-l" 'backlog-list-summary-home)
  (define-key local-map "\M-sp" 'backlog-set-project-key)
  (define-key local-map "\M-ss" 'backlog-set-status-ids)
  (define-key local-map "\M-sk" 'backlog-set-keyword)
  (define-key local-map "\M-sc" 'backlog-set-count)
  (define-key local-map "v" 'backlog-issue-comment)
  (use-local-map local-map)
  (run-hooks 'backlog-mode-hook))

(defvar backlog-url)
(defvar backlog-api-key)
(defvar backlog-project-key)
(defvar offset 0)
(defvar status-ids '(1 2 3))
(defvar keyword "")
(defvar disp-count 100)

(defun get-project-id (project-key)
   (let ((res (request (concatenate 'string backlog-url "api/v2/projects/" project-key)
     :params `(("apiKey" . ,backlog-api-key))
     :parser 'json-read
     :sync t)))
     (cdr (assoc 'id (request-response-data res)))))

(defun key-value-cons (key values)
  (mapcar (lambda (x) (cons key x)) values))

(cl-defun get-issues (&key
		      (project-ids nil)
		      (issue-type-ids nil)
		      (status-ids nil)
		      (assignee-ids nil)
		      (created-user-ids nil)
		      (sort "")
		      (order "desc")
		      (offset "")
		      (count disp-count)
		      (created-since "")
		      (created-until "")
		      (updated-since "")
		      (updated-until "")
		      (ids nil)
		      (keyword ""))		    
  (let ((res
    (request (concatenate 'string backlog-url "api/v2/issues")
      :params `(("apiKey" . ,backlog-api-key)
		,@(key-value-cons "projectId[]" project-ids)
		,@(key-value-cons "issueTypeId[]" issue-type-ids)
		,@(key-value-cons "statusId[]" status-ids)
		,@(key-value-cons "assigneeId[]" assignee-ids)
		,@(key-value-cons "createdUserId[]" created-user-ids)
		("sort" . ,sort)
		("order" . ,order)
		("offset" . ,offset)
		("count" . ,count)
		("createdSince" . ,created-since)
		("createdUntil" . ,created-until)
		("updatedSince" . ,updated-since)
		("updatedUntil" . ,updated-until)
		,@(key-value-cons "id[]" ids)
		("keyword" . ,keyword))
      :parser 'json-read
      :sync t)))
      (request-response-data res)))

(defun key-value (key data)
  (if (and (listp data) (cdr (assoc key data)))
      (cdr (assoc key data))
    ""))

(defun key-value-list (key value data)
  (let ((i 0)
	(result nil)
	(alis (key-value key data)))
    (while (> (length alis) i)
      (setq result (concat result " " (key-value value (aref alis i))))
      (setq i (1+ i)))
    (if result
	(string-trim result)
      "")))
      
(defun display-issue-summary (issue)
  (format "[%s]  %s  %s  %s %s %s %s\n"
	  (key-value 'id issue)
	  (key-value 'name (key-value 'issueType issue))
	  (key-value 'name (key-value 'assignee issue))
	  (key-value 'name (key-value 'status issue))
	  (key-value 'updated issue)
	  (key-value 'issueKey issue)
	  (key-value 'summary issue)))

(defun backlog-set-project-key (projectkey)
  (interactive "sproject key:")
  (setq backlog-project-key projectkey)
  (message backlog-project-key))

(defun backlog-set-keyword (key)
  (interactive "skeyword:")
  (setq keyword key)
  (message keyword))

(defun backlog-set-count (cnt)
  (interactive "scount:")
  (setq disp-count cnt)
  (message disp-count))

(defun backlog-set-status-ids (ids)
  (interactive "sStatus IDs 1:未対応 2:処理中 3:処理済み 4:完了 ex. 1 2 3 :")
  (setq status-ids (mapcar 'string-to-number (split-string ids)))
  (message "%s" status-ids))

(defun backlog-get-point-id ()
  (save-excursion
    (beginning-of-line)
    (buffer-substring (1+ (point)) (+ (point) 11))))

(defun utctime-to-jsttime (time)
  (format-time-string "%Y-%m-%d %H:%M:%S" (date-to-time time)))

(defun original-to-new-value (content)
  (let* ((i 0)
	 (result ""))
    (while (> (length content) i)
      (let ((log (aref content i)))
	(setq result (concat result (format "%s: %s -> %s\n"
					    (key-value 'field log)
					    (key-value 'originalValue log)
					    (key-value 'newValue log))))
	(setq i (1+ i))))
    result))

(defun png-urls (content)
  (let* ((i 0)
	 (result ""))
    (while (> (length (cdr content)) i)
      (let ((log (aref (cdr content) i)))
	(when (string= (key-value 'field log) "attachment")
	  (setq result (concat result (format "png->%s\n"
					      (key-value 'newValue log)))))
	(setq i (1+ i))))
    result))
  
(defun display-issue (issue)
  (insert "url:" (concat backlog-url "view/" (key-value 'issueKey issue)) "\n")
  (insert (key-value 'issueKey issue) "\n")
  (insert (key-value 'summary issue) "\n")
  (insert "=============================================================================\n")
  (insert (key-value 'name (key-value 'createdUser issue)) "\n")
  (insert (utctime-to-jsttime (key-value 'created issue)) "\n\n")
  (insert (key-value 'description issue) "\n")
  (insert "-----------------------------------------------------------------------------\n")
  (insert "優先度: " (key-value 'name (key-value 'priority issue))
	  " 担当者: " (key-value 'name (key-value 'assignee issue)) "\n")
  (insert "カテゴリー: " (key-value-list 'category 'name issue)
	  " マイルストーン: " (key-value-list 'milestone 'name issue) "\n")
  (insert "発生バージョン: " (key-value-list 'versions 'name issue) "\n")
  (insert "予定時間: " (key-value 'estimatedHours issue)
	  " 実績時間: " (key-value 'actualHours issue) "\n")
  (insert "完了理由: " (key-value 'name (key-value 'resolution issue)) "\n")
  (insert "=============================================================================\n\n"))

(defun display-issue-comment (comments)
  (let* ((i 0)
	 (rcomments (reverse comments)))
    (while (> (length rcomments) i)
      (let ((comment (aref rcomments i)))
	;(insert (format "%s" comment))
	(insert (key-value 'name (key-value 'createdUser comment)) "\n")
	(insert (utctime-to-jsttime (key-value 'created comment)) "\n\n")
	(insert (original-to-new-value (key-value 'changeLog comment)))
	(insert (key-value 'content comment) "\n")
	(insert "-----------------------------------------------------------------------------\n")
	(setq i (1+ i))
	))))

(defun backlog-issue-comment ()
  (interactive)
    (let* ((id (backlog-get-point-id))
	   (res (request (concatenate 'string backlog-url "api/v2/issues")
		 :params `(("apiKey" . ,backlog-api-key)
			   ("id[]" . ,id))
		 :parser 'json-read
		 :sync t))
	   (comment (request (concatenate 'string backlog-url "api/v2/issues/" id "/comments")
		      :params `(("apiKey" . ,backlog-api-key))
		      :parser 'json-read
		      :sync t)))
      (switch-to-buffer "*backlog-issue*")
      (goto-address-mode)
      (erase-buffer)
      (display-issue (aref (request-response-data res) 0))
      (display-issue-comment (request-response-data comment))
      (goto-char 0)))
  
(defun backlog-list-summary ()
  (switch-to-buffer "*backlog*")
  (erase-buffer)
  (message "backlog-projectkey:%s status-ids:%s keyword:%s count:%s offset:%s" backlog-project-key status-ids keyword disp-count offset)
  (let ((i 0)
	(issues (get-issues :project-ids (list (get-project-id backlog-project-key))
			    :status-ids status-ids
			    :offset offset
			    :keyword keyword)))
    (while (> (length issues) i)
      (insert (display-issue-summary (aref issues i)))
      (setq i (1+ i)))))

(defun backlog-list-summary-home ()
  (interactive)
  (setq offset 0)
  (backlog-list-summary))

(defun backlog-list-summary-next ()
  (interactive)
  (setq offset (+ offset disp-count))
  (backlog-list-summary))

(defun backlog-list-summary-pre ()
  (interactive)
  (setq offset (- offset disp-count))
  (backlog-list-summary))



(provide 'backlog-mode)
