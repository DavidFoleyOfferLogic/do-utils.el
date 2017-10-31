;;; droplet_utils.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  ailbe

;; Author: ailbe
;; Keywords: lisp
;; Version: 0.0.1

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

;; Put a description of the package here

;;; Code:

;; code goes here

(require 'json)
(require 'dash)
(require 's)
(require 'tblui)

;; json library reference page: http://tess.oconnor.cx/2006/03/json.el

(defcustom doutils/doctl-executable nil "Path to the doctl cli tool")
(defcustom doutils/doctl-api-key nil "Api key to use with doctl")
(defcustom doutils/username nil "Username to login to the droplets as")
(defvar doutils/list-droplets nil)
(setq doutils/list-droplets "doctl compute droplet list --output json")

(defun doutils/parse-droplet-ip-address (droplet)
  (setq networks (plist-get droplet ':networks))
  (setq v4 (plist-get networks ':v4))
  (setq i1 (elt v4 1))
  (setq ip-address (plist-get i1 ':ip_address))
  ip-address)

(defun doutils/parse-droplets-json (droplets-json)
  (mapcar (lambda (droplet)
            (setq id (plist-get droplet ':id))
            (setq name (plist-get droplet ':name))
            (setq tags (mapcar (lambda (s) (concat s ", ")) (plist-get droplet ':tags)))
            (setq tags (reduce 'concat tags))
            (setq ip (doutils/parse-droplet-ip-address droplet))
            (setq strid (number-to-string id))
            `(,strid ,(vector strid name tags ip))) droplets-json))

(defun doutils/doctl-response-as-json (command)
  "Parse the doctl response as a json"
  (let (
        (json-object-type 'plist))
    (->
     command
     (doutils/run-doctl-command)
     (json-read-from-string))))


(defun doutils/droplets-get-tabulated-list-entries ()
  "Get the IP-Address and tags of each droplet from DROPLETS list running in your account."
  (let (
        (json-object-type 'plist))
    (->
     doutils/list-droplets
     (doutils/doctl-response-as-json)
     (doutils/parse-droplets-json))))

(defun doutils/run-doctl-command (command)
  "docstring"
  (interactive "P")
  (message "running command: %s" command)
  (shell-command-to-string command))

(defun doutils/set-user-name (user-name)
  "docstring"
  (if (string= "" user-name) (setq user-name (user-login-name)))
  user-name)

(defun doutils/parse-default-directory (id)
  "Parse the new default directory for a droplet."
  (setq droplet-username doutils/username)
  (setq json (doutils/doctl-response-as-json (concat "doctl compute droplet get " id " --output json")))
  (setq ip-address (doutils/parse-droplet-ip-address (elt json 0)))
  (concat "/ssh:" droplet-username "@" ip-address ":/home/" droplet-username))

(defun doutils/instances-ssh-into-instance (ids)
  "SSH into aws instance with IDS."
  (if (/= 1 (length ids))
      (error "Multiple instances cannot be selected."))
  (let* ((id (nth 0 ids))
         (instance-default-directory (doutils/parse-default-directory id)))
    (let ((default-directory instance-default-directory))
      (better-shell-for-current-dir))))

(defun doutils/run-shell-command-on-instance (ids)
  "SSH into aws instance with IDS."
  (if (/= 1 (length ids))
      (error "Multiple instances cannot be selected."))
  (let* ((id (nth 0 ids))
         (instance-default-directory (doutils/parse-default-directory id)))
    (let ((default-directory instance-default-directory))
      (shell-command (read-string "Shell command: ")))))

(defun doutils/run-emacs-command-on-instance (ids)
  "SSH into aws instance with IDS."
  (if (/= 1 (length ids))
      (error "Multiple instances cannot be selected."))
  (let* ((id (nth 0 ids))
         (instance-default-directory (doutils/parse-default-directory id)))
    (let ((default-directory instance-default-directory))
      (counsel-M-x))))

(defun doutils/run-dired-on-instance (ids)
  "SSH into aws instance with IDS."
  (if (/= 1 (length ids))
      (error "Multiple instances cannot be selected."))
  (let* ((id (nth 0 ids))
         (instance-default-directory (doutils/parse-default-directory id)))
    (let ((default-directory instance-default-directory))
      (dired default-directory))))

(tblui-define
 digitalocean-instances
 doutils/droplets-get-tabulated-list-entries
 [("Id" 30 nil)
  ("Name" 30 nil)
  ("Tags" 30 nil)
  ("IP" 15 nil)]
 ((:key "I"
        :name doutils/instances-inspect-popup
        :funcs ((?I "Inspect" doutils/instances-inspect-instances)))

  (:key "A"
        :name doutils/instances-action-popup
        :funcs ( (?C "SSH Into Instance" doutils/instances-ssh-into-instance)
                 (?S "Run Shell Command on Instance" doutils/run-shell-command-on-instance)
                 (?E "Run Emacs Command on Instance" doutils/run-emacs-command-on-instance)
                 (?D "Run Dired on Instance" doutils/run-dired-on-instance)))))

;;;###autoload
(defun digitalocean-instances ()
  "List do instances using doctl. (The `doctl` command)."
  (interactive)
  (digitalocean-instances-goto-ui))

(provide 'droplet_utils)

;;; droplet_utils.el ends here
