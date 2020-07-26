;;; cs-org-transfer.el --- Copying/transferring an org file with it's assets (links to other files) into another directory   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-lint)

(defun cs-transfer-single-org-file (&optional org-file-path only-if-not-in-project)
  (interactive)
    "Integrate a notes file into the website.
If you want to publish something from your notes, you move it into the
project's git directory.  This function makes you aware of what you
need to do to properly integrate that org file.

It saves a buffer-local variable in the target buffer, which
contains the file name of the source org file.
This way, even when links are still relative in the target file,
they can be resolved to the absolute links using that
variable.

Things to consider are:

- if you have links to other org files in there, they will break.  So,
  edit the final file to either not include them, or if the file to be
  linked to is already integrated into the project, change the link
  manually.

- if you embed links like images/pdfs/source code files specific to
  only that org file, they will need to be copied to the file's own
  specifc directory.  You can put these files, if they are of general
  interest and you might want to share them between different org
  files into an assets directory or into a subdirectory in there.

  Hard:
- if you have a link to an org file in a different project, and you
  want to include it, you must locate that org file in it's directory
  and calculate a relative link, according to the website's
  conventions."
    (unless org-file-path
      (setq org-file-path (buffer-file-name)))

    ;; now check if it's actually an org file
    (unless (string-equal (file-name-extension org-file-path)
                          "org")
      (user-error "Not an org file"))


    (let* (actually-write-now
           (target-file-path
            (helm-read-file-name "Write file to (full name, with .org extension): "
                                 :initial-input (expand-file-name "~/Dropbox/"))))
      ;; now check if the file is already placed in a project and
      ;; in that case issue a warning
      ;; (if (file-in-project-p org-file-path)
      ;;   (when (yes-or-no-p (concat org-file-path " is aleady part of a project. "
      ;;                              "Do you still want to transfer it to a project, again?"))
      ;;     (setq actually-write-now t))
      ;;   (setq actually-write-now t))

      (setq actually-write-now t)

      (when actually-write-now
        ;; make sure the directory exists
        (make-directory (file-name-directory target-file-path) t)
        (copy-file org-file-path target-file-path 1)

        ;; ------- in the new buffer -----------
        (find-file target-file-path)
        (rename-buffer (concat (buffer-name) "<target>"))
        ;; create buffer-local-variable containing the source filepath
        ;; to be able to retrieve the files from the buffer
        ;; even on a second pass of pull-files-into-asset-dir
        (make-local-variable 'pull-files-into-asset-dir-org-source-path)
        (setq pull-files-into-asset-dir-org-source-path org-file-path)
        (pull-files-into-asset-dir (current-buffer) only-if-not-in-project))))


;; ----- high-level functions ------

(defun cs-org-gthoot (&optional org-file-path)
  "Get the hell out of there!
This means to grab every link in the org file (first level link) and
back them up into an assets directory, at the same level as the copied
org file."
  (interactive)
  (unless org-file-path
    (setq org-file-path (buffer-file-name)))
  (cs-transfer-single-org-file org-file-path nil))



;; ----- low-level stuff --------

;; ------- make a custom org lint checker --------

(setq my-link-checker (make-org-lint-checker
                       :name 'cs-link-to-local-file
                       :description "Report links to non-existent files under a specific subdirectory. "
                       :categories '(link)
                       :trust 'low))

(defun org-lint-cs-link-to-local-file (ast)
  (org-element-map ast
      'link
    (lambda (l)
      (when (equal "file" (org-element-property :type l))
        (let ((file (org-element-property :path l)))
          (and (not (files-under-same-project-p (buffer-file-name) file))
               (list (org-element-property :begin l)
                     (format (if (org-element-lineage l
                                                      '(link))
                                 "CS: image file \"%s\"\
 not found in the same project"
                               "CS: local file \"%s\" not found in the same project")
                             file))))))))


(defun cs-org-toggle-link-display (&optional show-full)
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if (or org-link-descriptive show-full)
      (remove-from-invisibility-spec '(org-link))
    (add-to-invisibility-spec '(org-link)))
  (org-restart-font-lock)
  (setq org-link-descriptive (not (or org-link-descriptive show-full))))


(defun cs-org-check-for-broken-links (;; root-dir org-file-path
                                               )
  "Check for broken links of an org file ORG-FILE-PATH.
All internal links should be to files placed in a subdirectory
of ROOT-DIR.
This file will check from the top down each link, and will halt
at the first broken link to be managed.  Thus, it needs to be run
multiple times to get to all the broken links."
  (interactive)
  (let* ((org-buffer (current-buffer)))
    (add-to-list 'org-lint--checkers my-link-checker
                 1)
    (cs-org-toggle-link-display t)
    (call-interactively 'org-lint)
    (delete my-link-checker org-lint--checkers)))

(defun get-assets-dir-from-org-file (org-filepath)
  "Get the assets directory associated to an org file ORG-FILEPATH."
  (assert (string-equal (file-name-extension org-filepath) "org"))

  (let* ((org-file-dir (file-name-directory org-filepath))
         (org-file-base (file-name-base org-filepath)))
    (file-name-as-directory (concat (file-name-as-directory (concat org-file-dir org-file-base))
                                    "assets"))))

(defun cs-org-create-assets-dir (&optional org-filepath)
  "For an org file myfile.org, create
a directory myfile and a directory myfile/assets"
  (interactive)
  (unless org-filepath
    (setq org-filepath (buffer-file-name)))

  ;; check if it's an org file and if it exists
  (unless (and (file-exists-p org-filepath) (string-equal (file-name-extension org-filepath) "org"))
    (user-error (concat "Org file does not exist, or file is not an org file: " org-filepath)))

  (let* ((assets-dir (get-assets-dir-from-org-file org-filepath)))
    (if (yes-or-no-p (concat "create asset directory " assets-dir " ?"))
        (progn
          (make-directory assets-dir t))
      (message "asset directory was not created"))))

(defun cs-org-get-linked-files-not-in-dedicated-assets-folder ()
  "Each org file can get a dedicated assets folder:
e.g.: ./a.org gets ./a/assets/ in which to ")

(defun cs-org-get-linked-files-more-types (&optional link-type-list)
  "Apart from what org-mode recognizes as links, there can be other links as well."
  (let* ((org-link-types-list (if link-type-list
                                  link-type-list
                                (list "bibliography"
                                    ;; org-ref's bibliography link
                                    "file"))))
    (remove nil (-flatten (mapcar (lambda (type-str)
                                    (let* ((result-lst (cs-org-get-linked-files type-str)))
                                      (cond ((string-equal type-str "bibliography")
                                             ;; in org-ref you can add multiple bibliographies
                                             ;; with a comma-separated string
                                             (car (mapcar (lambda (indiv-str)
                                                            (split-string indiv-str ","))
                                                          result-lst)))
                                            (t result-lst))))
                                  org-link-types-list)))))


(defun cs-org-get-linked-files (&optional type-str)
  "Gets linked file paths, but in their formatted version.
That means not in their full expanded version."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let* ((is-pdfview (string= (org-element-property :type link)
                                  "pdfview"))
             (link-str (org-element-property :path link)))
        (when (or (string= (org-element-property :type link)
                           (if (not type-str)
                               "file"
                             type-str))
                  is-pdfview)
          (if is-pdfview
              (car (split-string link-str "::"))
            link-str))))))


;; ---- helm source to pick out the custom list of files ----


(defun get-links-to-move (&optional original-org-filepath current-org-filepath only-if-not-in-project)
  "Being in an org file, get the links that you want to move:

- you could want to GTHOOT (get the hell out of there, i.e. you want to save all
linked files safely away to a dedicated folder, making a particular org file
more or less standalone) -> get all links (standard)

- you could want to only get the links of those files that are not under the
same project as the org file.  This is useful if you want to e.g. publish
a website and you want to share some of the resources, but also have certain
files dedicated to a specific org file -> ONLY-IF-NOT-IN-PROJECT

original-org-buffer refers to the org buffer the links of which should be extracted."
  (interactive)
  (when (and (not original-org-filepath) (boundp 'pull-files-into-asset-dir-org-source-path))
    (setq original-org-filepath pull-files-into-asset-dir-org-source-path))

  (if (and (not current-org-filepath)
           (not (string-equal (file-name-extension (buffer-file-name))
                              "org")))
      (user-error "No current-org-filepath to operate on")
    (when (and (not current-org-filepath)
               (string-equal (file-name-extension (buffer-file-name))
                             "org"))
      (setq current-org-filepath (buffer-file-name))))

  (let* ((original-org-buffer (when original-org-filepath
                                (get-file-buffer original-org-filepath)))
         (linked-files-absolute-paths-and-printed-paths
          (remove nil
                  (mapcar (lambda (lst)
                            (when (and (nth 0 lst)
                                       (nth 1 lst))
                              lst))
                          (cs-org-get-linked-files-absolute-paths-and-printed-paths
                           current-org-filepath original-org-filepath)))))
    (with-current-buffer (current-buffer)
      original-org-buffer
      (message (concat "FROM GET_DATA_2: "
                       (prin1-to-string (current-buffer))
                       " "
                       (prin1-to-string original-org-buffer)))
      (if only-if-not-in-project
          ;; (mapcar (lambda (filepath)
          ;;           (list filepath filepath))
          ;;         (remove nil
          ;;                 (mapcar (lambda (filepath)
          ;;                           (if (files-under-same-project-p filepath current-org-filepath)
          ;;                               nil
          ;;                             filepath))
          ;;                         linked-files-absolute-paths-and-printed-paths)))
          (user-error "The only-if-note-in-project option is not supported yet")
        ;; if it doesn't exist under assets, count it as a candidate and get the link
        (remove nil
                (mapcar (lambda (lst)
                          (let* ((absolute-filepath (nth 0 lst))
                                 ;; as it is printed in the target (current) file
                                 (as-printed-filepath (nth 1 lst))
                                 ;; as it would be printed if it was in the assets directory
                                 (target-dir (get-assets-dir-from-org-file current-org-filepath))
                                 (target-filepath-intended (get-target-filepath-in-assets-dir target-dir absolute-filepath)))
                            (when (or (not (file-exists-p
                                            (concat (get-assets-dir-from-org-file current-org-filepath)
                                                    (file-name-nondirectory absolute-filepath))))
                                      (not (member (cs-org-get-relative-file-path-to-insert-for-file-in-asset-dir
                                                    target-filepath-intended current-org-filepath)
                                                   (cs-org-get-linked-files-more-types))))
                              (list as-printed-filepath absolute-filepath target-filepath-intended))))
                        linked-files-absolute-paths-and-printed-paths))))))

(defun cs-org-get-linked-files-absolute-paths-and-printed-paths (current-org-filepath &optional original-org-filepath)
  "If you give original-org-filepath, the relative links in your current org filepath are
expanded w.r.t the directory of the original-org-filepath."
  (mapcar (lambda (linked-file-path-as-printed)
            (if (file-name-absolute-p linked-file-path-as-printed)
                linked-file-path-as-printed
              ;; else: make it absolute:
              ;; - first look if the file wrt the new-org-filepath exists
              ;;   - if yes, use that one
              ;;   - if no, check if it exists w.r.t the original-org-filepath
              (let* ((filepath-wrt-original-org-filepath
                      (when original-org-filepath
                          (concat (file-name-directory original-org-filepath)
                                  linked-file-path-as-printed)))
                     (filepath-wrt-current-org-filepath
                      (concat (file-name-directory current-org-filepath)
                              linked-file-path-as-printed)))
                (if (file-exists-p filepath-wrt-current-org-filepath)
                    (list filepath-wrt-current-org-filepath linked-file-path-as-printed)
                  (if (and filepath-wrt-original-org-filepath
                           (file-exists-p filepath-wrt-original-org-filepath))
                      (list filepath-wrt-original-org-filepath linked-file-path-as-printed)
                    (warn (concat "Both filepaths " (prin1-to-string filepath-wrt-current-org-filepath)
                                  " and " (prin1-to-string filepath-wrt-original-org-filepath)
                                  " do not exist"))
                    nil)))))
          (cs-org-get-linked-files-more-types)))


(defun get-target-filepath-in-assets-dir (assets-dir original-filepath)
  "When a file is to be copied to the assets dir, it may need to be
renamed or placed in a subdirectory, if there's already a file named
equally in the assets directory. This function figures out where to put
and what to call the new file."
  (let* ((flat-in-assets-dir-target-path (concat assets-dir (file-name-nondirectory original-filepath)))
         (target-file-name-base (file-name-base original-filepath))
         (target-path flat-in-assets-dir-target-path)
         (extension (file-name-extension original-filepath))
         files-equal-after-diff-p)

    (while (and (file-exists-p target-path)
                (not
                 (setq
                  files-equal-after-diff-p
                  (string-equal ""
                                (shell-command-to-string (concat "diff "
                                                                 (prin1-to-string original-filepath)
                                                                 " "
                                                                 (prin1-to-string target-path)))))))
      (setq target-file-name-base (concat target-file-name-base "-2"))
      (setq target-path (concat assets-dir target-file-name-base "."
                                extension)))
    (if files-equal-after-diff-p
        (progn
          ;; car: target path, cadr: override file
          ;; if two equally named files are equal, give back the target-path
          ;; for substitution, but indicate that there is no need
          ;; to write the file again
          (list target-path nil))
      (list target-path t))))


(defun copy-and-relink (org-buffer candidate-full-filepath
                                   candidate-filepath-as-printed-in-org)
  "Copy the file at candidate-filepath-as-printed-in-org to the ORG-BUFFER's assets dir."


  (with-current-buffer org-buffer
    (let* ((candidate-filepath candidate-full-filepath)
           (org-file-path (buffer-file-name org-buffer))
           ;; FIXME: this is redundant code vvvvv (you can pass the intended-target-dir
           ;; into the function as an argument directly, generated form get-links-to-move)
           (target-dir (get-assets-dir-from-org-file org-file-path))
           (target-path-results (get-target-filepath-in-assets-dir target-dir
                                                                   candidate-filepath)))

      (if (or (car target-path-results)
              (cadr target-path-results))
          ;; create assets directory if it's not already there
          (if (not (file-exists-p target-dir))
              (make-directory target-dir t)))

      (when (cadr target-path-results)
        ;; write the file to the target path
        (call-process-shell-command (read-shell-command "write to assets: "
                                                        (concat "cp -afT "
                                                                (prin1-to-string candidate-filepath)
                                                                " "
                                                                (prin1-to-string (car target-path-results))))
                                    nil
                                    "*writing to asset dir*"
                                    t))

      (when (car target-path-results)
        ;; now replace the links in the buffer
        (cs-org-toggle-link-display t)
        (save-excursion
          (goto-char (point-min))
          (query-replace-regexp (regexp-quote candidate-filepath-as-printed-in-org)
                                (cs-org-get-relative-file-path-to-insert-for-file-in-asset-dir target-path-results
                                                                                               org-file-path)))))))


(defun cs-org-get-relative-file-path-to-insert-for-file-in-asset-dir (target-path-resulted current-org-filepath)
  ""
  (concat "./"
          (file-relative-name (car target-path-resulted)
                              (file-name-directory current-org-filepath))))


(defun pull-files-into-asset-dir (&optional new-org-buffer only-if-not-in-project)
  "From within an org file, scan it's links one by one.
Act on them to pull them into the file's assets directory."
  ;; make helm source with the linked files inside
  ;; then, a helm action to post them into the assets directory
  (interactive)
  (unless new-org-buffer
    (if (and (string-equal (file-name-extension (buffer-file-name)) "org")
             ;; (file-in-project-p (buffer-file-name))
             )
        (setq new-org-buffer (current-buffer))
      (user-error "Not in the position to pull files")))

  ;; you may not have an 'original org path', if it's just a standalone file trying to
  ;; pull stuff into an asset directory
  (let* ((original-org-filepath (when (and (boundp 'pull-files-into-asset-dir-org-source-path)
                                           (file-exists-p pull-files-into-asset-dir-org-source-path))
                                  pull-files-into-asset-dir-org-source-path))
         (actionable-candidates (get-links-to-move original-org-filepath
                                                            (buffer-file-name new-org-buffer)
                                                            only-if-not-in-project)))
    (if actionable-candidates
        (helm :sources
              (helm-build-sync-source "Copy over"
                :header-name (lambda (_)
                               (format "header name"))
                :candidates (lambda ()
                              (mapcar (lambda (candidate)
                                        (list (prin1-to-string candidate) candidate))
                                      actionable-candidates))
                :action (helm-make-actions
                         "Copy this to org file assets and re-link"
                         (lambda (_)
                           (message
                            (concat
                             "writing to assets results: "
                             (prin1-to-string
                              (mapcar (lambda (candidate)
                                        (let* ((full-filepath (nth 1 (car candidate)))
                                               (as-printed-filepath (nth 0 (car candidate))))
                                          (message (prin1-to-string "In there: "))
                                          (message (prin1-to-string candidate))
                                          (copy-and-relink new-org-buffer
                                                           full-filepath
                                                           as-printed-filepath)))
                                      (helm-marked-candidates)))))
                           (pull-files-into-asset-dir new-org-buffer only-if-not-in-project))
                                           "Copy this to general assets and re-link"
                                           (lambda (_)
                                             (pull-files-into-asset-dir
                                              new-org-buffer only-if-not-in-project))))          )
      (message "no actionable candidates"))))


(provide 'cs-org-transfer)
;;; cs-org-transfer.el ends here
