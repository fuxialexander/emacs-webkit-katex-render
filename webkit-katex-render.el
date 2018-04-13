;;; webkit-katex-render.el --- Instant Preview of Latex Fragment using Webkit Widgets -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Fu Xi

;; Author: Alexander Fu Xi <fuxialexander@gmail.com>
;; URL: https://github.com/osener/emacs-webkit-katex-render
;; Maintainer: Alexander Fu Xi  <fuxialexander@gmail.com>
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "26.0") (posframe "0.1.0"))

;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

;; * require
(require 'xwidget)
(require 'posframe)

;; * global variables
(defvar webkit-katex-render--client-path
  (concat "file://"
          (file-name-directory (or load-file-name buffer-file-name))
          "katex.html"))
(defvar webkit-katex-render--buffer-name " *webkit-katex-render*")
(defvar webkit-katex-render--resize-flag nil)
(defvar webkit-katex-render--background-color (face-attribute 'default :background))
;; (defvar webkit-katex-render--background-color (doom-color 'bg))
(defvar webkit-katex-render--math-at-point-function nil)
(defvar webkit-katex-render--org-math-preprocess-function
  'webkit-katex-render--org-math-preprocess)

;; * init
(defun webkit-katex-render--run-xwidget ()
  "Launch embedded Webkit instance."
  (with-current-buffer webkit-katex-render--buffer-name
    (let ((inhibit-read-only t))
      (goto-char 1)
      (let ((id (make-xwidget
                 'webkit
                 nil
                 (window-pixel-width)
                 (window-pixel-height)
                 nil
                 webkit-katex-render--buffer-name)))
        (xwidget-resize id (window-pixel-width) (window-pixel-height))
        (set-xwidget-query-on-exit-flag id nil)
        (put-text-property (point) (+ 1 (point))
                           'display (list 'xwidget ':xwidget id))
        (xwidget-webkit-mode)
        (xwidget-webkit-goto-uri (xwidget-at 1)
                                 webkit-katex-render--client-path)
        (webkit-katex-render--set-background)))))

(defun webkit-katex-render--show ()
  "Make color picker childframe visible."
  (when-let* ((current-frame (selected-frame))
              (buffer (webkit-katex-render--get-buffer))
              (frame (webkit-katex-render--get-frame)))
    (progn
      (select-frame frame t)
      (switch-to-buffer buffer t t)
      (select-frame current-frame t)
      (make-frame-visible frame)
      (redraw-frame frame)

      (let*
          ((position (point))
           (parent-window (selected-window))
           (parent-frame (window-frame parent-window))
           (x-pixel-offset 0)
           (y-pixel-offset 0)
           (font-width (default-font-width))
           (font-height (posframe--get-font-height position))
           (frame-resize-pixelwise t)
           (position (posframe-poshandler-point-bottom-left-corner
                      `(;All poshandlers will get info from this plist.
                        :position ,position
                        :font-height ,font-height
                        :font-width ,font-width
                        :posframe ,frame
                        :posframe-buffer ,buffer
                        :parent-frame ,parent-frame
                        :parent-window ,parent-window
                        :x-pixel-offset ,x-pixel-offset
                        :y-pixel-offset ,y-pixel-offset))))
        (set-frame-position frame (car position) (cdr position))))))

(defun webkit-katex-render--create ()
  "Create a new posframe and launch Webkit."
  (posframe-show webkit-katex-render--buffer-name
                 :string " "
                 :min-width 40
                 :min-height 6
                 :position (point))

  (define-key (current-global-map) [xwidget-event]
    (lambda ()
      (interactive)

      (let ((xwidget-event-type (nth 1 last-input-event)))
        (when (eq xwidget-event-type 'load-changed)
          (webkit-katex-render--resize)
          ;; (webkit-katex-render--set-background)
          )

        (when (eq xwidget-event-type 'javascript-callback)
          (let ((proc (nth 3 last-input-event))
                (arg (nth 4 last-input-event)))
            (funcall proc arg))))))

  (webkit-katex-render--run-xwidget))

(defun webkit-katex-render--get-buffer ()
  "Return color picker buffer."
  (get-buffer webkit-katex-render--buffer-name))

(defun webkit-katex-render--get-frame ()
  "Return color picker frame."
  (when-let* ((buffer (webkit-katex-render--get-buffer)))
    (seq-find
     (lambda (frame)
       (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
         (or (eq buffer (car buffer-info))
             (eq buffer (cdr buffer-info)))))
     (frame-list))))

(defun webkit-katex-render--set-background ()
  "Evaluate JS code in color picker Webkit instance."
  (webkit-katex-render--execute-script
   (format "document.body.style.background = '%s';"
           webkit-katex-render--background-color)))

(defvar webkit-katex-render--emulation-alist '((t . nil)))

(defvar-local webkit-katex-render--my-keymap nil)
(defvar-local webkit-katex-render--last-position nil)

(defsubst webkit-katex-render--enable-overriding-keymap (keymap)
  "Enable color picker overriding KEYMAP."
  (webkit-katex-render--uninstall-map)
  (setq webkit-katex-render--my-keymap keymap))

(defun webkit-katex-render--ensure-emulation-alist ()
  "Append color picker emulation alist."
  (unless (eq 'webkit-katex-render--emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'webkit-katex-render--emulation-alist
                (delq 'webkit-katex-render--emulation-alist emulation-mode-map-alists)))))

(defun webkit-katex-render--install-map ()
  "Install temporary color picker keymap."
  (unless (or (cdar webkit-katex-render--emulation-alist)
              (null webkit-katex-render--my-keymap))
    (setf (cdar webkit-katex-render--emulation-alist) webkit-katex-render--my-keymap)))

(defun webkit-katex-render--uninstall-map ()
  "Uninstall temporary color picker keymap."
  (setf (cdar webkit-katex-render--emulation-alist) nil))

(defvar webkit-katex-render--active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\e\e\e" 'webkit-katex-render-hide)
    (define-key keymap "\C-g" 'webkit-katex-render-hide)
    keymap)
  "Keymap that is enabled during an active completion.")

(defun webkit-katex-render--get-xwidget ()
  "Return Xwidget instance."
  (with-current-buffer webkit-katex-render--buffer-name
    (xwidget-at 1)))

(defun webkit-katex-render--execute-script (script &optional fn)
  "Execute SCRIPT in embedded Xwidget and run optional callback FN."
  (when-let* ((xw (webkit-katex-render--get-xwidget)))
    (xwidget-webkit-execute-script xw script fn)))

(defun webkit-katex-render--resize-helper (size)
  (when-let* ((frame (webkit-katex-render--get-frame))
              (frame-resize-pixelwise t)
              (width (frame-parameter frame 'width))
              (height (frame-parameter frame 'height))
              (new-width (+ 40 (ceiling (aref size 0))))
              (new-height (+ 20 (* 2 (ceiling (aref size 1))))))
    (set-frame-size frame new-width new-height t)))

(defun webkit-katex-render--resize (&optional arg)
  "Resize color picker frame to widget boundaries."
  (webkit-katex-render--execute-script
   "[document.querySelector('.katex-html').offsetWidth, document.querySelector('.katex-html').offsetHeight];"
   'webkit-katex-render--resize-helper))

(defun webkit-katex-render--render (math)
  "Render math use Katex"
  (webkit-katex-render--execute-script
   (format
    "try_render('%s');"
    (url-hexify-string math)))
  'webkit-katex-render--render-helper)

(defun webkit-katex-render--render-helper (math)
  (webkit-katex-render--resize))

(defun webkit-katex-render--org-math-preprocess (math type)
  (if (eq type 'latex-fragment)
      (setq math (substring math 2 -2))
    (if (eq type 'latex-environment)
        (progn
          (setq math
                (replace-regexp-in-string
                 "begin{equation}\\|begin{align}\\|begin{align\\*}"
                 "begin{aligned}"
                 math))
          (setq math
                (replace-regexp-in-string
                 "end{equation}\\|end{align}\\|end{align\\*}"
                 "end{aligned}"
                 math)))))
  math)

(defun webkit-katex-render--org-math-at-point ()
  (if (org-inside-LaTeX-fragment-p)
      (let (beg end)
        (let ((datum (org-element-context)))
          (when (memq (org-element-type datum)
                      '(latex-environment latex-fragment))
            (setq beg (org-element-property :begin datum))
            (setq end (org-element-property :end datum)))
          (save-excursion
            (goto-char beg)
            (let* ((context (org-element-context))
                   (type (org-element-type context)))
              (when (memq type '(latex-environment latex-fragment))
                (let ((value (org-element-property :value context))
                      (beg (org-element-property :begin context))
                      (end (save-excursion
                             (goto-char (org-element-property :end context))
                             (skip-chars-backward " \r\t\n")
                             (point))))
                  (goto-char end)
                  (funcall webkit-katex-render--org-math-preprocess-function
                           value type)))))))
    nil))

(defun webkit-katex-render--tex-math-preprocess (math type)
  "Preprocess current MATH environment with TYPE."
  (cond ((eq type 'sw-on)
         (setq math (substring math 2 -2)))
        ((eq type 'sw-toggle)
         (setq math (substring math 2 -2)))
        ((eq type 'env-on)
         (setq math (replace-regexp-in-string
                     "\\\\"
                     "\\"
                     math t t))
         (setq math
               (replace-regexp-in-string
                "begin{equation}\\|begin{align}\\|begin{align\\*}"
                "begin{aligned}"
                math))
         (setq math
               (replace-regexp-in-string
                "end{equation}\\|end{align}\\|end{align\\*}"
                "end{aligned}"
                math)))
        (t math))

  math)

(defun webkit-katex-render--tex-math-at-point ()
  "Mark current math environment."
  (if (texmathp)
      (let* ((string (car texmathp-why))
             (pos (cdr texmathp-why))
             (reason (assoc string texmathp-tex-commands1))
             (type (cadr reason)))
        (cond
         ((eq type 'env-on) ;; environments equation, align, etc.
          (progn
            (let ((cur (point))
                  (count 1) beg end)
              ;; Only change point and mark after beginning and end were found.
              ;; Point should not end up in the middle of nowhere if the search fails.
              (save-excursion
                (dotimes (c count) (LaTeX-find-matching-end))
                (setq end (line-beginning-position 2))
                (goto-char cur)
                (dotimes (c count) (LaTeX-find-matching-begin))
                (setq beg (point)))
              (webkit-katex-render--tex-math-preprocess
               (buffer-substring-no-properties beg end) type))))
         ;; ((eq type 'arg-on) ;; \ensuremath etc.
         ;;  (goto-char pos)
         ;;  (set-mark (point))
         ;;  (forward-sexp 2)
         ;;  (exchange-point-and-mark))
         ((eq type 'sw-toggle) ;; $ and $$
          (webkit-katex-render--tex-math-preprocess
           (buffer-substring-no-properties pos (scan-sexps pos 1)) type))
         ((eq type 'sw-on) ;; \( and \[
          (webkit-katex-render--tex-math-preprocess
           (save-excursion
             (buffer-substring-no-properties
              pos
              (re-search-forward texmathp-onoff-regexp)))
           type))))
    nil))

(defun webkit-katex-render--math-at-point ()
  "Return recognized math at point."
  (or (and (equal major-mode 'latex-mode)
            (webkit-katex-render--tex-math-at-point))
      (and (equal major-mode 'org-mode)
           (webkit-katex-render--org-math-at-point))
      (when webkit-katex-render--math-at-point-function
        (funcall webkit-katex-render--math-at-point-function))))

(defun webkit-katex-render-show (math-at-point)
  "Activate color picker."
  (if (not (buffer-live-p (webkit-katex-render--get-buffer)))
      (webkit-katex-render--create))
  (webkit-katex-render--render math-at-point)
  (if webkit-katex-render--resize-flag
      (progn
        (webkit-katex-render--resize)
        (setq webkit-katex-render--resize-flag nil)))
  (webkit-katex-render--show)
  (webkit-katex-render--set-background)
  (webkit-katex-render--ensure-emulation-alist)
  (webkit-katex-render--enable-overriding-keymap webkit-katex-render--active-map)
  (webkit-katex-render--install-map)
  t)

(defun webkit-katex-render-hide ()
  "Hide color picker frame."
  (setq webkit-katex-render--resize-flag t)
  (when-let* ((frame (webkit-katex-render--get-frame)))
    (make-frame-invisible frame))
  (webkit-katex-render--enable-overriding-keymap nil))

(defun webkit-katex-render-cleanup ()
  "Destroy color picker buffer and frame."
  (dolist (xwidget-view xwidget-view-list)
    (delete-xwidget-view xwidget-view))
  (posframe-delete-all)
  (kill-buffer webkit-katex-render--buffer-name))

(defun webkit-katex-render-update ()
  (let ((math-at-point (webkit-katex-render--math-at-point)))
    (if math-at-point
        (webkit-katex-render-show math-at-point)
      (webkit-katex-render-hide))))

;;;###autoload
(define-minor-mode webkit-katex-render-mode
  "Toggle Katex preview"
  nil nil nil
  (if webkit-katex-render-mode
      (progn
        (unless (featurep 'xwidget-internal)
          (user-error "Your Emacs was not compiled with xwidgets support"))
        (unless (display-graphic-p)
          (user-error "webkit-katex-render only works in graphical displays"))
        (add-hook 'post-self-insert-hook #'webkit-katex-render--resize)
        (add-hook 'post-command-hook #'webkit-katex-render-update))
    (remove-hook 'post-command-hook #'webkit-katex-render-update)
    (remove-hook 'post-self-insert-hook #'webkit-katex-render--resize)
    (webkit-katex-render-cleanup)))

(provide 'webkit-katex-render)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; webkit-katex-render.el ends here
