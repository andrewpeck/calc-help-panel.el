;;; calc-help-panel.el --- Clickable keybinding panel for Emacs Calc  -*- lexical-binding: t; -*-

;; Author: Andrew Peck <me@andrewpeck.xyz>
;; Keywords: calc, convenience

;;; Commentary:

;; Shows a calculator-style button panel next to the Calc stack and
;; trail.  Hovering a button shows its keybinding; clicking it runs the
;; command on the stack.
;;
;;   (require 'calc-help-panel)
;;   (calc-help-panel-mode 1)

;;; Code:

(require 'calc)
(require 'calc-ext)                     ; most panel commands live here
(require 'button)
(require 'cus-edit)                     ; for the `custom-button' faces

(defgroup calc-help-panel nil
  "Clickable keybinding panel for Calc."
  :group 'calc)

(defcustom calc-help-panel-button-width 6
  "Width of each button label, in characters."
  :type 'natnum)

(defface calc-help-panel-button
  '((t :inherit custom-button))
  "Face for calc help panel buttons.")

(defface calc-help-panel-button-active
  '((t :inherit custom-button-pressed))
  "Face for buttons whose mode is currently active.")

(defconst calc-help-panel-buffer-name " *Calc Help*"
  "Leading space hides it from buffer lists and `global-hl-line-mode'.")

(defconst calc-help-panel-layout
  '((("ENTER" "RET"   calc-enter)
     ("DROP"  "DEL"   calc-pop)
     ("SWAP"  "TAB"   calc-roll-down)
     ("LAST"  "M-RET" calc-last-args)
     ("UNDO"  "U"     calc-undo)
     ("REDO"  "D"     calc-redo))
    (("+"     "+"     calc-plus)
     ("−"     "-"     calc-minus)
     ("×"     "*"     calc-times)
     ("÷"     "/"     calc-divide)
     ("±"     "n"     calc-change-sign)
     ("1/x"   "&"     calc-inv))
    (("yˣ"    "^"     calc-power)
     ("√x"    "Q"     calc-sqrt)
     ("eˣ"    "E"     calc-exp)
     ("ln"    "L"     calc-ln)
     ("10ˣ"   "H E"   calc-hyperbolic calc-exp)
     ("log"   "H L"   calc-log10))
    (("sin"   "S"     calc-sin)
     ("cos"   "C"     calc-cos)
     ("tan"   "T"     calc-tan)
     ("asin"  "I S"   calc-arcsin)
     ("acos"  "I C"   calc-arccos)
     ("atan"  "I T"   calc-arctan))
    (("sinh"  "H S"   calc-sinh)
     ("cosh"  "H C"   calc-cosh)
     ("tanh"  "H T"   calc-tanh)
     ("π"     "P"     calc-pi)
     ("DEG"   "m d"   calc-degrees-mode)
     ("RAD"   "m r"   calc-radians-mode))
    (("n!"    "!"     calc-factorial)
     ("mod"   "%"     calc-mod)
     ("abs"   "A"     calc-abs)
     ("floor" "F"     calc-floor)
     ("round" "R"     calc-round)
     ("logb"  "B"     calc-log))
    (("STO"   "s s"   calc-store)
     ("RCL"   "s r"   calc-recall)
     ("ALG"   "'"     calc-algebraic-entry)
     ("FIX"   "d f"   calc-fix-notation)
     ("SCI"   "d s"   calc-sci-notation)
     ("NORM"  "d n"   calc-normal-notation)))
  "Rows of buttons.  Each button is (LABEL KEYS COMMAND...).
KEYS is shown on hover; the COMMANDs are called in order on click.")

(defconst calc-help-panel-active-p
  `((calc-degrees-mode    . ,(lambda () (eq calc-angle-mode 'deg)))
    (calc-radians-mode    . ,(lambda () (eq calc-angle-mode 'rad)))
    (calc-fix-notation    . ,(lambda () (eq (car calc-float-format) 'fix)))
    (calc-sci-notation    . ,(lambda () (eq (car calc-float-format) 'sci)))
    (calc-normal-notation . ,(lambda () (eq (car calc-float-format) 'float))))
  "Alist of COMMAND to a predicate, run in the Calc buffer.
Buttons whose command's predicate is non-nil use the active face.")

(defvar calc-help-panel--calc-buffer nil
  "The Calc buffer the help panel is attached to.")

(defun calc-help-panel--run (commands)
  "Select the Calc window and call COMMANDS interactively."
  (let ((win (get-buffer-window calc-help-panel--calc-buffer)))
    (unless win
      (user-error "Calc window is not visible"))
    (select-window win)
    (dolist (cmd commands)
      (call-interactively cmd))))

(defun calc-help-panel--label (label)
  "Center LABEL in `calc-help-panel-button-width' columns."
  (let* ((pad (max 0 (- calc-help-panel-button-width (string-width label))))
         (left (/ pad 2)))
    (concat (make-string left ?\s) label (make-string (- pad left) ?\s))))

(defun calc-help-panel--buffer ()
  "Return the help panel buffer, filled with buttons."
  (with-current-buffer (get-buffer-create calc-help-panel-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (special-mode)
      (setq-local cursor-type nil
                  truncate-lines t)
      (dolist (row calc-help-panel-layout)
        (dolist (spec row)
          (pcase-let ((`(,label ,keys . ,commands) spec))
            (insert-text-button
             (calc-help-panel--label label)
             'face 'calc-help-panel-button
             'help-echo (format "%s   (%s)" keys
                                (mapconcat #'symbol-name commands " "))
             'follow-link t
             'active-p (alist-get (car commands) calc-help-panel-active-p)
             'action (lambda (_) (calc-help-panel--run commands))))
          (insert " "))
        (insert "\n"))
      (goto-char (point-min)))
    (current-buffer)))

(defun calc-help-panel--refresh (&rest _)
  "Update button faces to reflect the current Calc modes."
  (let ((help (get-buffer calc-help-panel-buffer-name))
        (calc calc-help-panel--calc-buffer))
    (when (and help (buffer-live-p calc))
      (with-current-buffer help
        (let ((inhibit-read-only t)
              (pos (point-min))
              button)
          (while (setq button (next-button pos t))
            (when-let* ((pred (button-get button 'active-p)))
              (button-put button 'face
                          (if (with-current-buffer calc (funcall pred))
                              'calc-help-panel-button-active
                            'calc-help-panel-button)))
            (setq pos (button-end button))))))))

;;;###autoload
(defun calc-help-panel-show ()
  "Show the help panel to the right of the current Calc stack and trail."
  (interactive)
  (unless (derived-mode-p 'calc-mode)
    (user-error "Not in a Calc buffer"))
  (setq calc-help-panel--calc-buffer (current-buffer))
  (unless (get-buffer-window calc-help-panel-buffer-name)
    (let* ((calc-win (get-buffer-window (current-buffer)))
           (anchor (if (get-buffer-window (calc-trail-buffer))
                       (window-parent calc-win)
                     calc-win))
           (cols (apply #'max (mapcar #'length calc-help-panel-layout)))
           (width (+ (* cols (1+ calc-help-panel-button-width)) 2))
           (win (split-window anchor (- width) t)))
      (set-window-buffer win (calc-help-panel--buffer))
      (set-window-dedicated-p win t)
      (calc-help-panel--refresh)
      (fit-window-to-buffer win))))

(defun calc-help-panel--hide ()
  "Delete any windows showing the help panel."
  (when-let* ((buf (get-buffer calc-help-panel-buffer-name)))
    (delete-windows-on buf)))

;;;###autoload
(define-minor-mode calc-help-panel-mode
  "Show a clickable keybinding panel beside Calc's stack and trail."
  :global t
  (if calc-help-panel-mode
      (progn
        (add-hook 'calc-start-hook #'calc-help-panel-show)
        (add-hook 'calc-end-hook #'calc-help-panel--hide)
        (advice-add 'calc-set-mode-line :after #'calc-help-panel--refresh))
    (remove-hook 'calc-start-hook #'calc-help-panel-show)
    (remove-hook 'calc-end-hook #'calc-help-panel--hide)
    (advice-remove 'calc-set-mode-line #'calc-help-panel--refresh)
    (calc-help-panel--hide)))

(provide 'calc-help-panel)
;;; calc-help-panel.el ends here
