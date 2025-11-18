;;; vkbd.el --- Emacs Virtual Keyboard               -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Keyboard, Input Method, Mouse, Touch

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

;; Usage:
;;   (autoload 'vkbd-toggle-global-keyboard "vkbd" nil t)
;;   M-x vkbd-open-global-keyboard

;; To add a keyboard toggle button to the toolbar:
;;   (autoload 'vkbd-add-to-tool-bar "vkbd-tool-bar" nil t)
;;   Eval (vkbd-add-to-tool-bar) or M-x vkbd-add-to-tool-bar

;; Customization:

;;   Global keyboard settings:
;;   - `vkbd-global-keyboard-options'

;;   Default layout & style:
;;   - `vkbd-default-keyboard-layout'
;;   - `vkbd-default-keyboard-style' (Currently only text01 style)

;;   Text style settings:
;;   - Variables:
;;     - `vkbd-text-key-width'
;;     - `vkbd-text-column-separator-width'
;;     - `vkbd-text-column-separator-display'
;;     - `vkbd-text-row-separator-height'
;;   - Faces:
;;     - `vkbd-text-keyboard'
;;     - `vkbd-text-key-common'
;;     - `vkbd-text-key'
;;     - `vkbd-text-key-invisible'
;;     - `vkbd-text-column-separator'
;;     - `vkbd-text-row-separator'
;;     - `vkbd-title-bar'
;;     - `vkbd-close-button'
;;     - `vkbd-title-caption'

;;; Code:

(require 'cl-lib)

(defgroup vkbd nil
  "Virtual keyboard."
  :tag "vkbd"
  :prefix "vkbd-"
  :group 'keyboard)

(defgroup vkbd-text-style nil
  "Text style in virtual keyboard."
  :tag "vkbd-text-style"
  :prefix "vkbd-"
  :group 'vkbd)

;;;; Logging

(eval-and-compile
  (defcustom vkbd-log-generate nil
    "Control log code generation at compile time."
    :group 'vkbd :type '(choice boolean regexp)))

(defcustom vkbd-log-output nil
  "Control log message output at runtime."
  :group 'vkbd :type '(choice boolean regexp))

(defmacro vkbd-log (format-string &rest args)
  "Log a message if enabled by `vkbd-log-generate' and `vkbd-log-output'."
  (when (if (stringp vkbd-log-generate)
            (string-match-p vkbd-log-generate format-string)
          vkbd-log-generate)
    `(when (if (stringp vkbd-log-output)
               (string-match-p vkbd-log-output ,format-string)
             vkbd-log-output)
       (message ,format-string ,@args))))


;;;; Global Keyboard

;; The global keyboard is a keyboard that is used for the entire Emacs session.

(defcustom vkbd-global-keyboard-options
  nil ;;'(:title "Global Keyboard")
  "Options for the global keyboard.
This is a property list passed to `vkbd-make-keyboard' when creating the
global keyboard."
  :group 'vkbd :type 'plist)

(defvar vkbd-global-keyboard nil
  "The global keyboard buffer, or nil if not created.")

;;;###autoload
(defun vkbd-global-keyboard-open-p ()
  (vkbd-keyboard-live-p vkbd-global-keyboard))

;;;###autoload
(defun vkbd-open-global-keyboard ()
  "Open the global keyboard.
If the global keyboard does not exist or is no longer live, create a new
one."
  (interactive)
  (when (and vkbd-global-keyboard
             (not (vkbd-keyboard-live-p vkbd-global-keyboard)))
    (vkbd-delete-keyboard vkbd-global-keyboard)
    (setq vkbd-global-keyboard nil))
  (unless vkbd-global-keyboard
    (setq vkbd-global-keyboard
          (vkbd-make-keyboard vkbd-global-keyboard-options))))

;;;###autoload
(defun vkbd-close-global-keyboard ()
  "Close the global keyboard if it exists."
  (interactive)
  (when vkbd-global-keyboard
    (vkbd-delete-keyboard vkbd-global-keyboard)
    (setq vkbd-global-keyboard nil)))

;;;###autoload
(defun vkbd-toggle-global-keyboard ()
  "Toggle the global keyboard."
  (interactive)
  (if (vkbd-global-keyboard-open-p)
      (vkbd-close-global-keyboard)
    (vkbd-open-global-keyboard)))


;;;; Keyboard

;; Basic keyboard functions:
;; - `vkbd-make-keyboard'
;; - `vkbd-delete-keyboard'
;; - `vkbd-keyboard-live-p'

(defmacro vkbd-keyboard-property (keyboard prop)
  "Access property PROP from KEYBOARD object."
  `(plist-get (cdr ,keyboard) ,prop))

(defun vkbd-make-keyboard (&optional options)
  "Create a virtual keyboard (on-screen keyboard).

OPTIONS is a property list containing various settings that affect creation.
When nil, everything is created based on default settings.

Return a object that holds all information about the keyboard."
  ;; Fix keyboard layout & style
  (setq options (vkbd-fix-keyboard-layout options))
  (setq options (vkbd-fix-keyboard-style options))

  ;; Create a frame and buffer
  (let* ((frame (vkbd-make-keyboard-frame options '(10 . 10) '(600 . 300)))
         (keyboard (list 'vkbd
                         :options options
                         :frame frame
                         :buffer nil
                         :pressed-modifiers nil))
         (buffer (vkbd-make-keyboard-buffer options keyboard)))
    ;; Connect the buffer and frame.
    (vkbd-set-keyboard-buffer-to-window (frame-root-window frame) buffer)

    ;; Modify the frame position and size.
    (vkbd-fit-keyboard-frame-size-to-buffer-contents options frame)
    (vkbd-initialize-frame-position options frame)

    (setf (vkbd-keyboard-property keyboard :buffer) buffer)
    keyboard))
;; EXAMPLE: (vkbd-make-keyboard)

(defun vkbd-delete-keyboard (keyboard)
  "Delete KEYBOARD, eliminating it from use."
  (let ((frame (vkbd-keyboard-frame keyboard)))
    (when (vkbd-frame-live-p frame)
      ;; Note: The buffer associated with the dedicated window is
      ;; deleted at this point.
      (vkbd-delete-frame frame)))
  ;; Likely already deleted along with the dedicated window, but
  ;; delete just in case
  (let ((buffer (vkbd-keyboard-buffer keyboard)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun vkbd-delete-all-keyboards ()
  "Forcefully delete all keyboard objects.
This function is for debugging purposes."
  (dolist (buffer (buffer-list))
    (when (vkbd-keyboard-buffer-p buffer)
      (vkbd-delete-keyboard buffer))))

(defun vkbd-keyboard-live-p (keyboard)
  "Return non-nil if KEYBOARD is a keyboard which has not been deleted."
  ;; or?
  (and (buffer-live-p (vkbd-keyboard-buffer keyboard))
       (vkbd-frame-live-p (vkbd-keyboard-frame keyboard))))

(defun vkbd-keyboard-options (keyboard)
  "Return the options plist associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :options))

(defun vkbd-keyboard-buffer (keyboard)
  "Return the buffer associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :buffer))

(defun vkbd-keyboard-frame (keyboard)
  "Return the frame associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :frame))

(defun vkbd-keyboard-pressed-modifiers (keyboard)
  "Return the list of currently pressed modifier keys in KEYBOARD."
  (vkbd-keyboard-property keyboard :pressed-modifiers))

(defun vkbd-set-keyboard-pressed-modifiers (keyboard modifiers)
  "Set the currently pressed modifier keys in KEYBOARD to MODIFIERS."
  (setf (vkbd-keyboard-property keyboard :pressed-modifiers) modifiers))

(defun vkbd-event-to-keyboard (event)
  "Return the keyboard object where EVENT occurred, or nil if not in a
keyboard."
  ;; TODO: Use text property?
  (vkbd-keyboard-buffer-keyboard (vkbd-event-to-keyboard-buffer event)))

(defun vkbd-keyboard-key-type-to-events (keyboard key-type)
  "Convert KEY-TYPE to events, updating pressed modifiers in KEYBOARD.

Return a list of events corresponding to KEY-TYPE."
  (let* ((pair (vkbd-key-type-to-events
                key-type (vkbd-keyboard-pressed-modifiers keyboard)))
         (events (car pair))
         (new-pressed-modifiers (cdr pair)))
    (vkbd-set-keyboard-pressed-modifiers keyboard new-pressed-modifiers)
    events))


;;;;; Frame Management

;; Basic frame functions (with recycling mechanism):
;; - `vkbd-make-frame'
;; - `vkbd-delete-frame'
;; - `vkbd-delete-all-unused-frames'
;; - `vkbd-frame-live-p'

(defcustom vkbd-recycle-frames t
  "Non-nil means recycle frames instead of deleting them immediately.
When non-nil, deleted frames are kept in a pool for potential reuse.
When nil, frames are deleted immediately and cannot be reused."
  :type 'boolean
  :group 'vkbd)

(defvar vkbd-unused-frames nil
  "List of unused frames.
This is a pool of unused frames that may be reused by `vkbd-make-frame'
and `vkbd-get-unused-frame'.")

(defun vkbd-get-unused-frame (frame-parameters)
  "Set FRAME-PARAMETERS to an unused frame and return it.
Return nil if there is no unused frame available for reuse."
  (let* ((parent-frame (or (alist-get 'parent-frame frame-parameters)
                           (selected-frame)))
         (frame (seq-find (lambda (frame)
                            (and frame
                                 (frame-live-p frame)
                                 (eq (frame-parent frame) parent-frame)
                                 (not (frame-visible-p frame))))
                          vkbd-unused-frames)))
    (when frame
      (setq vkbd-unused-frames (delq frame vkbd-unused-frames))
      (modify-frame-parameters
       frame (vkbd-remove-unmodifiable-frame-parameters frame-parameters))
      frame)))

(defun vkbd-make-frame (frame-parameters)
  "Create a frame with FRAME-PARAMETERS."
  (let ((frame (or (vkbd-get-unused-frame frame-parameters)
                   (make-frame frame-parameters))))
    frame))

(defun vkbd-delete-frame (frame)
  "Delete FRAME created by `vkbd-make-frame'."
  (if vkbd-recycle-frames
      (vkbd-delete-frame-for-reuse frame)
    (vkbd-delete-frame-immediately frame)))

(defun vkbd-delete-frame-for-reuse (frame)
  "Delete FRAME created by `vkbd-make-frame'.
FRAME is saved as an unused frame and may be recycled."
  (vkbd-kill-dedicated-buffer (frame-root-window frame))

  (let ((parent-frame (and (eq (selected-frame) frame)
                           (frame-parent frame))))
    (make-frame-invisible frame t)
    ;; Invisible frames can interfere with motion events, so move them
    ;; out of the way. (Emacs 29.1 for Windows)
    (set-frame-position frame -1000 -1000)
    ;; Transfor focus to parent
    (when parent-frame
      (select-frame-set-input-focus parent-frame)))
  (setq vkbd-unused-frames (nconc vkbd-unused-frames (list frame)))
  (vkbd-prune-unused-frames))

(defconst vkbd-max-unused-frames-per-parent 2)

(defun vkbd-prune-unused-frames ()
  "Release unnecessary unused frames."
  (setq vkbd-unused-frames
        (cl-loop with parent-alist = nil
                 for frame in vkbd-unused-frames
                 if (and frame
                         (frame-live-p frame)
                         ;; max frame count per same parent
                         (<= (cl-incf (alist-get
                                       (frame-parent frame)
                                       parent-alist 0))
                             vkbd-max-unused-frames-per-parent))
                 collect frame
                 else do (vkbd-delete-frame-immediately frame))))

(defun vkbd-kill-dedicated-buffer (window)
  "Kill the dedicated buffer set in WINDOW."
  (when (and window
             (window-live-p window)
             (window-dedicated-p window))
    (let ((buffer (window-buffer window)))
      (when buffer
        (set-window-dedicated-p window nil)
        (kill-buffer buffer)))))

(defun vkbd-delete-frame-immediately (frame)
  "Delete FRAME immediately.
The deleted frame will not be reused."
  (when (and frame (frame-live-p frame))
    (vkbd-kill-dedicated-buffer (frame-root-window frame))
    (delete-frame frame t)))

(defun vkbd-delete-all-unused-frames ()
  "Delete all unused frames immediately.
`vkbd-unused-frames' will become nil."
  (interactive)
  (mapc #'vkbd-delete-frame-immediately vkbd-unused-frames)
  (setq vkbd-unused-frames nil))

(defun vkbd-frame-live-p (object)
  "Return non-nil if OBJECT is a frame which has not been deleted."
  (and (frame-live-p object)
       (not (memq object vkbd-unused-frames))))


;;;;; Keyboard Frame

(defconst vkbd-unmodifiable-frame-parameters
  '(border-width))

(defun vkbd-remove-unmodifiable-frame-parameters (frame-parameters)
  (seq-remove (lambda (param-value)
                (memq (car param-value) vkbd-unmodifiable-frame-parameters))
              frame-parameters))

(defcustom vkbd-keyboard-frame-parameters
  ;; See: (info "(elisp) Window Frame Parameters")
  '(
    ;; [Basic]
    ;;(display)
    ;;(display-type)
    (title . nil)
    (name . " *Virtual Keyboard Frame*")
    ;;(explicit-name)
    ;; [Position]
    ;;(left . (+ 100))
    ;;(top . 100)
    ;;(icon-left)
    ;;(icon-top)
    (user-position . t)
    (z-group . above)
    ;; [Size]
    ;;(width . (text-pixels . 400))
    ;;(height . (text-pixels . 300))
    ;;(user-size)
    ;;(min-width)
    ;;(min-height)
    ;;(fullscreen)
    ;;(fullscreen-restore)
    ;;(fit-frame-to-buffer-margins)
    ;;(fit-frame-to-buffer-sizes)
    ;; [Layout]
    (border-width . 1)
    (internal-border-width . 0)
    (child-frame-border-width . 4)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (scroll-bar-width . 0)
    (scroll-bar-height . 0)
    (left-fringe . 0)
    (right-fringe . 0)
    (right-divider-width . 0)
    (bottom-divider-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    ;;(tool-bar-position)
    (tab-bar-lines . 0)
    (line-spacing . 0)
    (no-special-glyphs . t)
    ;; [Buffer]
    (minibuffer . nil)
    ;;(buffer-predicate . (lambda (_buffer) nil))
    ;;(buffer-list)
    (unsplittable . t)
    ;; [Frame Interaction]
    ;;(parent-frame . ,(selected-frame))
    ;;(delete-before . <related-frame>)
    ;;(mouse-wheel-frame)
    (no-other-frame . t)
    ;;(auto-hide-function)
    ;;(minibuffer-exit)
    ;;(keep-ratio . nil)
    ;; [Mouse Dragging]
    ;;(drag-internal-border)
    ;;(drag-with-header-line)
    ;;(drag-with-tab-line)
    ;;(drag-with-mode-line)
    ;;(snap-width)
    ;;(top-visible)
    ;;(bottom-visible)
    ;; [Management]
    (visibility . t)
    ;;(auto-raise)
    ;;(auto-lower)
    ;;(icon-type)
    ;;(icon-name)
    ;;(window-id)
    ;;(outer-window-id)
    ;;(wait-for-wm)
    ;;(sticky)
    ;;(inhibit-double-buffering)
    ;;(skip-taskbar)
    ;;(no-focus-on-map)
    ;;(no-accept-focus . t)
    (undecorated . t) ;; No caption, borders, buttons
    ;;(override-redirect . t)
    ;;(ns-appearance)
    ;;(ns-transparent-titlebar)
    ;; [Cursor]
    (cursor-type . nil)
    ;; [Font and Color]
    ;;(font-backend)
    ;;(tty-color-mode)
    ;;(screen-gamma)
    ;;(alpha)
    ;;(alpha-background)
    ;;(font)
    (background-mode . dark)
    (foreground-color . "#ffffff")
    (background-color . "#404040")
    ;;(mouse-color)
    ;;(cursor-color)
    ;;(border-color)
    ;;(scroll-bar-foreground)
    ;;(scroll-bar-background)
    )
  "Frame parameters for keyboard frames."
  :group 'vkbd
  :type 'alist)

(defun vkbd-keyboard-frame-parameters ()
  "Return `vkbd-keyboard-frame-parameters'."
  vkbd-keyboard-frame-parameters)

(defun vkbd-make-keyboard-frame (options position size)
  "Create a frame to display a keyboard.

OPTIONS is a property list of settings.
POSITION is a cons cell (X . Y) specifying the frame position.
SIZE is a cons cell (WIDTH . HEIGHT) specifying the frame size in pixels."
  (let ((before-make-frame-hook nil)
        (after-make-frame-functions nil))
    (vkbd-make-frame
     (append
      `((parent-frame . ,(selected-frame))
        (width . (text-pixels . ,(car size)))
        (height . (text-pixels . ,(cdr size)))
        (left . ,(car position))
        (top . ,(cdr position)))
      (or (plist-get options :frame-parameters)
          (vkbd-keyboard-frame-parameters))))))

(defun vkbd-set-keyboard-buffer-to-window (window buffer)
  "Set BUFFER to WINDOW for displaying the keyboard."
  (let ((old-buffer (window-buffer window)))
    (unless (eq buffer old-buffer)
      ;; Kill previous buffer if window dedicated
      (vkbd-kill-dedicated-buffer window)
      ;; Set new buffer
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t))))

;; `vkbd-text-key-common' faceに:box属性を使うとその:line-widthの分だ
;; け計測サイズが足りなくなる現象があるので、その場合はこれで調整する
;; こと。
(defconst vkbd-keyboard-size-adjustment '(0 . 0))

(defun vkbd-fit-keyboard-frame-size-to-buffer-contents (_options frame)
  "Fit the size of the keyboard FRAME to its buffer contents."
  (let ((size (window-text-pixel-size (frame-root-window frame)
                                      nil nil 10000 10000)))
    (vkbd-log "Fit Size: window-text-pixel-size=%s" size)
    (set-frame-size frame
                    (+ (car size) (car vkbd-keyboard-size-adjustment))
                    (+ (cdr size) (cdr vkbd-keyboard-size-adjustment))
                    t))
  ;; (fit-frame-to-buffer frame)
  )

(defun vkbd-initialize-frame-position (_options frame)
  "Initialize the position of the keyboard FRAME."
  ;; TODO: 位置をoptionsで指定したりカスタマイズ出来るようにする。
  (let* ((parent-edges
          (window-edges (frame-root-window (frame-parent frame)) nil nil t))
         (parent-top (nth 1 parent-edges))
         (parent-w (- (nth 2 parent-edges) (nth 0 parent-edges)))
         (frame-w (frame-outer-width frame)))
    (set-frame-position frame
                        (max 0 (/ (- parent-w frame-w) 2))
                        parent-top)))

(defun vkbd-select-parent-frame (&optional frame)
  "If FRAME has a parent frame, select it and transfer input focus to it."
  (interactive)
  (when-let* ((parent (frame-parent frame)))
    ;; (vkbd-log "Select Parent Frame: (before)selected frame=%s buffer=%s"
    ;;           (selected-frame) (current-buffer))
    (select-frame-set-input-focus parent t)
    ;; (vkbd-log "Select Parent Frame: (after)selected frame=%s buffer=%s"
    ;;           (selected-frame) (current-buffer))
    ))


;;;;; Keyboard Buffer

(defvar-keymap vkbd-keyboard-buffer-mode-map
  "<touchscreen-begin>" #'vkbd-move-keyboard-frame-on-mouse-down
  "<down-mouse-1>" #'vkbd-move-keyboard-frame-on-mouse-down
  "<drag-mouse-1>" #'ignore ;;#'vkbd-select-parent-frame
  "<mouse-1>" #'ignore ;;#'vkbd-select-parent-frame
  "S-<down-mouse-1>" #'ignore
  "S-<drag-mouse-1>" #'ignore
  "S-<mouse-1>" #'ignore
  "C-<down-mouse-1>" #'ignore
  "C-<drag-mouse-1>" #'ignore
  "C-<mouse-1>" #'ignore
  "<down-mouse-2>" #'ignore
  "<drag-mouse-2>" #'ignore
  "<mouse-2>" #'ignore
  "<down-mouse-3>" #'ignore
  "<drag-mouse-3>" #'ignore
  "<mouse-3>" #'ignore
  "<wheel-up>" #'ignore
  "<wheel-down>" #'ignore
  )

(defcustom vkbd-keyboard-buffer-line-spacing nil
  "`line-spacing' value in keyboard buffers."
  :type '(choice (const :tag "Global value" global)
                 (const :tag "No extra space" nil)
                 (integer :tag "In pixels")
                 (float :tag "Relative to the default frame line height"))
  :group 'vkbd-text-style)

(defconst vkbd-keyboard-buffer-name " *Virtual Keyboard*")

(defvar-local vkbd-keyboard-buffer-keyboard nil)

(defun vkbd-make-keyboard-buffer (options keyboard)
  "Create a buffer that holds the keyboard display contents and state.

OPTIONS is a property list of settings.
FRAME is the frame where this keyboard will be displayed."
  (let ((buffer (generate-new-buffer vkbd-keyboard-buffer-name)))
    (with-current-buffer buffer
      (vkbd-keyboard-buffer-mode)
      (setq-local vkbd-keyboard-buffer-keyboard keyboard)
      ;; Update `line-spacing'
      (let ((spec (if (plist-member options :line-spacing)
                      (plist-get options :line-spacing)
                    vkbd-keyboard-buffer-line-spacing)))
        (unless (eq spec 'global)
          (setq-local line-spacing spec)))
      ;; Make buffer contents
      (let ((inhibit-read-only t))
        (vkbd-keyboard-style--make-buffer-contents options))
      (goto-char (point-min)))
    buffer))

(define-derived-mode vkbd-keyboard-buffer-mode nil "VKBD"
  "Major mode for virtual keyboard buffer.
This major mode is for internal use and is not intended for direct user use."
  ;; Initialize local variables
  (defvar tab-bar-format) ;;Emacs 28.1
  (setq-local mode-line-format nil
              header-line-format nil
              tab-line-format nil
              tab-bar-format nil
              truncate-lines nil
              show-trailing-whitespace nil
              display-line-numbers nil
              buffer-read-only t
              truncate-lines t
              vkbd-keyboard-buffer-keyboard nil)
  ;; Ensure global settings
  (vkbd-global-setup))

(defun vkbd-keyboard-buffer-p (object)
  (and (bufferp object)
       (eq (buffer-local-value 'major-mode object)
           'vkbd-keyboard-buffer-mode)))

(defun vkbd-keyboard-buffer-keyboard (buffer)
  "Return the keyboard object associated with keyboard BUFFER."
  (buffer-local-value 'vkbd-keyboard-buffer-keyboard buffer))

;; Frame dragging

(defconst vkbd-frame-move-debounce-time 0.05
  "Debounce time (in seconds) after moving the frame.
Touch event coordinates immediately after moving a frame are unreliable,
so events are ignored until this many seconds have elapsed since the
last move.")

(defun vkbd-move-keyboard-frame-on-mouse-down (down-event)
  (interactive "e")
  (let* ((down-frame (window-frame (posn-window (event-start down-event))))
         (down-xy (vkbd-posn-x-y-on-display (event-start down-event)))
         (down-frame-xy (frame-position down-frame))
         (moved nil)
         (last-frame-moved-time 0.0)
         (touch (eq (car-safe down-event) 'touchscreen-begin))
         (on-move
          (lambda (move-event)
            (when (or (not touch)
                      (>= (- (float-time) last-frame-moved-time)
                          vkbd-frame-move-debounce-time))
              (let* ((curr-xy (vkbd-posn-x-y-on-display (event-start move-event)))
                     (dx (- (car curr-xy) (car down-xy)))
                     (dy (- (cdr curr-xy) (cdr down-xy))))
                (when (and (not moved)
                           (> (+ (* dx dx) (* dy dy))
                              (* double-click-fuzz double-click-fuzz)))
                  (setq moved t))
                (when moved
                  (let ((new-x (+ (car down-frame-xy) dx))
                        (new-y (+ (cdr down-frame-xy) dy)))
                    (set-frame-parameter down-frame 'left (list '+ new-x))
                    (set-frame-parameter down-frame 'top  (list '+ new-y))
                    (setq last-frame-moved-time (float-time)))))))))
    (vkbd-track-drag down-event on-move :on-up on-move
                     :allow-out-of-target-p t)
    moved))

(defun vkbd-move-keyboard-frame-or-close-on-mouse-down (down-event)
  (interactive "e")
  (unless (vkbd-move-keyboard-frame-on-mouse-down down-event)
    (vkbd-delete-frame (window-frame (posn-window (event-start down-event))))))

;;

(defun vkbd-event-in-keyboard-buffer-p (event)
  "Return non-nil if EVENT occurred in a `vkbd-keyboard-buffer-mode' buffer."
  (not (null (vkbd-event-to-keyboard-buffer event))))

(defun vkbd-event-to-keyboard-buffer (event)
  "Return the keyboard buffer where EVENT occurred, or nil if not in a
keyboard buffer."
  (when event
    (let ((window (posn-window (event-start event))))
      (when (windowp window)
        (let ((buffer (window-buffer window)))
          (when (vkbd-keyboard-buffer-p buffer)
            buffer))))))


;;;;; Key Translation

(defun vkbd-current-key-remap-event ()
  "Return the single event from `current-key-remap-sequence' if it contains
exactly one event.
Returns nil otherwise."
  (when (= (length current-key-remap-sequence) 1)
    (aref current-key-remap-sequence 0)))

(defun vkbd-translate-keyboard-buffer-event (prompt)
  "Translate `current-key-remap-sequence' if it occurred in a keyboard buffer.

PROMPT is the prompt string for the current key sequence.

Return the translated key sequence vector, or nil if no translation is needed.
Return an empty vector ([]) to cancel the input event."
  ;; TODO: `vkbd-text-keyboard-translate-event'や
  ;; `vkbd-text-keyboard-key-type-at-event'との役割分担を見直したい。
  ;; 将来的に専用バッファ以外にもキーボードを置けるようにしたいかもしれない。
  ;; それなら処理対象イベント判別の時点で色々と変えなければならなくなる。
  (when-let* ((event (vkbd-current-key-remap-event))
              (buffer (vkbd-event-to-keyboard-buffer event))
              (keyboard (vkbd-keyboard-buffer-keyboard buffer)))
    (vkbd-log "Translate Event: Event occurred in keyboard buffer %s" event)
    (let ((result (vkbd-keyboard-style--translate-event keyboard prompt event)))
      (vkbd-log "Translate Event: Return %s" result)
      result)))


(defconst vkbd-translation-event-types
  '(down-mouse-1
    mouse-1 drag-mouse-1
    touchscreen-begin touchscreen-update touchscreen-end))

(defun vkbd-global-setup ()
  ;; TODO: What to do if it's already in use.
  (dolist (event-type vkbd-translation-event-types)
    (define-key input-decode-map (vector event-type)
                #'vkbd-translate-keyboard-buffer-event)))

(defun vkbd-global-teardown ()
  (dolist (event-type vkbd-translation-event-types)
    (let ((vec (vector event-type)))
      (when (eq (lookup-key input-decode-map vec)
                'vkbd-translate-keyboard-buffer-event)
        (define-key input-decode-map vec nil t)))))


;;;;; Key Modifiers

(defconst vkbd-all-modifiers
  ;; See: (info "(elisp) Keyboard Events")
  '(alt super hyper shift control meta))

(defun vkbd-modifier-p (modifier-key)
  "Return non-nil if MODIFIER-KEY is a valid modifier key."
  (not (null (memq modifier-key vkbd-all-modifiers))))
;; TEST: (vkbd-modifier-p 'meta) => t

(defun vkbd-modifier-bit (modifier-key)
  "Return the modifier bit position for MODIFIER-KEY.
See Info node `(elisp) Keyboard Events' for details on modifier bits.
Return nil if MODIFIER-KEY is not a valid modifier."
  (when-let* ((pos (seq-position vkbd-all-modifiers modifier-key #'eq)))
    ;; See: (info "(elisp) Keyboard Events")
    (+ 22 pos)))
;; TEST: (vkbd-modifier-bit 'meta) => 27

(defun vkbd-modifier-prefix (modifier-key)
  "Return the prefix string for MODIFIER-KEY (e.g., \"M-\" for meta).
Return nil if MODIFIER-KEY is not a valid modifier."
  (when-let* ((pos (seq-position vkbd-all-modifiers modifier-key #'eq)))
    (aref ["A-" "s-" "H-" "S-" "C-" "M-"] pos)))
;; TEST: (vkbd-modifier-prefix 'meta) => "M-"

(defun vkbd-apply-modifier-key (base-key modifier-key)
  "Apply MODIFIER-KEY to BASE-KEY and return the modified key.
If MODIFIER-KEY is nil or invalid, return BASE-KEY unchanged."
  (if-let* ((bit (vkbd-modifier-bit modifier-key))
            (prefix (vkbd-modifier-prefix modifier-key)))
      (event-apply-modifier base-key modifier-key bit prefix)
    base-key))
;; TEST: (vkbd-apply-modifier-key ?a 'shift) => 65
;; TEST: (vkbd-apply-modifier-key 'tab 'shift) => S-tab
;; TEST: (vkbd-apply-modifier-key ?a nil) => 97

(defun vkbd-apply-modifiers (base-key modifiers)
  "Apply all modifier keys in MODIFIERS to BASE-KEY.
MODIFIERS is a list of modifier key symbols.
Return the resulting modified key."
  (dolist (modifier-key modifiers)
    (setq base-key (vkbd-apply-modifier-key base-key modifier-key)))
  base-key)
;; TEST: (vkbd-apply-modifiers 'tab '(shift control)) => C-S-tab

;;;;; Key Types

(defconst vkbd-key-type-alist
  '((esc :text "Esc" :seq (escape))
    (tab :text "Tab" :seq (tab))
    (alt :text "A-" :seq (alt))
    (sup :text "s-" :seq (super))
    (hyp :text "H-" :seq (hyper))
    (shf :text "S-" :seq (shift))
    (ctl :text "C-" :seq (control))
    (met :text "M-" :seq (meta))
    (C-  :text "C-" :seq (control))
    (S-  :text "S-" :seq (shift))
    (M-  :text "M-" :seq (meta))
    (C-c :text "C-c" :seq (control ?c))
    (C-x :text "C-x" :seq (control ?x))
    (C-g :text "C-g" :seq (control ?g))
    (M-x :text "M-x" :seq (meta ?x))
    (spc :text " " :seq (?\x20)) ;; "SPC"
    (bs  :text "BS" :seq (backspace))
    (ret :text "Ret" :seq (return))
    (pup :text "PUp" :seq (prior))
    (pdw :text "PDown" :seq (next))
    (up  :text "↑" :seq (up))
    (lft :text "←" :seq (left)) ;; "<-"
    (dwn :text "↓" :seq (down))
    (rit :text "→" :seq (right)) ;; "->"
    (ins :text "Ins" :seq (insert))
    (del :text "Del" :seq (delete))
    (hom :text "Home" :seq (home))
    (end :text "End" :seq (end))
    (f1  :text "F1" :seq (f1))
    (f2  :text "F2" :seq (f2))
    (f3  :text "F3" :seq (f3))
    (f4  :text "F4" :seq (f4))
    (f5  :text "F5" :seq (f5))
    (f6  :text "F6" :seq (f6))
    (f7  :text "F7" :seq (f7))
    (f8  :text "F8" :seq (f8))
    (f9  :text "F9" :seq (f9))
    (f10  :text "F10" :seq (f10))
    (f11  :text "F11" :seq (f11))
    (f12  :text "F12" :seq (f12))))
;; To customize:
;;   (setf (plist-get (alist-get 'up vkbd-key-type-alist) :text) "Up")

(defun vkbd-key-type-to-key-sequence (key-type)
  "Convert KEY-TYPE to a key sequence list.

KEY-TYPE can be:
- nil: return nil
- An integer (character code): return a single-element list
- A symbol: look up in `vkbd-key-type-alist' and return its :seq property

Return nil if KEY-TYPE is nil or not found in the alist."
  (cond
   ((null key-type)
    nil)
   ((integerp key-type)
    (list key-type))
   ((symbolp key-type)
    (plist-get (alist-get key-type vkbd-key-type-alist) :seq))))
;; TEST: (vkbd-key-type-to-key-sequence ?a) => (97)
;; TEST: (vkbd-key-type-to-key-sequence 'C-c) => (control 99)

(defun vkbd-fold-key-sequence-modifiers (key-sequence-list initial-modifiers)
  "Fold modifier keys in KEY-SEQUENCE-LIST into modified key events.

KEY-SEQUENCE-LIST is a list containing both modifier key symbols and key
events.  Each key event is modified by the currently pressed modifiers
at that point.

INITIAL-MODIFIERS is a list of modifier symbols already pressed.

Return a cons cell (FOLDED-SEQUENCE . PRESSED-MODIFIERS) where:
- FOLDED-SEQUENCE is a list of modified key events (modifier keys
  themselves are not included)
- PRESSED-MODIFIERS is the list of modifiers still pressed after
  processing

For example:
  (vkbd-fold-key-sequence-modifiers
    \\='(?a ?b ?c shift meta return control up control) \\='(meta hyper))
   => ((#x9000061 ?b ?c S-M-return C-up) . (control))"
  (let ((pressed-modifiers (copy-sequence initial-modifiers))
        (folded-key-sequence nil))
    (dolist (key key-sequence-list)
      (if (vkbd-modifier-p key)
          (unless (memq key pressed-modifiers)
            (push key pressed-modifiers))
        (push (vkbd-apply-modifiers key pressed-modifiers) folded-key-sequence)
        (setq pressed-modifiers nil)))
    (cons (nreverse folded-key-sequence) pressed-modifiers)))
;; TEST: (vkbd-fold-key-sequence-modifiers '(?a ?b ?c shift meta return control up control) '(meta hyper)) => ((150995041 98 99 S-M-return C-up) control)
;; TEST: (vkbd-fold-key-sequence-modifiers '(control ?a ?b meta ?c meta control) nil) => ((1 98 134217827) control meta)

(defun vkbd-key-type-to-events (key-type initial-modifiers)
  "Convert KEY-TYPE to events, applying INITIAL-MODIFIERS.

Return a cons cell (EVENTS . PRESSED-MODIFIERS) where:
- EVENTS is a list of key events
- PRESSED-MODIFIERS is the list of modifiers still pressed after conversion"
  (vkbd-fold-key-sequence-modifiers
   (vkbd-key-type-to-key-sequence key-type)
   initial-modifiers))


;;;;; Key Data

;; <key-data>:
;;   <key-type>
;;   ([<no-mod-key-type> [<shifted-key-type>]] [<plist>])
;;
;; <key-type>:
;;   nil          : no key
;;   <symbol>     : key of `vkbd-key-type-alist'
;;   <integer>    : character code point

(defun vkbd-key-data-key-types (key-data)
  "Return ([<no-mod-key-type> [<shifted-key-type>]])."
  (cond
   ((null key-data)
    nil)
   ((consp key-data)
    (cl-loop for x in key-data until (keywordp x) collect x))
   ((or (integerp key-data) (symbolp key-data))
    (list key-data))))
;; TEST: (vkbd-key-data-key-types nil) => nil
;; TEST: (vkbd-key-data-key-types ?a) => (97)
;; TEST: (vkbd-key-data-key-types '(?a)) => (97)
;; TEST: (vkbd-key-data-key-types '(:w 1.5)) => nil
;; TEST: (vkbd-key-data-key-types '(nil :w 1.5)) => (nil)
;; TEST: (vkbd-key-data-key-types '(nil ?a :w 1.5)) => (nil 97)
;; TEST: (vkbd-key-data-key-types '(esc :w 1.5)) => (esc)

(defun vkbd-key-data-properties (key-data)
  (when (consp key-data)
    (cl-loop for x on key-data when (keywordp (car x)) return x)))
;; TEST: (vkbd-key-data-properties nil) => nil
;; TEST: (vkbd-key-data-properties ?a) => nil
;; TEST: (vkbd-key-data-properties '(?a)) => nil
;; TEST: (vkbd-key-data-properties '(:w 1.5)) => (:w 1.5)
;; TEST: (vkbd-key-data-properties '(esc :w 1.5)) => (:w 1.5)

(defun vkbd-key-data-to-key-type (key-data pressed-modifiers)
  (let ((key-types (vkbd-key-data-key-types key-data)))
    (if (memq 'shift pressed-modifiers)
        (or (cadr key-types)
            (let ((kt (car key-types)))
              (if (integerp kt)
                  (upcase kt) ;; TODO: Customize?
                kt)))
      (car key-types))))
;; TEST: (vkbd-key-data-to-key-type nil nil) => nil
;; TEST: (vkbd-key-data-to-key-type nil '(shift control)) => nil
;; TEST: (vkbd-key-data-to-key-type ?a nil) => 97
;; TEST: (vkbd-key-data-to-key-type ?a '(shift)) => 65
;; TEST: (vkbd-key-data-to-key-type ?a '(shift control)) => 65
;; TEST: (vkbd-key-data-to-key-type 'esc '(shift control)) => esc
;; TEST: (vkbd-key-data-to-key-type '(?a :w 2) '(shift control)) => 65
;; TEST: (vkbd-key-data-to-key-type '(?2 ?\") '(shift control)) => 34


;;;;; Text Keyboard Style

;;;;;; Text Keyboard Input

(defun vkbd-text-keyboard-key-type-at-event (event)
  "Return the key type at the position where EVENT occurred."
  ;; TODO: vkbd-keyboardテキストプロパティがあればキーボードバッファで
  ;; なくても動作するようにしたい。vkbd-keyboardテキストプロパティから
  ;; キーボードオブジェクトを取得し、vkbd-key-dataテキストプロパティか
  ;; らキー入力値を割り出す。そうすれば(専用のバッファやフレームを伴わ
  ;; ない)任意のバッファ内にキーボードを配置できるようになるはず。
  ;; `vkbd-translate-keyboard-buffer-event'から変えないといけないけど。
  ;; `vkbd-event-to-keyboard'や`vkbd-keyboard-style--translate-event'
  ;; にも影響がある。
  (when-let* ((buffer (vkbd-event-to-keyboard-buffer event))
              (keyboard (vkbd-keyboard-buffer-keyboard buffer)))
    (vkbd-key-data-to-key-type
     (with-current-buffer buffer
       (get-text-property (posn-point (event-start event)) 'vkbd-key-data))
     (vkbd-keyboard-pressed-modifiers keyboard))))

(defun vkbd-text-keyboard-translate-event (keyboard _prompt event)
  (when-let* ((key-type (vkbd-text-keyboard-key-type-at-event event)))
    (vkbd-log "Translate Event: key-type=%s" key-type)
    (cond
     ;; ((memq (car-safe event) '(down-mouse-1 touchscreen-begin))
     ;;  (vkbd-post-key keyboard key-type)
     ;;  ;; pressed
     ;;  )
     ((memq (car-safe event) '(mouse-1 touchscreen-end))
      (vkbd-select-parent-frame)
      (apply #'vector (vkbd-keyboard-key-type-to-events keyboard key-type)))
     (t
      ;; Return [] if EVENT occurs at a key position.
      ;; Return nil if EVENT occurs at a non-key position.
      []))))


;;;;;; Text Keyboard Appearance

(defface vkbd-text-keyboard
  '((t (:inherit
        ;; fixed-pitch
        default
        :height 1.0)))
  "Face for all text in keyboard."
  :group 'vkbd-text-style)

(defface vkbd-text-key-common
  ;; See: `custom-button' face
  '((((type x w32 ns haiku pgtk android) (class color)
      (min-colors 88))	; Like default mode line
     :inherit vkbd-text-keyboard
     ;; :box (:line-width 4 :style flat-button)
     )
    (((type x w32 ns haiku pgtk android))
     :inherit vkbd-text-keyboard
     ;; :box (:line-width 4 :style flat-button)
     ))
  "Face for all keys."
  :group 'vkbd-text-style)

(defface vkbd-text-key
  '((((type x w32 ns haiku pgtk android) (class color)
      (min-colors 88))	; Like default mode line
     :inherit vkbd-text-key-common
     :background "lightgrey" :foreground "black")
    (((type x w32 ns haiku pgtk android))
     :inherit vkbd-text-key-common
     :background "white" :foreground "black"))
  "Face for normal keys."
  :group 'vkbd-text-style)

(defface vkbd-text-key-invisible
  '((t :inherit vkbd-text-key-common))
  "Face for invisible keys."
  :group 'vkbd-text-style)

(defface vkbd-text-column-separator
  '((t (:inherit vkbd-text-keyboard
                 :height 1.6)))
  "Face for spacing between columns."
  :group 'vkbd-text-style)

(defface vkbd-text-row-separator
  '((t (:inherit vkbd-text-keyboard)))
  "Face for spacing between rows."
  :group 'vkbd-text-style)

(defface vkbd-title-bar
  '((t (:inherit vkbd-text-keyboard)))
  "Face for title bars."
  :group 'vkbd-text-style)

(defface vkbd-close-button
  '((t (:inherit vkbd-title-bar
                 :background "#505050")))
  "Face for close buttons."
  :group 'vkbd-text-style)

(defface vkbd-title-caption
  '((t (:inherit vkbd-title-bar
                 :foreground "#c0c0c0")))
  "Face for title text."
  :group 'vkbd-text-style)

(defun vkbd-get-face-opt (options face-name)
  (or (when-let* ((faces (plist-get options :faces))) ;; alist
        (alist-get face-name faces))
      face-name))
;; TEST: (vkbd-get-face-opt '(:faces ((face-a . (:height 2)))) 'face-a) => (:height 2)
;; TEST: (vkbd-get-face-opt '(:faces ((face-a . (:height 2)))) 'face-b) => face-b

(defcustom vkbd-text-key-width 5
  "Width of one key (number of characters)."
  :type 'integer
  :group 'vkbd-text-style)

(defun vkbd-text-key-width (options &optional key-width-ratio)
  "Return the width (number of characters) of one key."
  (round
   (* (or (plist-get options :text-key-width)
          vkbd-text-key-width)
      (or key-width-ratio 1.0))))

(defun vkbd-text-key-centering (options text &optional key-width-ratio)
  ;; TODO: Use display property (space :width 0.x)
  (let ((key-width (round
                    (* (or (plist-get options :text-key-width)
                           vkbd-text-key-width)
                       (or key-width-ratio 1.0))))
        (text-width (string-width text)))
    (when (< text-width key-width)
      (let* ((short-width (- key-width text-width))
             (left-width (/ short-width 2))
             (right-width (- short-width left-width)))
        (setq text (concat (make-string left-width ?\s)
                           text
                           (make-string right-width ?\s)))))
    (truncate-string-to-width text key-width)))

(defun vkbd-key-type-string (_options key-type &optional _key-width)
  (cond
   ((null key-type) nil)
   ((integerp key-type)
    (char-to-string key-type))
   ((symbolp key-type)
    ;; TODO: Select a string that matches KEY-WIDTH ("Ctl" "Ctrl" "Control")
    (plist-get (alist-get key-type vkbd-key-type-alist) :text))))

(defcustom vkbd-text-key-raise '((0.0) (0.0 0.5))
  "Vertical position of characters when displaying multiple characters per key.

Format:
  ((raise-without-shift) (raise-with-shift-char1 raise-with-shift-char2))

Each value is a multiple of the text height used for the `raise' display
property. Positive values raise text above the baseline; negative values
lower it."
  :type '(list :tag "By condition"
               (list :tag "Without shift"
                     (float :tag "Raise factor"))
               (list :tag "With shift"
                     (float :tag "First character raise factor")
                     (float :tag "Second character raise factor")))
  :group 'vkbd)

(defun vkbd-text-keyboard-key-data-string (options key-data)
  "Generate the display string for a key based on KEY-DATA.
OPTIONS specifies the keyboard configuration."
  (let* ((key-types (vkbd-key-data-key-types key-data))
         (key-props (vkbd-key-data-properties key-data))
         (key-width-ratio (or (plist-get key-props :width)
                              (plist-get key-props :w)))
         (key-width (vkbd-text-key-width options key-width-ratio))
         (num-types (length key-types))
         (width-per-type (max 1 (/ key-width (max 1 num-types))))
         (raise-spec (or (plist-get options :text-key-raise)
                         vkbd-text-key-raise))
         (type-strs
          (cl-loop
           for type in key-types
           for type-index from 0
           for raise-factor = (nth type-index (nth (1- num-types) raise-spec))
           for str = (or (vkbd-key-type-string options type width-per-type) " ")
           collect (if raise-factor
                       (propertize str 'display (list 'raise raise-factor))
                     str)))
         (type-strs-total-width (apply #'+ (mapcar #'string-width type-strs)))
         (text
          (when type-strs
            (mapconcat
             #'identity type-strs
             ;; Add separators only if there is enough whitespace
             ;; [ab] [ab ] [ ab ] [ a b ] [ a b  ] [  a b  ]
             (if (>= (- (- key-width 2) type-strs-total-width) (1- num-types))
                 " "
               nil)))))
    (if text
        (vkbd-insert-propertized
         (vkbd-text-key-centering options text key-width-ratio)
         'face (vkbd-get-face-opt options 'vkbd-text-key)
         'vkbd-key-data key-data
         'pointer 'hand)
      (vkbd-insert-propertized
       (vkbd-text-key-centering options "" key-width-ratio)
       'face (vkbd-get-face-opt options 'vkbd-text-key-invisible)
       'pointer 'arrow))))

(defun vkbd-insert-text-key-data (options key-data)
  (let ((text (vkbd-text-keyboard-key-data-string options key-data)))
    (when (stringp text)
      (insert text))))

(defcustom vkbd-text-column-separator-width 0.25
  "Width of spacing between columns (horizontal spacing between keys)."
  :type 'float
  :group 'vkbd-text-style)

(defconst vkbd-text-column-separator-display 'space) ;; or "|"

(defun vkbd-insert-text-column-separator (options)
  (let ((width (or (plist-get options :text-column-separator-width)
                   vkbd-text-column-separator-width)))
    (when (and (numberp width) (> width 0))
      (vkbd-insert-propertized
       " "
       'display
       (if (stringp vkbd-text-column-separator-display)
           vkbd-text-column-separator-display
         `(space :width ,width))
       'face (vkbd-get-face-opt options 'vkbd-text-column-separator)))))

(defcustom vkbd-text-row-separator-height 0.1
  "Height of spacing between rows (vertical spacing between keys)."
  :type 'float
  :group 'vkbd-text-style)

(defun vkbd-insert-text-row-separator (options)
  (when (display-graphic-p)
    (let ((height (or (plist-get options :text-row-separator-height)
                      vkbd-text-row-separator-height)))
      (when (and (numberp height) (> height 0))
        (vkbd-insert-propertized
         "\n" 'face `(:inherit vkbd-text-row-separator
                               :height ,height
                               ))))))

(defun vkbd-insert-text-keyboard (options)
  (cl-loop for row in (vkbd-default-keyboard-layout options)
           for first-row = t then nil
           do
           (unless first-row
             (vkbd-insert-text-row-separator options))
           (cl-loop for key-data in row
                    for first-column = t then nil
                    do
                    (unless first-column
                      (vkbd-insert-text-column-separator options))
                    (vkbd-insert-text-key-data options key-data))
           (insert "\n")))

(defun vkbd-insert-default-title (options)
  (vkbd-insert-close-button options)
  (let ((title (plist-get options :title)))
    (when (stringp title)
      (vkbd-insert-propertized
       title 'face (vkbd-get-face-opt options 'vkbd-title-caption))))
  (vkbd-insert-propertized
   "\n" 'face (vkbd-get-face-opt options 'vkbd-title-bar))
  (vkbd-insert-text-row-separator options))

(defun vkbd-on-close-button-click (event)
  (interactive "e")
  (vkbd-log "on-close-button-click")
  (when-let* ((keyboard (vkbd-event-to-keyboard event)))
    (vkbd-delete-keyboard keyboard)))

(defun vkbd-insert-propertized (string &rest properties)
  (unless (plist-member properties 'face)
    (setq properties (cons 'face
                           (cons 'vkbd-text-keyboard properties))))
  (unless (plist-member properties 'font-lock-face)
    (setq properties (cons 'font-lock-face
                           (cons (plist-get properties 'face) properties))))
  (unless (plist-member properties 'pointer)
    (setq properties (cons 'pointer (cons 'arrow properties))))
  (insert (apply #'propertize string properties)))

(defconst vkbd-text-keyboard-close-button-caption "  x  ")

(defun vkbd-insert-close-button (options)
  (vkbd-insert-propertized
   vkbd-text-keyboard-close-button-caption
   'face (vkbd-get-face-opt options 'vkbd-close-button)
   'pointer 'hand
   'keymap
   (let ((km (make-sparse-keymap)))
     (define-key km [down-mouse-1] #'ignore)
     (define-key km [mouse-1] #'vkbd-on-close-button-click)
     (define-key km [touchscreen-begin] #'ignore)
     (define-key km [touchscreen-update] #'ignore)
     (define-key km [touchscreen-end] #'vkbd-on-close-button-click)
     km))
  (vkbd-insert-propertized " " 'keymap (make-sparse-keymap)))


;;;;; Text01 Style

(defun vkbd-text01-insert-keys (options)
  (vkbd-insert-text-keyboard options))

(defconst vkbd-text01-style
  '(vkbd-text01-style
    :insert-keys vkbd-text01-insert-keys
    :translate-event vkbd-text-keyboard-translate-event
    ))


;;;;; Text02 Style
;; TODO: Impl Text02 Style

;;;;; SVG01 Style
;; TODO: Impl SVG01 Style

;;;;; Keyboard Layouts

(defconst vkbd-layout-10x9
  '((esc ?~  ?^  ?`  ?_  ?|  ?\\ ?{  ?}  ?*)
    (tab ?:  ?<  ?=  ?>  ??  ?@  ?\[ ?\] ?+)
    (?!  ?\" ?#  ?$  ?%  ?&  ?'  ?\( ?\) ?-)
    (?1  ?2  ?3  ?4  ?5  ?6  ?7  ?8  ?9  ?0)
    (?q  ?w  ?e  ?r  ?t  ?y  ?u  ?i  ?o  ?p)
    (?a  ?s  ?d  ?f  ?g  ?h  ?j  ?k  ?l  ?\;)
    (?z  ?x  ?c  ?v  ?b  ?n  ?m  ?,  ?.  ?/)
    (shf ctl met spc ins pup hom up  end bs)
    (M-x C-x C-c C-g del pdw lft dwn rit ret))
  "Layout that allows entering all ASCII symbols without shift.
The width is limited to 10 keys, making it easy to use on smartphones.")

(defconst vkbd-layout-10x7
  '((esc tab (?` ?~) (?' ?!) (?\" ?#) (?: ?$) (?\( ?%) (?\) ?^) (?- ?&) shf)
    (?1 ?2 ?3 ?4 ?5 ?6 ?7 (?8 ?|)  (?9 ?\\) (?0 ?=))
    (?q ?w ?e ?r ?t ?y ?u (?i ?{)  (?o ?})  (?p ?*))
    (?a ?s ?d ?f ?g ?h ?j (?k ?\[) (?l ?\]) (?\; ?+))
    (?z ?x ?c ?v ?b ?n ?m (?, ?<)  (?. ?>)  (?/ ?_))
    (shf ctl met hom end pup ?@  up  ??  bs)
    (M-x C-x C-c C-g spc pdw lft dwn rit ret))
  "Compact version of `vkbd-layout-10x9' with 7 rows.
Most symbols require shift to enter.")

(defconst vkbd-layout-10x6
  '(((?1 ?!) (?2 ?\") (?3 ?#) (?4 ?$) (?5 ?%) (?6 ?&) (?7 ?')
     (?8 ?\() (?9 ?\)) (?0 ?=))
    (?q ?w ?e ?r (?t ?-) (?y ?~)  (?u ?`) (?i ?{)  (?o ?})  (?p ?*))
    (?a ?s ?d ?f (?g ?@) (?h ?|)  (?j ?^) (?k ?\[) (?l ?\]) (?\; ?+))
    (?z ?x ?c ?v (?b ?:) (?n ?\\) (?m ??) (?, ?<)  (?. ?>)  (?/ ?_))
    (esc shf ctl met spc pup hom up  end bs)
    (tab M-x C-x C-c C-g pdw lft dwn rit ret))
  "Compact version of `vkbd-layout-10x7' with 6 rows.
Almost all symbols require shift to enter. Ins and Del keys are removed.")

(defconst vkbd-layout-11x7
  '((esc tab ?^  ?@  ?*  ?+  ?-      ?=      ?\(      ?\)      shf)
    (?1  ?2  ?3  ?4  ?5  ?6  (?7 ?!) (?8 ?#) (?9 ?$)  (?0 ?%)  (?` ?~))
    (?q  ?w  ?e  ?r  ?t  ?y  ?u      ?i      (?o ?{)  (?p ?})  (?' ?&))
    (?a  ?s  ?d  ?f  ?g  ?h  ?j      ?k      (?l ?\[) (?: ?\]) (?\" ?|))
    (?z  ?x  ?c  ?v  ?b  ?n  ?m      ?,      (?. ?<)  (?\; ?>) (?/ ?\\))
    (shf ctl met hom end ins pup ?_  up  ??  bs)
    (M-x C-x C-c C-g spc del pdw lft dwn rit ret))
  "Compact layout based on `vkbd-layout-10x7' with one additional column.
Allows entering more symbols without shift while maintaining compactness.")

(defconst vkbd-layout-jp
  '((esc (:w 0.5) f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12)
    (nil (?1 ?!) (?2 ?\") (?3 ?#) (?4 ?$) (?5 ?%) (?6 ?&)
         (?7 ?') (?8 ?\() (?9 ?\)) ?0 (?- ?=) (?^ ?~) (?\\ ?|) bs)
    ((tab :w 1.5)
     ?q  ?w  ?e  ?r  ?t  ?y  ?u  ?i  ?o  ?p  (?@ ?`) (?\[ ?{) (ret :w 1.4))
    ((ctl :w 2.0)
     ?a  ?s  ?d  ?f  ?g  ?h  ?j  ?k  ?l  (?\; ?+) (?: ?*) (?\] ?}) ctl)
    ((shf :w 2.5)
     ?z  ?x  ?c  ?v  ?b  ?n  ?m  (?, ?<) (?. ?>) (?/ ??) (?\\ ?_) (shf :w 1.5))
    (alt sup hyp met nil (spc :w 3) nil ins hom pup nil up)
    (M-x C-x C-c C-g nil (nil :w 3) nil del end pdw lft dwn rit))
  "Japanese keyboard-like layout.")

(defconst vkbd-layout-us
  '((esc (:w 0.5) f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12)
    ((?~ ?`) (?1 ?!) (?2 ?@) (?3 ?#) (?4 ?$) (?5 ?%) (?6 ?^)
         (?7 ?&) (?8 ?*) (?9 ?\() (?0 ?\)) (?- ?_) (?= ?+) (bs :w 1.4))
    ((tab :w 1.4)
     ?q  ?w  ?e  ?r  ?t  ?y  ?u  ?i  ?o  ?p  (?\[ ?\{) (?\] ?\}) (?\\ ?|))
    ((ctl :w 1.6)
     ?a  ?s  ?d  ?f  ?g  ?h  ?j  ?k  ?l  (?\; ?:) (?' ?\") (ret :w 1.8))
    ((shf :w 2.2)
     ?z  ?x  ?c  ?v  ?b  ?n  ?m  (?, ?<) (?. ?>) (?/ ??) (shf :w 2.2))
    (alt sup hyp met (:w 0.5) (spc :w 3) (:w 0.5) ins hom pup (:w 0.5) nil up)
    (M-x C-x C-c C-g (:w 0.5) (nil :w 3) (:w 0.5) del end pdw (:w 0.5) lft dwn rit))
  "US keyboard-like layout.")

(defcustom vkbd-default-keyboard-layout 'vkbd-layout-10x7
  "Default keyboard layout to use.

This can be either a symbol naming a variable that holds a layout list,
or a layout list itself."
  :type '(choice (const vkbd-layout-10x9)
                 (const vkbd-layout-10x7)
                 (const vkbd-layout-10x6)
                 (const vkbd-layout-11x7)
                 (const vkbd-layout-us)
                 (const vkbd-layout-jp)
                 (symbol :tag "Variable name")
                 sexp)
  :group 'vkbd)

(defun vkbd-resolve-keyboard-layout-spec (layout)
  "Convert a layout specification LAYOUT to a layout list.

LAYOUT can be a symbol (variable name) or a list."
  (or
   ;; Variable name
   (when (and (symbolp layout) (not (null layout)))
     (default-value layout))
   ;; List of list
   (when (and (consp layout) (listp (car layout)))
     layout)))

(defun vkbd-default-keyboard-layout (options)
  "Return the default layout list to use.

OPTIONS is a property list that may contain a `:layout' property."
  (or (vkbd-resolve-keyboard-layout-spec (plist-get options :layout))
      (vkbd-resolve-keyboard-layout-spec vkbd-default-keyboard-layout)
      vkbd-layout-10x9))

(defun vkbd-fix-keyboard-layout (options)
  "Fix the `:layout' property in OPTIONS to a concrete layout list.

This function resolves the layout specification to an actual layout list
and sets it in the `:layout' property. This prevents malfunctions that
could occur if the layout specification (like
`vkbd-default-keyboard-layout') changes while the keyboard is in use."
  (plist-put (seq-copy options)
             :layout (vkbd-default-keyboard-layout options)))

;;;;; Keyboard Styles

;; Style Definition:
;; (defconst <style-name>
;;   '(<style-name>
;;     :insert-title <function (options):void>
;;     :insert-keys <function (options):void>
;;     :make-buffer-contents <function (options):void>
;;     :traslate-function <function (options prompt event):nil|event-vector>
;;    )

(defcustom vkbd-default-keyboard-style 'vkbd-text01-style
  "Default keyboard style to use.

This can be either a symbol naming a variable that holds a style
descriptor, or a style descriptor itself."
  :type '(choice (const vkbd-text01-style)
                 (symbol :tag "Variable name")
                 sexp)
  :group 'vkbd)

(defun vkbd-resolve-keyboard-style-spec (style)
  "Convert a style specification STYLE to a style descriptor.

STYLE can be a symbol (variable name) or a style descriptor list."
  (or
   ;; Variable name
   (when (and (symbolp style) (not (null style)))
     (default-value style))
   ;; Style descriptor
   (when (and (consp style) (symbolp (car style)))
     style)))

(defun vkbd-default-keyboard-style (&optional options)
  "Return the default style descriptor to use.

OPTIONS is a property list that may contain a `:style' property."
  (or (vkbd-resolve-keyboard-style-spec (plist-get options :style))
      (vkbd-resolve-keyboard-style-spec vkbd-default-keyboard-style)
      vkbd-text01-style))
;; (vkbd-default-keyboard-style nil)

(defun vkbd-fix-keyboard-style (options)
  "Fix the `:style' property in OPTIONS to a concrete style descriptor.

This prevents malfunctions if the style specification changes while
the keyboard is in use."
  (plist-put (seq-copy options)
             :style (vkbd-default-keyboard-style options)))

(defun vkbd-keyboard-style-plist (options)
  "Return the property list of the keyboard style in OPTIONS."
  (cdr (vkbd-default-keyboard-style options)))

(defun vkbd-keyboard-style-property (options prop &optional default)
  "Get property PROP from the keyboard style in OPTIONS.

Return DEFAULT if PROP is not found."
  (or
   (plist-get (vkbd-keyboard-style-plist options) prop)
   default))

(defun vkbd-keyboard-style-funcall (options prop &optional default &rest args)
  "Call the function specified by property PROP in the keyboard style.

OPTIONS specifies the keyboard style.
If PROP is not found or not a function, use DEFAULT instead.
ARGS are passed to the function."
  (let ((fun (vkbd-keyboard-style-property options prop default)))
    (when (functionp fun)
      (apply fun args))))

(defun vkbd-keyboard-style--insert-title (options)
  "Insert the keyboard title bar using the style specified in OPTIONS."
  (vkbd-keyboard-style-funcall
   options :insert-title #'vkbd-insert-default-title
   options))

(defun vkbd-keyboard-style--insert-keyboard (options)
  "Insert the keyboard keys using the style specified in OPTIONS."
  (vkbd-keyboard-style-funcall
   options :insert-keys #'vkbd-insert-default-title
   options))

(defun vkbd-keyboard-style--make-buffer-contents-default (options)
  "Insert the keyboard buffer contents using the style specified in OPTIONS."
  (vkbd-keyboard-style--insert-title options)
  (vkbd-keyboard-style--insert-keyboard options))

(defun vkbd-keyboard-style--make-buffer-contents (options)
  "Insert the keyboard buffer contents using the style specified in OPTIONS."
  (vkbd-keyboard-style-funcall
   options :make-buffer-contents
   #'vkbd-keyboard-style--make-buffer-contents-default
   options))

(defun vkbd-keyboard-style--translate-event (options prompt event)
  "Translate EVENT using the style specified in OPTIONS.

PROMPT is the prompt string for the current key sequence."
  (vkbd-keyboard-style-funcall
   options :translate-event nil
   options prompt event))


;;;; Utilities

;;;;; Echo Area

(defun vkbd-echo (string)
  (let ((message-log-max nil))
    (if (and (stringp string) (not (string= string "")))
        (message "%s" string)
      (message nil))))


;;;;; Events

(defun vkbd-posn-same-object-p (pos1 pos2 &optional target)
  (and (eq (posn-window pos1)
           (posn-window pos2))
       (if (memq target '(point object nil))
           (equal (posn-point pos1)
                  (posn-point pos2))
         t)
       (if (memq target '(object nil))
           (eq (car (posn-object pos1)) ;;ex: 'image
               (car (posn-object pos2))) ;;ex: 'image
         t)))

(defsubst vkbd-touch-event-assoc (event id)
  "If touchscreen EVENT has a point matching ID, return its POSN."
  (pcase (car-safe event)
    ('touchscreen-update
     ;; (touchscreen-update ( (id . posn) ... ))
     (cdr (assoc id (cadr event))))
    ((or 'touchscreen-begin 'touchscreen-end)
     ;; (touchscreen-(begin|end) (id . posn)
     (let ((point (cadr event)))
       (when (eq (car point) id)
         (cdr point))))))

(defun vkbd-read-event-silent (&optional not-keep-echo-area)
  ;; Suppress display of events
  ;; (e.g. down-mouse-1-) in echo area
  (let ((echo-keystrokes 0))
    (if not-keep-echo-area
        (read-event)
      (let ((old-message (current-message)))
        (prog1 (read-event)
          (vkbd-echo old-message))))))

;;;;;; Input Event Coordinates

(defun vkbd-posn-x-y-on-display (position &optional default-inside-window-p)
  (let* ((window-or-frame (posn-window position))
         (frame (cond
                 ((windowp window-or-frame) (window-frame window-or-frame))
                 ((framep window-or-frame) window-or-frame))))
    (when frame
      (let ((xy (vkbd-posn-x-y-on-frame position default-inside-window-p))
            (frame-xy (frame-position frame)))
        (cons
         (+ (car frame-xy) (car xy))
         (+ (cdr frame-xy) (cdr xy)))))))

(defun vkbd-posn-x-y-on-frame (position &optional default-inside-window-p)
  "Convert POSITION to frame coordinates.

POSITION should be a list of the form returned by `event-start'
and `event-end'.

If area of POSITION is an unknown location and
DEFAULT-INSIDE-WINDOW-P is non-nil, the event is assumed to be
within window and the result is returned. If it is nil, it
returns nil."
  (let* ((window-or-frame (posn-window position))
         (window (and (windowp window-or-frame) window-or-frame))
         (area (posn-area position))
         (xy (posn-x-y position))
         (x (car xy))
         (y (cdr xy)))
    (cond
     (window
      (let ((edges (cond
                    ((null area)
                     (window-inside-pixel-edges window))
                    ((memq area '(horizontal-scroll-bar
                                  mode-line header-line tab-line))
                     (window-pixel-edges window))
                    ((memq area '(left-margin
                                  left-fringe
                                  ;; Right
                                  right-fringe
                                  right-margin
                                  vertical-line
                                  vertical-scroll-bar))
                     (let ((win (window-pixel-edges window))
                           (ins (window-inside-pixel-edges window)))
                       ;; Just to make sure, do the processing when
                       ;; x is not a number.
                       (unless (numberp x)
                         (setq x (if (memq area '(left-margin left-fringe))
                                     (nth 0 ins)
                                   (nth 2 ins))))
                       ;; x=win, y=ins
                       ;; NOTE: Elisp manual says "x does not have
                       ;; meaningful data" But at least in
                       ;; MS-Windows x means the coordinate from the
                       ;; left edge of the window.
                       (list (nth 0 win) (nth 1 ins)
                             (nth 2 win) (nth 3 ins))))
                    (default-inside-window-p
                     (window-inside-pixel-edges window)))))
        (when edges
          (cons (+ x (car edges))
                (+ y (cadr edges))))))
     ((framep window-or-frame)
      xy))))

(defun vkbd-posn-delta-xy-frame-to-object (down-pos)
  "Calculate coordinate delta from frame to image."
  (let* ((down-xy-on-frame (vkbd-posn-x-y-on-frame down-pos))
         (down-xy-on-object (posn-object-x-y down-pos)))
    (and down-xy-on-frame
         (cons (- (car down-xy-on-object) (car down-xy-on-frame))
               (- (cdr down-xy-on-object) (cdr down-xy-on-frame))))))

;;;;;; Drag Tracking

(cl-defun vkbd-track-drag (down-event
                           on-move
                           &key
                           on-up on-leave target
                           allow-pointer-shape-change-p
                           allow-out-of-target-p
                           not-keep-echo-area
                           need-intermediate-points
                           (use-timer t))
  (cond
   ;; Start with touchscreen-begin event
   ((eq (car-safe down-event) 'touchscreen-begin)
    (vkbd-track-drag--touchscreen
     down-event on-move on-up on-leave target allow-out-of-target-p
     not-keep-echo-area
     need-intermediate-points
     use-timer))
   ;; Start with down-mouse-(1|2|3) event
   ((memq 'down (event-modifiers down-event))
    (vkbd-track-drag--mouse
     down-event on-move on-up on-leave target allow-out-of-target-p
     not-keep-echo-area
     allow-pointer-shape-change-p))
   (t
    (error "down-event is not down event. %s" (event-modifiers down-event)))))

(defun vkbd-track-drag--mouse (down-event
                               on-move
                               on-up on-leave target
                               allow-out-of-target-p
                               not-keep-echo-area
                               allow-pointer-shape-change-p)
  (vkbd-log "Track Drag: mouse")
  (let* ((down-basic-type (event-basic-type down-event))
         (down-posn (event-start down-event))
         (result nil)
         ;; Generate detailed movement events even on characters,
         ;; fringes and scrollbars
         (mouse-fine-grained-tracking t))
    (track-mouse ;; Enable mouse-movement events
      (unless allow-pointer-shape-change-p
        (setq track-mouse 'dragging))
      (while (null result)
        (let ((event (vkbd-read-event-silent not-keep-echo-area)))
          (vkbd-log "Track Drag: Read event %s" (car-safe event))
          (cond
           ;; mouse-movement
           ((mouse-movement-p event)
            (when-let* ((posn (event-start event)))
              (setq result (vkbd-track-drag--on-move
                            event on-move on-leave target
                            allow-out-of-target-p down-posn))))
           ;; mouse up
           ((or
             (and (eq (event-basic-type event) down-basic-type)
                  (or (memq 'click (event-modifiers event))
                      (memq 'drag (event-modifiers event))))
             ;; Just in case.
             ;; If touchscreen-begin is converted to down-mouse-1, then
             ;; up-mouse-1 will not come and touchscreen-end will come.
             (eq (car-safe event) 'touchscreen-end))
            (setq result (vkbd-track-drag--on-up event on-up)))
           (t
            (setq result (vkbd-track-drag--on-unknown-event event on-up)))))))
    result))

(defconst vkbd-track-drag--skip-move-delay 0.3)

(defun vkbd-track-drag--touchscreen (down-event
                                      on-move
                                      on-up on-leave target
                                      allow-out-of-target-p
                                      not-keep-echo-area
                                      need-intermediate-points
                                      use-timer)
  (vkbd-log "Track Drag: touchscreen")
  (let* ((down-posn (event-start down-event))
         (down-point-id (caadr down-event))
         (pending-events nil)
         (timer nil)
         (result nil)
         on-timer)
    (setq on-timer
          (lambda ()
            (setq timer nil)
            (when pending-events
              (vkbd-log "Track Drag: Dispatch pending-events %d"
                         (length pending-events))
              ;; Use only the last event if not NEED-INTERMEDIATE-POINTS
              (when (and (not need-intermediate-points) (cdr pending-events))
                (vkbd-log "Track Drag: Skip delayed events %d"
                           (1- (length pending-events)))
                (setcdr pending-events nil))

              ;; Reorder events from oldest to newest
              (setq pending-events (nreverse pending-events))

              ;; Dispatch on-move or on-leave
              (while (and (null result) pending-events)
                (let* ((event-time-posn (pop pending-events))
                       (event-time (car event-time-posn))
                       (event-posn (cdr event-time-posn))
                       (delay (- (float-time) event-time)))
                  (when (or (null pending-events)
                            (< delay vkbd-track-drag--skip-move-delay))
                    (setq result
                          (vkbd-track-drag--on-move
                           ;; Convert to mouse-movement event
                           (list 'mouse-movement event-posn)
                           on-move on-leave target allow-out-of-target-p
                           down-posn))))))))
    (while (null result)
      (let* ((event (vkbd-read-event-silent not-keep-echo-area))
             (posn (vkbd-touch-event-assoc event down-point-id)))
        (vkbd-log "Track Drag: Read event %s" (car-safe event))

        (unless result ;; Might be set to nil from the timer in read-event?
          (if posn ;; -end or -update, and matching DOWN-POINT-ID
              (pcase (car-safe event)
                ('touchscreen-update
                 (push (cons (float-time) posn) pending-events)
                 (if use-timer
                     (unless timer
                       (vkbd-log "Track Drag: Schedule timer")
                       (setq timer
                             ;; (run-at-time (time-add nil (list 0 0 5000))
                             (run-with-idle-timer 0 nil on-timer)))
                   (funcall on-timer)))
                ('touchscreen-end
                 (when timer
                   (cancel-timer timer)
                   (funcall on-timer))
                 (setq result (vkbd-track-drag--on-up event on-up))))
            (setq result (vkbd-track-drag--on-unknown-event event on-up))))))
    result))

(defun vkbd-track-drag--on-move (move-event ;; mouse-movement event (not touch)
                                  on-move on-leave
                                  target
                                  allow-out-of-target-p
                                  down-posn)
  (vkbd-log "Track Drag: On move event")
  (if (or allow-out-of-target-p
          (vkbd-posn-same-object-p (cadr move-event) ;; posn of mouse-movement
                                    down-posn
                                    target))
      (progn
        (when on-move (funcall on-move move-event))
        nil)
    ;; out of target
    (when on-leave (funcall on-leave move-event))
    move-event))

(defun vkbd-track-drag--on-up (up-event on-up)
  (vkbd-log "Track Drag: On up event")
  (when on-up (funcall on-up up-event))
  up-event)

(defun vkbd-track-drag--on-unknown-event (event on-up)
  (vkbd-log "Track Drag: On unknown event %s" (car-safe event))
  (cond
   ;; Ignore some events
   ((memq
     (car-safe event)
     '(;; (For Ubuntu 22/Emacs 27.1, To allow dragging in child frames)
       switch-frame
       ;; Unrelated touch events
       touchscreen-begin touchscreen-update touchscreen-end))
    nil)
   ;; Ignore unrelated mouse events
   ((mouse-event-p event)
    nil)
   ;; Otherwise
   (t
    (when on-up (funcall on-up event))
    (push (cons t event) unread-command-events)
    event)))

(provide 'vkbd)
;;; vkbd.el ends here
