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
;;   - `vkbd-global-keyboard-user-data-storage'

;;   Frame Settings:
;;   - `vkbd-keyboard-frame-parameters'
;;   - `vkbd-recycle-frames'
;;   - `vkbd-keyboard-frame-keep-visible-margins'

;;   Default layout & style:
;;   - `vkbd-default-keyboard-layout'
;;   - `vkbd-default-keyboard-style' (Currently only text01 style)

;;   Text style settings:
;;   - Variables:
;;     - `vkbd-text-key-width'
;;     - `vkbd-text-key-raise'
;;     - `vkbd-text-column-separator-width'
;;     - `vkbd-text-column-separator-display'
;;     - `vkbd-text-row-separator-height'
;;     - `vkbd-keyboard-buffer-line-spacing'
;;   - Faces:
;;     - `vkbd-text-keyboard'
;;     - `vkbd-text-key-common'
;;     - `vkbd-text-key'
;;     - `vkbd-text-key-pressed'
;;     - `vkbd-text-key-locked'
;;     - `vkbd-text-key-invisible'
;;     - `vkbd-text-column-separator'
;;     - `vkbd-text-row-separator'
;;     - `vkbd-text-title-bar'
;;     - `vkbd-text-title-button'
;;     - `vkbd-text-title-button-separator'
;;     - `vkbd-text-title-caption'

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

;;;; Message

(defun vkbd-msg (text)
  text)

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

(defun vkbd-log-watch (regexp)
  "Enable log output for logs whose format-string matches REGEXP.

The format-string is the first argument passed to the `vkbd-log' macro.

If an empty string is specified, all log output is enabled.

To output logs, `vkbd-log-generate' must be set to non-nil before
loading the library."
  (interactive (list (read-regexp
                      "Regexp to match format-string of vkbd-log (empty for all)")))
  (message "Log output enabled")
  (setq vkbd-log-output
        (if (and (stringp regexp) (not (string-empty-p regexp)))
            regexp
          t)))

(defun vkbd-log-unwatch ()
  "Disable log output."
  (interactive)
  (setq vkbd-log-output nil)
  (message "Log output disabled"))


;;;; Global Keyboard

;; The global keyboard is a keyboard that is used for the entire Emacs session.

(defcustom vkbd-global-keyboard-options
  nil ;;'(:title "Global Keyboard")
  "Options for the global keyboard.
This is a property list passed to `vkbd-make-keyboard' when creating the
global keyboard."
  :group 'vkbd :type 'plist)

(defun vkbd-global-keyboard-options ()
  (let ((options vkbd-global-keyboard-options))
    (unless (plist-member options :user-data-storage)
      (setq options
            (nconc (list :user-data-storage
                         (vkbd-global-keyboard-user-data-storage))
                   options)))
    options))

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
          (vkbd-make-keyboard (vkbd-global-keyboard-options)))))

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


;;;; Keyboards

;;;;; Keyboard Objects

;; Basic keyboard functions:
;; - `vkbd-make-keyboard'
;; - `vkbd-delete-keyboard'
;; - `vkbd-keyboard-live-p'

(defmacro vkbd-keyboard-property (keyboard prop)
  "Access property PROP from KEYBOARD object."
  `(plist-get (cdr ,keyboard) ,prop))

(defconst vkbd-keyboard-user-data-item-names
  '(frame-position layout container-type window-side))

(defun vkbd-make-keyboard (&optional options)
  "Create a virtual keyboard (on-screen keyboard).

OPTIONS is a property list containing various settings that affect creation.
When nil, everything is created based on default settings.

Return a object that holds all information about the keyboard."
  ;; Create a frame and buffer
  (let* ((options (copy-tree options))
         (user-data (vkbd-load-keyboard-user-data
                     options
                     (mapcar (lambda (name)
                               (cons name nil))
                             vkbd-keyboard-user-data-item-names)))
         (layout
          (or
           (vkbd-resolve-keyboard-layout-spec (plist-get options :layout))
           (vkbd-resolve-keyboard-layout-spec (alist-get 'layout user-data))
           (vkbd-default-keyboard-layout options)))
         (style (vkbd-default-keyboard-style options))
         (keyboard
          (list 'vkbd
                :deleted nil
                :options options
                :layout layout
                :style style
                :frame nil
                :window nil
                :buffer nil
                :pressed-modifiers nil
                :locked-modifiers nil
                :user-data user-data
                :user-data-modified nil
                :key-repeat-state nil))
         (buffer (vkbd-make-keyboard-buffer keyboard)))
    (setf (vkbd-keyboard-property keyboard :buffer) buffer)

    (vkbd-set-keyboard-container-type
     keyboard
     (or (plist-get options :container-type)
         (alist-get 'container-type user-data)
         (plist-get options :default-container-type)
         'child-frame))

    ;; Ensure global settings
    (vkbd-global-setup)

    keyboard))
;; EXAMPLE: (vkbd-make-keyboard)

(defun vkbd-delete-keyboard (keyboard)
  "Delete KEYBOARD, eliminating it from use."
  (interactive (list (vkbd-guess-current-keyboard)))
  (vkbd-delete-keyboard--internal keyboard 'external))

(defun vkbd-delete-keyboard--internal (keyboard reason)
  "Delete KEYBOARD, eliminating it from use."
  (vkbd-log "Life Cycle: vkbd-delete-keyboard reason=%s kbd-buffer=%s frame=%s"
            reason
            (vkbd-keyboard-buffer keyboard)
            (vkbd-keyboard-frame keyboard))
  (unless (vkbd-keyboard-property keyboard :deleted)
    ;; Stop the timer if it exists
    (vkbd-stop-key-repeat keyboard t)
    ;; Mark as killed
    (setf (vkbd-keyboard-property keyboard :deleted) t)
    ;; Delete container
    (vkbd-delete-keyboard-container keyboard)
    ;; Delete buffer
    (when-let* ((buffer (vkbd-keyboard-buffer keyboard)))
      (setf (vkbd-keyboard-property keyboard :buffer) nil) ;; Prevent re-entry
      (when (and (not (eq reason 'kill-buffer-hook))
                 (buffer-live-p buffer))
        (kill-buffer buffer)))
    ;; Save user data
    (vkbd-save-keyboard-user-data keyboard)

    ;; Revert global changes if no other keyboard objects exist
    (vkbd-auto-global-teardown)))

(defun vkbd-delete-all-keyboards ()
  "Forcefully delete all keyboard objects.
This function is for debugging purposes."
  (dolist (buffer (buffer-list))
    (when (vkbd-keyboard-buffer-p buffer)
      (vkbd-delete-keyboard (vkbd-keyboard-buffer-keyboard buffer)))))

;;;;;; Global Setup

(defun vkbd-global-setup ()
  (vkbd-global-setup-for-input-decode-map)
  (vkbd-global-setup-for-read-event)
  (vkbd-global-setup-for-user-data-save))

(defun vkbd-global-teardown ()
  (vkbd-log "Global Teardown")
  (vkbd-global-teardown-for-input-decode-map)
  (vkbd-global-teardown-for-read-event)
  (vkbd-global-teardown-for-user-data-save))

(defun vkbd-auto-global-teardown ()
  (unless (seq-some #'vkbd-keyboard-buffer-keyboard (buffer-list))
    (vkbd-global-teardown)))

;;;;;; Accessors

(defun vkbd-keyboard-p (object)
  (and (consp object) (eq (car object) 'vkbd)))

(defun vkbd-keyboard-live-p (keyboard)
  "Return non-nil if KEYBOARD is a keyboard which has not been deleted."
  ;; (and (buffer-live-p (vkbd-keyboard-buffer keyboard))
  ;;      (vkbd-frame-live-p (vkbd-keyboard-frame keyboard)))
  (and (vkbd-keyboard-p keyboard)
       (not (vkbd-keyboard-property keyboard :deleted))))

(defun vkbd-keyboard-options (keyboard)
  "Return the options plist associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :options))

(defun vkbd-keyboard-buffer (keyboard)
  "Return the buffer associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :buffer))

(defun vkbd-keyboard-frame (keyboard)
  "Return the frame associated with KEYBOARD."
  (vkbd-keyboard-property keyboard :frame))

(defun vkbd-keyboard-target-frame (keyboard)
  (let ((keyboard-frame (vkbd-keyboard-frame keyboard)))
    (if (frame-live-p keyboard-frame)
        (or (frame-parent keyboard-frame)
            (let ((ff (frame-focus keyboard-frame)))
              (when (and ff (not (eq ff keyboard-frame)))
                ff))
            ;; TODO: Track switch-frame and use previous frame
            (seq-find (lambda (f)
                        (and (not (eq f keyboard-frame))
                             (frame-live-p f)
                             (null (frame-parent f))
                             (null (frame-parameter f 'vkbd-keyboard))))
                      (visible-frame-list))
            keyboard-frame)
      (let ((sf (selected-frame)))
        (or (frame-parent sf)
            sf)))))

(defun vkbd-keyboard-key-repeat-state (keyboard)
  (vkbd-keyboard-property keyboard :key-repeat-state))

(defun vkbd-set-keyboard-key-repeat-state (keyboard key-repeat-state)
  (setf (vkbd-keyboard-property keyboard :key-repeat-state) key-repeat-state))

;;;;;; User Data

(defun vkbd-load-keyboard-user-data (options user-data-alist)
  (vkbd-data-storage-load
   (plist-get options :user-data-storage)
   user-data-alist))

(defun vkbd-save-keyboard-user-data (keyboard)
  (when (vkbd-keyboard-property keyboard :user-data-modified)
    (vkbd-data-storage-save
     (plist-get (vkbd-keyboard-options keyboard) :user-data-storage)
     (vkbd-keyboard-property keyboard :user-data))
    (setf (vkbd-keyboard-property keyboard :user-data-modified) nil)))

(defun vkbd-set-keyboard-user-data (keyboard item-name item-value)
  (unless (equal
           (alist-get item-name (vkbd-keyboard-property keyboard :user-data))
           item-value)
    (setf (alist-get item-name (vkbd-keyboard-property keyboard :user-data))
          item-value
          (vkbd-keyboard-property keyboard :user-data-modified) t)))

(defun vkbd-set-keyboard-user-data-save (keyboard item-name item-value)
  (vkbd-set-keyboard-user-data keyboard item-name item-value)
  (vkbd-save-keyboard-user-data keyboard))

(defun vkbd-keyboard-user-data (keyboard item-name &optional type-cast)
  (let ((value
         (alist-get item-name (vkbd-keyboard-property keyboard :user-data))))
    (if (functionp type-cast)
        (funcall type-cast value)
      value)))

(defun vkbd-cast-cons-numbers (value)
  (and (consp value) (numberp (car value)) (numberp (cdr value)) value))

(defun vkbd-save-all-keyboard-user-data ()
  (dolist (buffer (buffer-list))
    (when-let* ((keyboard (vkbd-keyboard-buffer-keyboard buffer)))
      (vkbd-save-keyboard-user-data keyboard))))

(defun vkbd-global-setup-for-user-data-save ()
  (add-hook 'kill-emacs-hook #'vkbd-save-all-keyboard-user-data))

(defun vkbd-global-teardown-for-user-data-save ()
  (remove-hook 'kill-emacs-hook #'vkbd-save-all-keyboard-user-data))


;;;;;; Keyboard Container

(defun vkbd-keyboard-container-type (keyboard)
  "Return the current keyboard container type of KEYBOARD.
Return nil or one of the symbols in `vkbd-keyboard-container-type-list'."
  (vkbd-keyboard-user-data keyboard 'container-type))

(defun vkbd-recreate-keyboard-container (keyboard)
  (vkbd-set-keyboard-container-type keyboard
                                    (vkbd-keyboard-container-type keyboard)))

(defun vkbd-set-keyboard-container-type (keyboard container-type)
  "Set the keyboard container type of KEYBOARD to CONTAINER-TYPE.

CONTAINER-TYPE must be one of the symbols in
`vkbd-keyboard-container-type-list'.

If an invalid value is specified, no error occurs and it is treated as
if the value returned by `vkbd-default-keyboard-container-type' was
specified."
  (interactive
   (let ((keyboard (vkbd-guess-current-keyboard)))
     (list keyboard
           (vkbd-read-keyboard-container-type-from-menu keyboard))))

  ;; Normalize CONTAINER-TYPE
  (unless (vkbd-cast-keyboard-container-type container-type)
    ;; (error "Unknown container-type: %s" container-type)
    (setq container-type (vkbd-default-keyboard-container-type)))

  ;; Delete the current container
  ;; Note: If first time, the container type is nil.
  (vkbd-delete-keyboard-container keyboard)

  ;; 子フレーム→独立フレーム切り替え時に、子フレームが残っているとな
  ;; ぜか独立フレームをクリックしてもフォーカスが当たらなくなる。
  ;; なのでここで明示的に未使用フレームを削除する。
  (vkbd-delete-all-unused-frames)

  ;; Change the value (Before `vkbd-make-keyboard-container')
  (vkbd-set-keyboard-user-data-save keyboard 'container-type container-type)

  ;; Make a new container
  (vkbd-make-keyboard-container keyboard))

;;;;;; Key States

(defun vkbd-keyboard-pressed-modifiers (keyboard)
  "Return the list of currently pressed modifier keys in KEYBOARD."
  (seq-union (vkbd-keyboard-property keyboard :locked-modifiers)
             (vkbd-keyboard-property keyboard :pressed-modifiers)
             #'eq))

(defun vkbd-keyboard-modifier-pressed-p (keyboard modifier)
  "Return t if MODIFIER key is pressed."
  (not (null (memq modifier (vkbd-keyboard-pressed-modifiers keyboard)))))

(defun vkbd-keyboard-shift-pressed-p (keyboard)
  "Return t if the shift key is pressed."
  (vkbd-keyboard-modifier-pressed-p keyboard 'shift))

(defun vkbd-set-keyboard-pressed-modifiers (keyboard modifiers)
  "Set the currently pressed modifier keys in KEYBOARD to MODIFIERS."
  (unless (seq-set-equal-p (vkbd-keyboard-property keyboard :pressed-modifiers)
                           modifiers)
    (setf (vkbd-keyboard-property keyboard :pressed-modifiers) modifiers)
    ;; Update display
    (vkbd-update-keyboard-display keyboard)))

(defun vkbd-keyboard-locked-modifiers (keyboard)
  "Return the list of currently locked modifier keys in KEYBOARD."
  (vkbd-keyboard-property keyboard :locked-modifiers))

(defun vkbd-keyboard-modifier-locked-p (keyboard modifier)
  "Return t if MODIFIER key is locked."
  (not (null (memq modifier (vkbd-keyboard-locked-modifiers keyboard)))))

(defun vkbd-set-keyboard-locked-modifiers (keyboard modifiers)
  "Set the currently locked modifier keys in KEYBOARD to MODIFIERS."
  (unless (seq-set-equal-p (vkbd-keyboard-property keyboard :locked-modifiers)
                           modifiers)
    (setf (vkbd-keyboard-property keyboard :locked-modifiers) modifiers)
    ;; Update display
    (vkbd-update-keyboard-display keyboard)))

(defun vkbd-set-keyboard-modifier-state (keyboard modifier pressed locked)
  (if pressed
      (unless (memq modifier
                    (vkbd-keyboard-property keyboard :pressed-modifiers))
        (push modifier (vkbd-keyboard-property keyboard :pressed-modifiers)))
    (setf (vkbd-keyboard-property keyboard :pressed-modifiers)
          (delq modifier (vkbd-keyboard-property keyboard :pressed-modifiers))))
  (if locked
      (unless (memq modifier
                    (vkbd-keyboard-property keyboard :locked-modifiers))
        (push modifier (vkbd-keyboard-property keyboard :locked-modifiers)))
    (setf (vkbd-keyboard-property keyboard :locked-modifiers)
          (delq modifier (vkbd-keyboard-property keyboard :locked-modifiers))))
  ;; Update display
  (vkbd-update-keyboard-display keyboard))

(defun vkbd-clear-keyboard-modifier-state (keyboard)
  (when (or (vkbd-keyboard-property keyboard :pressed-modifiers)
            (vkbd-keyboard-property keyboard :locked-modifiers))
    (setf (vkbd-keyboard-property keyboard :pressed-modifiers) nil
          (vkbd-keyboard-property keyboard :locked-modifiers) nil)
    ;; Update display
    (vkbd-update-keyboard-display keyboard)))

(defun vkbd-cycle-modifier-state (keyboard modifier)
  ;; Cycle normal => pressed => lock => normal
  (if (vkbd-keyboard-modifier-pressed-p keyboard modifier)
      ;; Already pressed
      (if (vkbd-keyboard-modifier-locked-p keyboard modifier)
          ;; Unlock & Release
          (vkbd-set-keyboard-modifier-state keyboard modifier nil nil)
        ;; Lock
        (vkbd-set-keyboard-modifier-state keyboard modifier t t))
    ;; Press the MODIFIER
    (vkbd-set-keyboard-modifier-state keyboard modifier t nil)))

;;;;;; Keyboard Display

(defvar vkbd-prevent-keyboard-updates nil) ;; t or (update . nil)

(defun vkbd-update-keyboard-display (keyboard)
  "Update the appearance of KEYBOARD to match its state."
  (if vkbd-prevent-keyboard-updates
      (when (consp vkbd-prevent-keyboard-updates)
        (setcar vkbd-prevent-keyboard-updates t))
    (vkbd-update-keyboard-display-forced keyboard)))

(defun vkbd-update-keyboard-display-forced (keyboard)
  "Update the appearance of KEYBOARD to match its state."
  (vkbd-keyboard-style--update-keyboard keyboard))

(defmacro vkbd-delay-keyboard-display-update (keyboard &rest body)
  (declare (indent 1) (debug (body)))
  (let ((sym-keyboard (gensym 'keyboard)))
    `(let ((,sym-keyboard ,keyboard)
           (vkbd-prevent-keyboard-updates
            ;; nil => (nil . nil)
            ;; t => t
            ;; (? . nil) => t
            (if vkbd-prevent-keyboard-updates t (cons nil nil))))
       (prog1
           (progn
             ,@body)
         (when (car-safe vkbd-prevent-keyboard-updates)
           ;; Update only if (t . nil)
           (vkbd-update-keyboard-display-forced ,sym-keyboard))))))

;;;;;; Keyboard Events

(defun vkbd-event-to-keyboard (event)
  "Return the keyboard object where EVENT occurred, or nil if not in a
keyboard."
  ;; TODO: Use text property?
  (vkbd-keyboard-buffer-keyboard (vkbd-event-to-keyboard-buffer event)))

;;;;;; Current Keyboard Object

(defvar vkbd-current-keyboard nil
  "The keyboard object that `vkbd-guess-current-keyboard' should
preferentially return.
When you want to clarify the keyboard object to be operated on,
dynamically bind this variable.")

(defun vkbd-guess-current-keyboard ()
  "Return the keyboard object currently being operated on."
  (or vkbd-current-keyboard
      (vkbd-event-to-keyboard last-command-event)
      (vkbd-event-to-keyboard last-input-event)
      (vkbd-keyboard-frame-keyboard last-event-frame)
      (error "No virtual keyboard")))

;;;;;; Menu

(defvar vkbd-keyboard-menu-map
  (let ((km (make-sparse-keymap (vkbd-msg "Keyboard Menu"))))
    (define-key-after km [layout]
      '(menu-item (vkbd-msg "Layout") vkbd-set-keyboard-layout))
    (define-key-after km [container-type]
      '(menu-item (vkbd-msg "Frame Type") vkbd-set-keyboard-container-type))
    (define-key-after km [window-side]
      '(menu-item (vkbd-msg "Window Side") vkbd-set-keyboard-window-side
                  :visible nil))
    (define-key-after km [close]
      '(menu-item (vkbd-msg "Close") vkbd-delete-keyboard))
    km))

(defun vkbd-open-keyboard-menu (keyboard &optional event)
  (interactive
   (list (vkbd-guess-current-keyboard)))
  (let ((vkbd-current-keyboard keyboard))
    (popup-menu (vkbd-make-keyboard-menu keyboard) event)))

(defun vkbd-make-keyboard-menu (keyboard)
  (let ((menu (copy-tree vkbd-keyboard-menu-map)))
    ;; Set layout menu
    (when-let* ((menu-item (assq 'layout (cdr menu))))
      (setf (nthcdr 3 menu-item)
            (vkbd-make-keyboard-layout-menu keyboard)))
    ;; Set container-type menu
    (when-let* ((menu-item (assq 'container-type (cdr menu))))
      (setf (nthcdr 3 menu-item)
            (vkbd-make-keyboard-container-type-menu keyboard)))
    ;; Set window-side menu
    (when-let* ((menu-item (assq 'window-side (cdr menu))))
      (setf (nthcdr 3 menu-item)
            (vkbd-make-keyboard-window-side-menu keyboard)))
    menu))

;;;;;; Layout

(defun vkbd-keyboard-layout (keyboard)
  (vkbd-keyboard-property keyboard :layout))

(defun vkbd-current-keyboard-layout-name (keyboard)
  (let ((current-layout (vkbd-keyboard-layout keyboard)))
    (seq-find (lambda (layout)
                ;; TODO: Use `equal'? (current-layout may be copied)
                (eq (vkbd-resolve-keyboard-layout-spec layout) current-layout))
              (vkbd-layout-list))))

(defun vkbd-set-keyboard-layout (keyboard new-layout-spec)
  (interactive
   (let ((keyboard (vkbd-guess-current-keyboard)))
     (list keyboard
           (vkbd-read-keyboard-layout-from-menu keyboard))))

  (when-let* ((new-layout (vkbd-resolve-keyboard-layout-spec new-layout-spec)))
    (let ((vkbd-prevent-keyboard-updates t))
      (vkbd-clear-keyboard-modifier-state keyboard))

    (with-current-buffer (vkbd-keyboard-buffer keyboard)
      (vkbd-erase-keyboard-buffer-contents keyboard)

      (setf (plist-get (vkbd-keyboard-property keyboard :options) :layout)
            new-layout-spec)
      (setf (vkbd-keyboard-property keyboard :layout)
            new-layout)

      (vkbd-make-keyboard-buffer-contents keyboard))

    (vkbd-fit-keyboard-container-to-buffer-contents keyboard)

    ;; Save
    (when (symbolp new-layout-spec)
      (vkbd-set-keyboard-user-data-save keyboard 'layout new-layout-spec))))

(defun vkbd-read-keyboard-layout-from-menu (keyboard)
  (car (x-popup-menu
        t (car (vkbd-make-keyboard-layout-menu keyboard)))))

(defun vkbd-make-keyboard-layout-selector (keyboard new-layout)
  (lambda () (interactive) (vkbd-set-keyboard-layout keyboard new-layout)))

(defun vkbd-make-keyboard-layout-menu (keyboard)
  (let ((current-layout (vkbd-current-keyboard-layout-name keyboard))
        (menu (make-sparse-keymap (vkbd-msg "Select a layout"))))
    (dolist (layout (vkbd-layout-list))
      (when (symbolp layout)
        (define-key-after
          menu (vector layout)
          (list 'menu-item (symbol-name layout)
                (vkbd-make-keyboard-layout-selector keyboard layout)
                :button `(:radio . ,(eq layout current-layout))))))
    (list menu)))

;;;;;; Style

(defun vkbd-keyboard-style (keyboard)
  (vkbd-keyboard-property keyboard :style))


;;;;; Keyboard Containers

;; A keyboard container is a frame or window used to display the
;; keyboard buffer.

;; The following container types are available:
;; - `child-frame' : The keyboard is displayed as a child frame.
;; - `independent-frame' : The keyboard is displayed as an independent frame.
;; - `window' : The keyboard is displayed as a window in an existing frame.

(defconst vkbd-keyboard-container-type-list
  '(child-frame
    independent-frame
    window))

(defun vkbd-cast-keyboard-container-type (container-type)
  (and (memq container-type vkbd-keyboard-container-type-list)
       container-type))

(defun vkbd-default-keyboard-container-type ()
  (car vkbd-keyboard-container-type-list))

(defun vkbd-make-keyboard-container (keyboard)
  (vkbd-log "Container: Make %s" (vkbd-keyboard-container-type keyboard))
  (pcase (vkbd-keyboard-container-type keyboard)
    ('child-frame
     (vkbd-make-keyboard-container--child-frame keyboard))
    ('independent-frame
     (vkbd-make-keyboard-container--independent-frame keyboard))
    ('window
     (vkbd-make-keyboard-container--window keyboard))))

(defun vkbd-delete-keyboard-container (keyboard)
  (vkbd-log "Container: Delete %s" (vkbd-keyboard-container-type keyboard))
  (pcase (vkbd-keyboard-container-type keyboard) ;; If nil, do nothing
    ('child-frame
     (vkbd-delete-keyboard-container--child-frame keyboard))
    ('independent-frame
     (vkbd-delete-keyboard-container--independent-frame keyboard))
    ('window
     (vkbd-delete-keyboard-container--window keyboard))))

(defun vkbd-fit-keyboard-container-to-buffer-contents (keyboard)
  (pcase (vkbd-keyboard-container-type keyboard)
    ('child-frame
     (vkbd-fit-keyboard-container-to-buffer-contents--frame keyboard))
    ('independent-frame
     (vkbd-fit-keyboard-container-to-buffer-contents--frame keyboard))
    ('window
     (vkbd-fit-keyboard-container-to-buffer-contents--window keyboard))))

(defun vkbd-select-input-target (keyboard)
  (interactive (list (vkbd-guess-current-keyboard)))
  (pcase (vkbd-keyboard-container-type keyboard)
    ('child-frame
     (vkbd-select-input-target--frame keyboard))
    ('independent-frame
     (vkbd-select-input-target--frame keyboard))
    ('window
     (vkbd-select-input-target--window keyboard))))

;;;;;; Child Frame

(defun vkbd-make-keyboard-container--child-frame (keyboard)
  (let* ((selected-frame (selected-frame))
         (parent-frame (or (frame-parent selected-frame)
                           selected-frame)))
    ;; PARENT-FRAMEはKeyboardフレームであってはならない。
    (when (frame-parameter parent-frame 'vkbd-keyboard)
      (setq parent-frame
            (seq-find
             (lambda (f)
               (and (null (frame-parameter parent-frame 'vkbd-keyboard))
                    (null (frame-parent f))))
             (visible-frame-list))))
    (vkbd-make-keyboard-container--frame keyboard parent-frame)))

(defun vkbd-delete-keyboard-container--child-frame (keyboard)
  (vkbd-delete-keyboard-container--frame keyboard))

;;;;;; Independent Frame

(defun vkbd-make-keyboard-container--independent-frame (keyboard)
  (vkbd-make-keyboard-container--frame keyboard nil))

(defun vkbd-delete-keyboard-container--independent-frame (keyboard)
  (vkbd-delete-keyboard-container--frame keyboard))

;;;;;; Frame Common

(defun vkbd-make-keyboard-container--frame (keyboard parent-frame)
  (let ((frame (vkbd-make-keyboard-frame keyboard parent-frame)))
    ;; Connect the KEYBOARD buffer and frame.
    (vkbd-set-keyboard-buffer-to-window (frame-root-window frame)
                                        (vkbd-keyboard-buffer keyboard))
    ;; Connect KEYBOARD and frame.
    (set-frame-parameter frame 'vkbd-keyboard keyboard)
    (setf (vkbd-keyboard-property keyboard :frame) frame)

    ;; Modify the frame size and position.
    (vkbd-fit-keyboard-frame-size-to-buffer-contents
     (vkbd-keyboard-options keyboard) frame)
    (vkbd-initialize-keyboard-frame-position keyboard)

    (when parent-frame
      (redirect-frame-focus frame parent-frame))

    (vkbd-start-window-deletion-detection)
    frame))

(defun vkbd-delete-keyboard-container--frame (keyboard)
  (when-let* ((frame (vkbd-keyboard-frame keyboard)))
    ;; Deselect FRAME and remove input focus from it.
    ;; (削除した後の選択フレームがFRAMEであってはならない。
    ;; 直後のmake-keyboard-container時にFRAMEの子フレームを作ってしま
    ;; う可能性があるから)
    (vkbd-select-target-frame keyboard)

    (setf (vkbd-keyboard-property keyboard :frame) nil) ;; Prevent re-entry

    (when (vkbd-frame-live-p frame)
      ;; Disconnect the KEYBOARD buffer and frame.
      (set-window-dedicated-p (frame-root-window frame) nil)

      ;; Disconnect KEYBOARD and frame.
      (set-frame-parameter frame 'vkbd-keyboard nil)

      ;; Delete frame.
      ;; Note: The buffer associated with the dedicated window is
      ;; deleted at this point.
      ;; Note: `vkbd-on-window-change' may be called.
      (vkbd-delete-frame frame))))

(defun vkbd-fit-keyboard-container-to-buffer-contents--frame (keyboard)
  (vkbd-fit-keyboard-frame-size-to-buffer-contents
   (vkbd-keyboard-options keyboard)
   (vkbd-keyboard-frame keyboard)))

(defun vkbd-select-input-target--frame (keyboard)
  (let ((target-frame (vkbd-select-target-frame keyboard)))
    ;; Windowsでは上(select-frame-set-input-focus)だけでカレントバッファ
    ;; も入力対象バッファに移るが、Androidではselected-frameだけ切り替わっ
    ;; てカレントバッファは変わらない。
    ;;
    ;; `vkbd-select-input-target--window'でも説明しているように
    ;; カレントバッファも切り替えないとコマンドの解決がうまくいかない。
    ;;
    ;; 仕方ないのでカレントバッファも強制的に切り替える。
    (when target-frame
      (let ((window (frame-selected-window target-frame)))
        (when (window-live-p window)
          (let ((buffer (window-buffer window)))
            (when (buffer-live-p buffer)
              (set-buffer buffer))))))))


;;;;;; Window

(defconst vkbd-keyboard-container--window-parameters
  '((no-delete-other-windows . t)
    (no-other-window . t)))

(defun vkbd-make-keyboard-container--window (keyboard)
  (let ((window (display-buffer-in-side-window
                 (vkbd-keyboard-buffer keyboard)
                 (vkbd-keyboard-container--window-display-alist keyboard))))
    (when window
      (setf (vkbd-keyboard-property keyboard :window) window)

      (dolist (param-value vkbd-keyboard-container--window-parameters)
        (set-window-parameter window (car param-value) (cdr param-value)))

      (set-window-dedicated-p window t)

      (fit-window-to-buffer window)

      (let ((side (vkbd-keyboard-window-side keyboard)))
        (window-preserve-size window (memq side '(left right)) t))

      (vkbd-start-window-deletion-detection))))

(defun vkbd-delete-keyboard-container--window (keyboard)
  (when-let* ((window (vkbd-keyboard-property keyboard :window)))
    (setf (vkbd-keyboard-property keyboard :window) nil) ;; Prevent re-entry
    (when (window-live-p window)
      (set-window-dedicated-p window nil)
      ;; Note: `vkbd-on-window-change' may be called.
      (delete-window window))))

(defun vkbd-fit-keyboard-container-to-buffer-contents--window (keyboard)
  (let ((window (vkbd-keyboard-property keyboard :window)))
    (when (window-live-p window)
      (fit-window-to-buffer window))))

(defun vkbd-select-input-target--window (keyboard)
  (or
   ;; input-decode-mapの後のキーマップ処理が、確実に入力対象のバッファ
   ;; 上で行われなければならない。
   ;; input-decode-mapに設定したdown-mouse-1が処理されるとき:
   ;; - current-bufferはキーボードバッファ
   ;; - selected-windowは元の選択ウィンドウのまま
   ;; となっている。
   ;;
   ;; 変換結果を返すときにカレントバッファを変更しないと(キーボードバッ
   ;; ファのままだと)、その変換結果によって実行されるコマンドはキーボー
   ;; ドバッファのローカルマップによって決定されてしまう。
   ;;
   ;; そしてその後、カレントバッファが元のバッファに戻ってキーボードバッ
   ;; ファ内で決定されたコマンドが実行されるという奇妙なことが起きる。
   ;;
   ;; 例: shell-modeを起動。container-type=windowで仮想キーボードを表示
   ;; し、shell-modeをカレントバッファにした状態で仮想キーボードのRETを
   ;; 押す。するとcomint-send-inputではなくnewlineコマンドがshell-mode
   ;; バッファで実行される。
   (let ((window (selected-window)))
     (when (and (window-live-p window)
                (not (eq window (vkbd-keyboard-property keyboard :window))))
       (let ((buffer (window-buffer window)))
         (when (and (buffer-live-p buffer)
                    (not (eq buffer (vkbd-keyboard-buffer keyboard))))
           (set-buffer buffer)
           t))))
   ;; 現在のバッファが仮想キーボードなら、
   ;; 直前に使用していたバッファのウィンドウを選択する。
   (when (vkbd-keyboard-buffer-p (current-buffer))
     (when-let* ((prev-window (get-mru-window nil nil t t)))
       (select-window prev-window)))))


;; Side Window Side

(defun vkbd-keyboard-container--window-display-alist (keyboard)
  (list (cons 'side (vkbd-keyboard-window-side keyboard))))

(defconst vkbd-side-window-side-list '(top left right bottom))

(defun vkbd-cast-side-window-side (value)
  (when (and value (memq value vkbd-side-window-side-list)) value))

(defun vkbd-make-keyboard-window-side-selector (keyboard side)
  (lambda () (interactive) (vkbd-set-keyboard-window-side keyboard side)))

(defun vkbd-make-keyboard-window-side-menu (keyboard)
  (list
   (let ((km (make-sparse-keymap (vkbd-msg "Select a side"))))
     (dolist (side vkbd-side-window-side-list)
       (define-key-after
         km (vector (intern (format "window-side-%s" side)))
         (list 'menu-item
               (symbol-name side) ;; TODO: (vkbd-msg (alist-get side dic))
               (vkbd-make-keyboard-window-side-selector keyboard side)
               :button
               `(:radio . ,(eq (vkbd-keyboard-window-side keyboard) side)))))
     km)
   :visible (and (vkbd-keyboard-window-side-user-selectable-p keyboard)
                 (eq (vkbd-keyboard-container-type keyboard) 'window))))

(defun vkbd-read-keyboard-window-side-from-menu (keyboard)
  (car (x-popup-menu
        t (car (vkbd-make-keyboard-window-side-menu keyboard)))))

(defun vkbd-keyboard-window-side (keyboard)
  (let ((options (vkbd-keyboard-options keyboard)))
    (or (vkbd-cast-side-window-side
         (plist-get options :window-side))
        (vkbd-cast-side-window-side
         (vkbd-keyboard-user-data keyboard 'window-side))
        (vkbd-cast-side-window-side
         (plist-get options :default-window-side))
        'bottom)))

(defun vkbd-set-keyboard-window-side (keyboard side)
  (interactive
   (let ((keyboard (vkbd-guess-current-keyboard)))
     (list keyboard  (vkbd-read-keyboard-window-side-from-menu keyboard))))
  (setq side (vkbd-cast-side-window-side side))
  (when (and side
             (vkbd-keyboard-window-side-user-selectable-p keyboard))
    (vkbd-set-keyboard-user-data-save keyboard 'window-side side)
    (vkbd-recreate-keyboard-container keyboard)))

(defun vkbd-keyboard-window-side-user-selectable-p (keyboard)
  (let ((options (vkbd-keyboard-options keyboard)))
    (null (vkbd-cast-side-window-side (plist-get options :window-side)))))


;;;;;; Window Deletion Detection

(defun vkbd-start-window-deletion-detection ()
  "Start window deletion detection.
Install global hooks for monitoring."
  (vkbd-log "Window Deletion Detection: Start")
  (add-hook 'window-configuration-change-hook #'vkbd-on-window-change)
  (add-hook 'after-delete-frame-functions #'vkbd-on-window-change))

(defun vkbd-stop-window-deletion-detection ()
  "Stop window deletion detection.
Uninstall global hooks."
  (vkbd-log "Window Deletion Detection: Stop")
  (remove-hook 'window-configuration-change-hook #'vkbd-on-window-change)
  (remove-hook 'after-delete-frame-functions #'vkbd-on-window-change))

(defun vkbd-on-window-change (&rest _args)
  "Function called when window configuration changes.

Called by the following global hooks (with the arguments shown in parentheses):

  - `window-configuration-change-hook' ()
  - `after-delete-frame-functions' (frame)"
  (vkbd-log "Window Deletion Detection: on-window-change current-frame=%s"
            (selected-frame))
  (when (zerop (vkbd-delete-invalid-window-keyboards))
    ;; No need to detect window deletion anymore
    (vkbd-stop-window-deletion-detection)))

(defun vkbd-delete-invalid-window-keyboards ()
  "Delete all keyboard objects whose window has been deleted.

This function checks all keyboard buffers and deletes keyboard objects
that no longer have a valid window displaying them."
  (let ((num-valid-keyboards 0))
    (dolist (buffer (buffer-list))
      (when-let* ((keyboard (vkbd-keyboard-buffer-keyboard buffer)))
        (if (or
             ;; そもそもウィンドウが関連付けられていない
             ;; (おそらくcontainer-type切り替え中)
             (and (null (vkbd-keyboard-frame keyboard))
                  (null (vkbd-keyboard-property keyboard :window)))
             ;; どこかのウィンドウに表示されている。
             (get-buffer-window buffer t)
             ;; container-type=windowの場合、ウィンドウが生きていれば
             ;; 一応OKとする。
             ;; which-key-modeのサイドバーが被ると一時的に消えてしまう。
             ;; その時に削除されると困るので。
             (window-live-p (vkbd-keyboard-property keyboard :window)))
            (cl-incf num-valid-keyboards)
          (vkbd-delete-keyboard--internal keyboard 'invalid-window))))
    num-valid-keyboards))


;;;;;; Keyboard Container Type Menu

(defun vkbd-read-keyboard-container-type-from-menu (keyboard)
  (car (x-popup-menu
        t (car (vkbd-make-keyboard-container-type-menu keyboard)))))

(defun vkbd-make-keyboard-container-type-selector (keyboard type)
  (lambda () (interactive) (vkbd-set-keyboard-container-type keyboard type)))

(defun vkbd-make-keyboard-container-type-menu (keyboard)
  (let ((current-type (vkbd-keyboard-container-type keyboard))
        (menu (make-sparse-keymap (vkbd-msg "Select a type"))))
    (dolist (type vkbd-keyboard-container-type-list)
      (when (symbolp type)
        (define-key-after
          menu (vector type)
          (list 'menu-item (symbol-name type)
                (vkbd-make-keyboard-container-type-selector keyboard type)
                :button `(:radio . ,(eq type current-type))))))
    (list menu)))


;;;;; Keyboard Frames

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

(defun vkbd-make-keyboard-frame (keyboard &optional parent-frame position size)
  "Create a frame to display a keyboard.

KEYBOARD is a keyboard object.
POSITION is a cons cell (X . Y) specifying the frame position.
SIZE is a cons cell (WIDTH . HEIGHT) specifying the frame size in pixels."
  (let* ((before-make-frame-hook nil)
         (after-make-frame-functions nil)
         (frame
          (vkbd-make-frame
           (append
            `((parent-frame . ,parent-frame))
            (when size
              `((width . (text-pixels . ,(car size)))
                (height . (text-pixels . ,(cdr size)))))
            (when position
              `((left . ,(car position))
                (top . ,(cdr position))))
            (or (plist-get (vkbd-keyboard-options keyboard) :frame-parameters)
                (vkbd-keyboard-frame-parameters))))))
    frame))

(defun vkbd-keyboard-frame-keyboard (frame)
  "Return th keyboard object associated with FRAME."
  (frame-parameter frame 'vkbd-keyboard))

(defun vkbd-set-keyboard-buffer-to-window (window buffer)
  "Set BUFFER to WINDOW for displaying the keyboard."
  (let ((old-buffer (window-buffer window)))
    (unless (eq buffer old-buffer)
      ;; Kill previous buffer if window dedicated
      (vkbd-kill-dedicated-buffer window)
      ;; Set new buffer
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t))))

;; Position & Size Control

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
  ;; TODO: Use `fit-frame-to-buffer' ?
  ;; (fit-frame-to-buffer frame)
  )

(defun vkbd-initialize-keyboard-frame-position (keyboard)
  "Initialize the position of the KEYBOARD."
  (let ((frame (vkbd-keyboard-frame keyboard)))
    (vkbd-set-keyboard-frame-position
     frame
     (or (vkbd-cast-cons-numbers
          (plist-get (vkbd-keyboard-options keyboard) :frame-position))
         (vkbd-keyboard-user-data keyboard 'frame-position
                                  #'vkbd-cast-cons-numbers)
         (vkbd-frame-position-center-top frame)))))

(defun vkbd-frame-position-center-top (frame)
  (let* ((parent-edges
          (window-edges (frame-root-window (frame-parent frame)) nil nil t))
         (parent-top (nth 1 parent-edges))
         (parent-w (- (nth 2 parent-edges) (nth 0 parent-edges)))
         (frame-w (frame-outer-width frame)))
    (cons (max 0 (/ (- parent-w frame-w) 2))
          parent-top)))

(defun vkbd-keyboard-frame-position (frame)
  (frame-position frame))

(defun vkbd-set-keyboard-frame-position (frame xy)
  (setq xy (vkbd-limit-keyboard-frame-position frame xy))
  (modify-frame-parameters frame `((left . (+ ,(car xy)))
                                   (top . (+ ,(cdr xy)))))
  (when-let* ((keyboard (vkbd-keyboard-frame-keyboard frame)))
    (vkbd-set-keyboard-user-data ;; Not -save
     keyboard 'frame-position (cons (car xy) (cdr xy)))))

(defcustom vkbd-keyboard-frame-keep-visible-margins
  '(200 48 80 1000000)
  "Margins to keep visible when dragging the virtual keyboard frame.

These values define the minimum amount (in pixels) of the keyboard
frame that must remain visible within the parent display area during
dragging operations.  This prevents the frame from being moved
completely off-screen while still allowing partial positioning
outside the visible area.

The list contains four integers: (LEFT TOP RIGHT BOTTOM).

Each margin value is capped by the corresponding frame dimension.
For example, if BOTTOM is set to a value larger than the frame height,
it effectively equals the frame height, preventing the frame from
extending above the parent display area.  This ensures the title bar
remains accessible."
  :type '(list :tag "Margins (In pixels)"
               (integer :tag "Left")
               (integer :tag "Top")
               (integer :tag "Right")
               (integer :tag "Bottom"))
  :group 'vkbd)

(defun vkbd-limit-keyboard-frame-position (frame xy)
  "Limit position XY to keep FRAME partially visible within the parent area.

XY is a cons cell (X . Y) representing the desired frame position.
Returns a cons cell with the position adjusted to satisfy the
visibility constraints defined by `vkbd-keyboard-frame-keep-visible-margins'."
  (if-let* ((parent-frame (frame-parent frame)))
      (let* ((frame-w (frame-outer-width frame))
             (frame-h (frame-outer-height frame))
             (parent-inner-edges (frame-edges parent-frame 'inner))
             (parent-origin-x (nth 0 parent-inner-edges))
             (parent-origin-y (nth 1 parent-inner-edges))
             (parent-limit-edges (frame-edges parent-frame 'native))
             (parent-l (- (nth 0 parent-limit-edges) parent-origin-x))
             (parent-t (- (nth 1 parent-limit-edges) parent-origin-y))
             (parent-r (- (nth 2 parent-limit-edges) parent-origin-x))
             (parent-b (- (nth 3 parent-limit-edges) parent-origin-y))
             (margin-l (min (nth 0 vkbd-keyboard-frame-keep-visible-margins)
                            frame-w))
             (margin-t (min (nth 1 vkbd-keyboard-frame-keep-visible-margins)
                            frame-h))
             (margin-r (min (nth 2 vkbd-keyboard-frame-keep-visible-margins)
                            frame-w))
             (margin-b (min (nth 3 vkbd-keyboard-frame-keep-visible-margins)
                            frame-h))
             (limit-l (+ (- parent-l frame-w) margin-r))
             (limit-r (- parent-r margin-l))
             (limit-t (+ (- parent-t frame-h) margin-b))
             (limit-b (- parent-b margin-t)))
        (cons (max limit-l (min (car xy) limit-r))
              (max limit-t (min (cdr xy) limit-b))))
    ;; Not a child frame
    ;; TODO: Limit coordinates to within the screen area
    xy))

;; Focus Control

(defun vkbd-select-parent-frame ()
  (interactive)
  (when-let* ((parent-frame (frame-parent nil)))
    (select-frame-set-input-focus parent-frame t)))

(defun vkbd-select-target-frame (keyboard)
  (interactive)
  (when-let* ((target-frame (vkbd-keyboard-target-frame keyboard)))
    (vkbd-log "Select Parent Frame: (before)selected frame=%s buffer=%s"
              (selected-frame) (current-buffer))
    (select-frame-set-input-focus target-frame t)
    (vkbd-log "Select Parent Frame: (after)selected frame=%s buffer=%s"
              (selected-frame) (current-buffer))
    ;; Return target-frame
    target-frame))


;;;;; Keyboard Buffers

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
  "<remap> <self-insert-command>" #'vkbd-select-input-target
  "<remap> <newline>" #'vkbd-select-input-target
  "<remap> <delete-char>" #'vkbd-select-input-target
  "<remap> <delete-forward-char>" #'vkbd-select-input-target
  "<remap> <delete-backward-char>" #'vkbd-select-input-target
  "<remap> <indent-for-tab-command>" #'vkbd-select-input-target
  "<remap> <scroll-down-command>" #'vkbd-select-input-target
  "<remap> <scroll-up-command>" #'vkbd-select-input-target
  "<remap> <kill-word>" #'vkbd-select-input-target
  "<remap> <backward-kill-word>" #'vkbd-select-input-target
  "<remap> <kill-line>" #'vkbd-select-input-target
  "<remap> <kill-whole-line>" #'vkbd-select-input-target
  "<remap> <kill-region>" #'vkbd-select-input-target
  "<remap> <yank>" #'vkbd-select-input-target
  "<remap> <split-window>" #'vkbd-select-input-target
  "<remap> <split-window-below>" #'vkbd-select-input-target
  "<remap> <split-window-right>" #'vkbd-select-input-target
  "<remap> <split-window-vertically>" #'vkbd-select-input-target
  "<remap> <split-window-horizontally>" #'vkbd-select-input-target
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

(defun vkbd-make-keyboard-buffer (keyboard)
  "Create a buffer that holds the keyboard display contents and state.

KEYBOARD is a keyboard object."
  (let ((options (vkbd-keyboard-options keyboard))
        (buffer (generate-new-buffer vkbd-keyboard-buffer-name)))
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
      (vkbd-make-keyboard-buffer-contents keyboard)

      ;; Add kill-buffer-hook
      (add-hook 'kill-buffer-hook
                #'vkbd-keyboard-buffer-on-before-buffer-kill nil t))
    buffer))

(defun vkbd-keyboard-buffer-on-before-buffer-kill ()
  (vkbd-log "Life Cycle: on-before-buffer-kill current-buffer=%s"
            (current-buffer))
  (when-let* ((keyboard (vkbd-keyboard-buffer-keyboard (current-buffer))))
    (setq-local vkbd-keyboard-buffer-keyboard nil) ;; Prevent re-entry
    (vkbd-delete-keyboard--internal keyboard 'kill-buffer-hook)))

(defun vkbd-make-keyboard-buffer-contents (keyboard)
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (vkbd-keyboard-style--insert-keyboard keyboard))
  (goto-char (point-min)))

(defun vkbd-erase-keyboard-buffer-contents (keyboard)
  ;; Stop the key repeat timer if it exists.
  ;; Because clearing the buffer contents will invalidate all key-objects.
  ;; TODO: Should I add delete-key-object and do this from there?
  (vkbd-stop-key-repeat keyboard t)

  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (vkbd-keyboard-style--erase-keyboard keyboard))
  (goto-char (point-min)))

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
              vkbd-keyboard-buffer-keyboard nil))

(defun vkbd-keyboard-buffer-p (object)
  "Return non-nil if OBJECT is a buffer in `vkbd-keyboard-buffer-mode'."
  (and (bufferp object)
       (eq (buffer-local-value 'major-mode object)
           'vkbd-keyboard-buffer-mode)))

(defun vkbd-keyboard-buffer-keyboard (buffer)
  "Return the keyboard object associated with keyboard BUFFER."
  (when buffer
    (buffer-local-value 'vkbd-keyboard-buffer-keyboard buffer)))

;;;;;; Frame dragging

(defconst vkbd-frame-move-debounce-time 0.07
  "Debounce time (in seconds) after moving the frame.
Touch event coordinates immediately after moving a frame are unreliable,
so events are ignored until this many seconds have elapsed since the
last move.")

(defun vkbd-move-keyboard-frame-on-mouse-down (down-event)
  (interactive "e")
  (let ((down-frame (window-frame (posn-window (event-start down-event))))
        (buffer (window-buffer (posn-window (event-start down-event))))
        (pos (posn-point (event-start down-event))))
    (when (and (frame-live-p down-frame)
               ;; If decorated, the use native title bar should be used.
               (frame-parameter down-frame 'undecorated)
               (buffer-live-p buffer)
               (integerp pos)
               (ignore-errors
                 (get-text-property pos 'vkbd-draggable buffer)))

      (let* ((down-xy (vkbd-posn-x-y-on-display (event-start down-event)))
             (down-frame-xy (vkbd-keyboard-frame-position down-frame))
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
                      (vkbd-set-keyboard-frame-position
                       down-frame
                       (cons (+ (car down-frame-xy) dx)
                             (+ (cdr down-frame-xy) dy)))
                      (setq last-frame-moved-time (float-time))))))))
        (vkbd-track-drag down-event on-move :on-up on-move
                         :allow-out-of-target-p t)

        (vkbd-select-parent-frame)
        moved))))

;;;;;; Buffer Events

(defun vkbd-event-in-keyboard-buffer-p (event)
  "Return non-nil if EVENT occurred in a `vkbd-keyboard-buffer-mode' buffer."
  (not (null (vkbd-event-to-keyboard-buffer event))))

(defun vkbd-event-to-keyboard-buffer (event)
  "Return the keyboard buffer where EVENT occurred, or nil if not in a
keyboard buffer."
  (when (vkbd-input-event-p event)
    ;; Note: If EVENT is a switch-frame event etc., posn-* will not
    ;; work, so make sure to limit it to input events.
    (let ((window (posn-window (event-start event))))
      (when (windowp window)
        (let ((buffer (window-buffer window)))
          (when (vkbd-keyboard-buffer-p buffer)
            buffer))))))


;;;;; Key Event Generation

;;;;;; Key Translation

(defconst vkbd-translation-event-types
  '(down-mouse-1 double-down-mouse-1 triple-down-mouse-1
                 mouse-1 double-mouse-1 triple-mouse-1
                 drag-mouse-1 double-drag-mouse-1 triple-drag-mouse-1
                 touchscreen-begin touchscreen-update touchscreen-end))

(defun vkbd-global-setup-for-input-decode-map ()
  ;; TODO: What to do if it's already in use.
  (dolist (event-type vkbd-translation-event-types)
    (define-key input-decode-map (vector event-type)
                #'vkbd-translate-keyboard-buffer-event)))

(defun vkbd-global-teardown-for-input-decode-map ()
  (dolist (event-type vkbd-translation-event-types)
    (let ((vec (vector event-type)))
      (when (eq (lookup-key input-decode-map vec)
                'vkbd-translate-keyboard-buffer-event)
        (define-key input-decode-map vec nil t)))))


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
  ;; Called when events in `vkbd-translation-event-types' occur.

  ;; TODO: キーボードバッファだけでなく任意のバッファにキーボードを置
  ;; けるようにしたい。それにはここでイベント発生ポイントのテキストプ
  ;; ロパティを見るなどの処理が必要になる。
  ;;
  ;; 関連する関数:
  ;; - `vkbd-text-keyboard-translate-event'
  ;; - `vkbd-text-keyboard-key-object-at-event'
  ;; - `vkbd-event-to-keyboard'
  ;; - `vkbd-event-to-keyboard-buffer'
  ;; - `vkbd-event-in-keyboard-buffer-p'
  (when-let* ((event (vkbd-current-key-remap-event))
              (buffer (vkbd-event-to-keyboard-buffer event))
              (keyboard (vkbd-keyboard-buffer-keyboard buffer)))
    (vkbd-log "Translate Event: Occurred in keyboard buffer %s" event)

    ;; Call the event translation function of the current keyboard style.
    ;; For example, `vkbd-text-keyboard-translate-event' and
    ;; `vkbd-translate-keyboard-event'.
    (let ((result (vkbd-keyboard-style--translate-event keyboard prompt event)))
      (vkbd-log "Translate Event: Return %s" result)
      result)))

(defun vkbd-translate-keyboard-event (keyboard _prompt event key-picker)
  ;; Called when events in `vkbd-translation-event-types' occur.
  (let ((result-events
         (if (vkbd-keyboard-key-repeat-state keyboard)
             ;; In key repeat state
             ;; キーリピートの待ち時間中(キーイベントを返してそれが消
             ;; 費されて再び戻ってくるまでの間)に何かイベントが発生し
             ;; たら、キーリピートを停止する。
             (vkbd-translate-keyboard-event--in-key-repeat-state keyboard event)
           ;; Not in key repeat state
           (vkbd-translate-keyboard-event-loop keyboard event key-picker))))
    (vkbd-log "Translate Event: Result=%s" result-events)
    (cond
     ((eq result-events 'no-conv)
      nil)
     ((null result-events)
      (vkbd-select-input-target keyboard)
      [])
     ((listp result-events)
      (vkbd-select-input-target keyboard)
      (setq last-input-event (car (last result-events))) ;;For context-menu-mode
      (apply #'vector result-events))
     (t
      nil))))

(defun vkbd-translate-keyboard-event--in-key-repeat-state (keyboard event)
  (vkbd-log
   "Translate Event: An event occurs during the repeat timer: %s"
   event)
  (pcase (car-safe event)
    ('touchscreen-update
     ;; Ignore
     )
    ((or 'mouse-1 'drag-mouse-1 'touchscreen-end)
     (vkbd-stop-key-repeat keyboard))
    (_ ;;Unknown event in keyboard buffer
     (vkbd-stop-key-repeat keyboard)))
  nil)

;; input-decode-mapによるイベント変換の流れ:
;; `vkbd-translate-keyboard-buffer-event' : input-decode-mapに登録される関数
;; => `vkbd-keyboard-style--translate-event' : キースタイルによるカスタマイズ
;; => `vkbd-text-keyboard-translate-event' : テキストスタイル用
;; => `vkbd-translate-keyboard-event' : 変換結果の調整
;; => `vkbd-translate-keyboard-event-loop'

;; 変換対象のマウス・タッチイベントが:
;;   キー以外の場所で発生 => 変換しない
;;   キー上で発生
;;     mouse down => upまで待ってから変換
;;     mouse up   => 速やかに変換
;;     その他(mouse-movement, touchscreen-update) => キー上である以上はブロック

;; 問題はmouse down, mouse upの変換後。
;; 変換後のイベントがまだ無く、修飾キーが押しっぱなしなら処理を継続する。
;; read-eventで後続のイベントを読み込んで処理をする。
;;   キー上
;;     mouse down => upまで待ち変換 => 繰り返し
;;     mouse up => 変換 => 繰り返し
;;     その他mouse, touch系 => key上である以上はブロック
;;     その他 => そのイベントを返す(変換結果とする)
;;   キー以外
;;     switch-frame => 無視?
;;     その他入力イベント => 「修飾キーを適用して」返す(変換結果とする)

(defun vkbd-translate-keyboard-event-loop (keyboard event key-picker)
  (let ((result-events nil)
        (first t))
    (while (or event
               ;; 注意: input-decode-mapに設定した関数は複数のイベント
               ;; は返せないので、すでに返すものがあるならループしない。
               (and (null result-events)
                    (vkbd-keyboard-pressed-modifiers keyboard)))

      (unless event
        (setq event (vkbd-translate-keyboard-event--read-event keyboard)))

      (setq result-events
            (if-let* ((keyobj (funcall key-picker event)))
                ;; EVENT occurred on a key
                (let ((result-events (vkbd-translate-keyboard-event--on-keyobj
                                      event keyobj first (vkbd-key-repeat-delay))))
                  (when (and result-events
                             ;; Still being pressed
                             (vkbd-key-object-pressed keyobj))
                    (vkbd-start-key-repeat keyobj))
                  result-events)
              ;; EVENT did not occur on a key
              (vkbd-translate-keyboard-event--off-keyobj
               keyboard event first)))
      (setq event nil
            first nil))
    result-events))

(defvar vkbd-translate-keyboard-event-control-text-conversion-style
  (eq system-type 'android)
  "Non-nil means ensure text-conversion-style is nil in
`vkbd-translate-keyboard-event--read-event' function.")

(defun vkbd-translate-keyboard-event--read-event (keyboard)
  (vkbd-log "Translate Event: Waiting for event to apply modifiers %s
    current buffer=%s
    window=%s winbuf=%s text-conversion-style=%s
    frame=%s focus-state=%s"
            (vkbd-keyboard-pressed-modifiers keyboard)
            (current-buffer)
            (selected-window) (window-buffer (selected-window))
            (buffer-local-value 'text-conversion-style
                                (window-buffer (selected-window)))
            (selected-frame)
            (frame-focus-state (selected-frame)))
  (let ((event
         (or
          ;; Make sure `text-conversion-style' is nil in the target
          ;; frame's buffer. With t or action, the IM inserts text
          ;; directly into the buffer, causing read-event to only
          ;; receive text-conversion events.
          (when (and vkbd-translate-keyboard-event-control-text-conversion-style
                     (boundp 'text-conversion-style)
                     (boundp 'overriding-text-conversion-style)
                     (fboundp 'set-text-conversion-style))

            (when-let* ((target-frame (vkbd-keyboard-target-frame keyboard))
                        (target-window (frame-selected-window target-frame)))
              (with-current-buffer (window-buffer target-window)
                (when text-conversion-style
                  (defvar overriding-text-conversion-style)
                  (let ((overriding-text-conversion-style nil)
                        (old-style text-conversion-style))
                    (unwind-protect
                        (progn
                          (set-text-conversion-style nil)
                          (vkbd-read-event-silent))
                      (set-text-conversion-style old-style)))))))
          ;; `text-conversion-style' is not used.
          (vkbd-read-event-silent))))
    (vkbd-log "Translate Event: read-event=%s" event)
    event))

(defun vkbd-translate-keyboard-event--on-keyobj (event keyobj first seconds)
  (vkbd-log
   "Translate Event: Occurred on a key (#%s first=%s)"
   (vkbd-key-object-id keyobj) first)

  (cond
   ;; Down event
   ((memq (car-safe event) '(down-mouse-1
                             double-down-mouse-1 triple-down-mouse-1
                             touchscreen-begin))
    ;; Update key state
    (vkbd-set-key-object-pressed keyobj t)
    ;; Wait for up event
    (unless (eq (vkbd-wait-for-mouse-up-event
                 (when (vkbd-key-repeat-enabled
                        (vkbd-key-object-keyboard keyobj))
                   seconds))
                'timeout)
      ;; Update key state
      (vkbd-set-key-object-pressed keyobj nil))
    ;; Convert KEYOBJ and KEYBOARD's pressed modifiers to events
    (vkbd-generate-key-object-pressed-events keyobj))

   ;; Up event without down event
   ;;
   ;; 注意: 例えば C-x などと打った後にマウスボタンを押し下げると
   ;; down-mouse-1が来ないことがある。その場合でもmouse-1は来ること
   ;; が多い。詳しい原因は分からない。その場合はここにいきなり来る。
   ;; 何もしないと C-x などの後にキーが打てないので対応する。
   ((memq (car-safe event) '(mouse-1
                             double-mouse-1 triple-mouse-1
                             touchscreen-end))
    (vkbd-generate-key-object-pressed-events keyobj))

   ;; Movement event => Block
   ((or (mouse-event-p event)
        (eq (car-safe event) 'touchscreen-update))
    nil)

   ;; Unknown event on the key (mouse-2|3 ?) => No conv
   (t
    (vkbd-log "Translate Event: Unknown event occurred on keyobj: %s" event)
    (if first
        'no-conv
      (list event)))))

(defun vkbd-translate-keyboard-event--off-keyobj (keyboard event first)
  (vkbd-log "Translate Event: Occurred not on a key")
  (cond
   ;; First event => No conv
   (first
    'no-conv)
   ;; Character, mouse, touchscreen => Apply modifiers
   ((vkbd-input-event-p event)
    (prog1
        (list (vkbd-apply-modifiers
               event (vkbd-keyboard-pressed-modifiers keyboard)))
      ;; 注意: ロック状態は維持できない。
      (vkbd-clear-keyboard-modifier-state keyboard)))
   ;; switch-frame => Ignore
   ;; C-c C- の後に割り込んできたりするので。
   ((eq (car-safe event) 'switch-frame)
    nil)
   ;; Unknown event => No conv
   ;; 注意: firstではないのでno-convを返してはならない。
   ;; 返すとEVENTではなく最初のイベントが選ばれてしまう。
   (t
    ;; (if first
    ;;     'no-conv
    (list event))))

(defun vkbd-wait-for-mouse-up-event (&optional seconds)
  (vkbd-log "Translate Event: Waiting for mouse up")
  (let ((result nil)
        (timeout-time (when (numberp seconds) (+ (float-time) seconds))))
    (while
        (let* ((new-event (vkbd-read-event-silent
                           nil
                           (when timeout-time
                             (max 0.0 (- timeout-time (float-time))))))
               (new-event-type (car-safe new-event)))
          (vkbd-log "Translate Event: new-event=%s" new-event)
          (cond
           ;; Timeout
           ((null new-event)
            (setq result 'timeout)
            nil)
           ;; End of key press
           ((memq new-event-type '(mouse-1 drag-mouse-1 touchscreen-end))
            nil)
           ;; Movement
           ((memq new-event-type '(mouse-movement touchscreen-update))
            t)
           ;; switch-frame
           ((eq new-event-type 'switch-frame)
            t) ;; or nil?
           ;; Unknown event
           (t
            (vkbd-log
             "Translate Event: Unknown event occurred while waiting: %s"
             new-event)
            nil))))
    result))

;;;;;; Replacement for read-event, read-char, and read-char-exclusive

;; `input-decode-map' is not applied to `read-event', `read-char', or
;; `read-char-exclusive'. Therefore, we use advice to forcibly apply
;; the translation.

(defcustom vkbd-replace-read-functions t
  "Non-nil means replace read functions to apply virtual keyboard translations.

When non-nil, `read-event', `read-char', and `read-char-exclusive' are
advised to apply virtual keyboard event translations (converting
mouse/touch operations on the virtual keyboard to key events)."
  :type 'boolean
  :group 'vkbd)

(defun vkbd-global-setup-for-read-event ()
  "Install advice on read functions to apply virtual keyboard translations."
  (when vkbd-replace-read-functions
    (advice-add 'read-event :around #'vkbd-read-event-wrapper)
    (advice-add 'read-char :override #'vkbd-read-char)
    (advice-add 'read-char-exclusive :override #'vkbd-read-char-exclusive)))

(defun vkbd-global-teardown-for-read-event ()
  "Remove advice from read functions."
  (advice-remove 'read-event #'vkbd-read-event-wrapper)
  (advice-remove 'read-char #'vkbd-read-char)
  (advice-remove 'read-char-exclusive #'vkbd-read-char-exclusive))

(defun vkbd-read-char (&optional prompt inherit-input-method seconds)
  "A replacement for the `read-char' that supports virtual keyboards.

Mouse events for virtual keyboards are translated into the corresponding
keystroke events."
  (when-let* ((val (vkbd-read-filtered-event
                    t t t prompt inherit-input-method seconds)))
    ;; For example: (char-resolve-modifiers (logior ?a (lsh 1 25))) => ?A
    (char-resolve-modifiers val)))

(defun vkbd-read-char-exclusive (&optional prompt inherit-input-method seconds)
  "A replacement for the `read-char-exclusive' that supports virtual keyboards.

Mouse events for virtual keyboards are translated into the corresponding
keystroke events."
  (when-let* ((val (vkbd-read-filtered-event
                    t t nil prompt inherit-input-method seconds)))
    (char-resolve-modifiers val)))

(defvar vkbd-read-event-wrapper--entry nil)
(defvar vkbd-suppress-read-event-replacement nil)

(defun vkbd-not-replace-read-event-p ()
  (or vkbd-read-event-wrapper--entry
      vkbd-suppress-read-event-replacement))

(defun vkbd-read-event-from-internal (&optional
                                      prompt inherit-input-method seconds)
  (let ((vkbd-suppress-read-event-replacement t))
    (read-event prompt inherit-input-method seconds)))

(defun vkbd-read-event-wrapper (old-fun
                                &optional prompt inherit-input-method seconds
                                &rest unknown-args)
  (if (vkbd-not-replace-read-event-p)
      ;; Call original `read-event'
      (apply old-fun
             prompt inherit-input-method seconds unknown-args)
    (let ((vkbd-read-event-wrapper--entry t)
          event)
      (while
          (eq (setq event
                    (vkbd-read-event--translete
                     ;; Call original `read-event'
                     (apply old-fun
                            prompt inherit-input-method seconds unknown-args)))
              'vkbd-retry))
      event)))

(defun vkbd-read-event--translete (event)
  (if event
      (let* ((current-key-remap-sequence (vector event))
             (translated-sequence
              (vkbd-translate-keyboard-buffer-event "")))
        (if translated-sequence
            ;; Translated
            (if (and (sequencep translated-sequence)
                     (> (length translated-sequence) 0))
                ;; Use translated event
                ;; TODO: Use the entire translated-sequence?
                ;; or 0 or (1- (length ..))?
                (aref translated-sequence 0)
              ;; Cancel EVENT and retry
              'vkbd-retry)
          ;; No need to translate.
          ;; Return EVENT
          event))
    ;; nil means that no input arrived within SECONDS, or some
    ;; unknown condition.
    ;; Return nil
    nil))

(defun vkbd-read-filtered-event (no-switch-frame
                                 ascii-required error-nonascii
                                 prompt inherit-input-method seconds)
  "Emulate `read_filtered_event' function defined in lread.c.

`read_filtered_event' is a function called by the `read-event',
`read-char', and `read-char-exclusive' functions."
  (defvar text-conversion-style)
  (defvar overriding-text-conversion-style)
  (defvar disable-inhibit-text-conversion)
  (if (and ascii-required
           (fboundp 'set-text-conversion-style)
           (boundp 'text-conversion-style)
           (boundp 'overriding-text-conversion-style)
           (boundp 'disable-inhibit-text-conversion)
           (not disable-inhibit-text-conversion))
      (unwind-protect
          (let ((overriding-text-conversion-style nil))
            ;; disable_text_conversion()
            ;; Update text conversion style
            (set-text-conversion-style text-conversion-style)

            (vkbd-read-filtered-event--loop no-switch-frame ascii-required
                                            error-nonascii
                                            prompt inherit-input-method seconds))
        ;; resume_text_conversion
        ;; Update text conversion style
        (set-text-conversion-style text-conversion-style))
    (vkbd-read-filtered-event--loop no-switch-frame ascii-required
                                    error-nonascii
                                    prompt inherit-input-method seconds)))

(defun vkbd-read-filtered-event--loop (no-switch-frame
                                       ascii-required error-nonascii
                                       prompt inherit-input-method seconds)
  (let ((delayed-switch-frame nil)
        ;; Compute timeout.
        (end-time (when (numberp seconds) (+ (float-time) seconds)))
        val)

    (while
        (progn
          (setq val (read-event prompt inherit-input-method
                                (when (numberp seconds)
                                  (max 0 (- end-time (float-time))))))

          (cond
           ((and no-switch-frame (consp val) (eq (car val) 'switch-frame))
            (setq delayed-switch-frame val)
            ;; Retry
            t)
           ((and ascii-required (not (and (numberp seconds) (null val))))
            ;; Convert certain symbols to their ASCII equivalents.
            (when (symbolp val)
              (when-let* ((tem (get val 'event-symbol-element-mask))
                          (tem1 (get (car tem) 'ascii-character)))
                ;; Merge this symbol's modifier bits
                ;; with the ASCII equivalent of its basic code.
                (setq val (logior tem1 (car (cdr tem))))))

            ;; If we don't have a character now, deal with it
            ;; appropriately.
            (if (not (fixnump val))
                (progn
                  (when error-nonascii
                    (push val unread-command-events)
                    (error "Non-character input-event"))
                  ;; Retry
                  t)
              ;; Return val
              nil))
           (t
            ;; Return val
            nil))))
    (when delayed-switch-frame
      ;; Cannot access `unread_switch_frame' !
      ;; unread_switch_frame = delayed_switch_frame;
      (push delayed-switch-frame unread-command-events))
    val))


;;;;;; Key Repeat

(defcustom vkbd-key-repeat-delay 0.6
  "Initial delay in seconds before key repeat starts."
  :group 'vkbd
  :type 'float)

(defcustom vkbd-key-repeat-interval 0.04
  "Interval in seconds between repeated key events."
  :group 'vkbd
  :type 'float)

(defun vkbd-key-repeat-delay ()
  vkbd-key-repeat-delay)

(defcustom vkbd-key-repeat-enabled t
  "Non-nil enables key repeat.

Note: Even if this variable is non-nil, key repeat may not work due to
limitations.

Currently, key repeat is not available when Frame Type (container-type)
is not `window'."
  :group 'vkbd
  :type 'boolean)

(defun vkbd-key-repeat-enabled (keyboard)
  (and vkbd-key-repeat-enabled
       ;; Key repeat does not work properly when using frames.
       ;; To correctly process keys generated by the virtual keyboard,
       ;; the frame containing the target buffer must be selected and focused.
       ;; However, doing so prevents mouse-up events from occurring on the
       ;; unfocused virtual keyboard frame, causing key repeat to continue
       ;; indefinitely.
       (eq (vkbd-keyboard-container-type keyboard) 'window)))

(defmacro vkbd-key-repeat-prop (key-repeat-state prop)
  `(plist-get (cdr ,key-repeat-state) ,prop))

(defun vkbd-start-key-repeat (keyobj)
  (vkbd-log "Translate Event: Start key repeat")
  (let ((keyboard (vkbd-key-object-keyboard keyobj)))
    (if (vkbd-key-repeat-enabled keyboard)
        (let ((key-repeat-state
               (list 'vkbd-key-repeat-state
                     :keyobj keyobj
                     :timer nil
                     :last-generation-time (float-time))))
          (vkbd-set-keyboard-key-repeat-state keyboard key-repeat-state)
          (vkbd-schedule-key-repeat-timer key-repeat-state 1))
      ;; Disabled
      (vkbd-set-key-object-pressed keyobj nil))))

(defun vkbd-stop-key-repeat (keyboard &optional prevent-display-update)
  (when-let* ((key-repeat-state (vkbd-keyboard-key-repeat-state keyboard)))
    (vkbd-log "Translate Event: Stop key repeat")
    ;; Cancel timer
    (when-let* ((timer (vkbd-key-repeat-prop key-repeat-state :timer)))
      (cancel-timer timer))
    ;; Release the key pressed
    (let ((keyobj (vkbd-key-repeat-prop key-repeat-state :keyobj)))
      (if prevent-display-update
          (let ((vkbd-prevent-keyboard-updates t))
            (vkbd-set-key-object-pressed keyobj nil))
        (vkbd-set-key-object-pressed keyobj nil)))
    ;; Clear keyboard state
    (vkbd-set-keyboard-key-repeat-state keyboard nil)))

;; キーリピート作動中に、キー生成関数がキーを生成してから再度キー生成
;; 関数に戻ってくるまでの時間(秒)です。
;; 理想は0ですが、短いと生成したキーがまだ消費されないうちに戻ってきて
;; しまい繰り返し待つことになります。
;; 長いとキーリピートの最短間隔が長くなってしまいますし、また、待って
;; いる間に発生した想定外のイベントを見逃す可能性が高くなります。
(defconst vkbd-key-repeat--timer-delay 0.02)

;; キーリピート処理の流れ:
;;
;; input-decode-mapのmouse downが反応
;;   => up待ち(`vkbd-wait-for-mouse-up-event')
;;      => delay時間経過 => タイマーセット => キーイベントを変換結果として返す
;;      => upした => キーイベントを変換結果として返す
;; vkbd-key-repeat--timer-delayだけ経過しタイマーイベント発生
;; (`vkbd-on-key-repeat-timer')
;;   => 前に生成したイベントが消費されていないなら再びタイマーセットして繰り返し
;;   => 消費されているなら
;;      => up待ち(`vkbd-wait-for-mouse-up-event')
;;         => interval時間経過 => タイマーセット => キーイベントを生成
;;         => upした => キーイベントを生成

(defun vkbd-schedule-key-repeat-timer (key-repeat-state repeat-count)
  (vkbd-log "Translate Event: Schedule key repeat timer %s" repeat-count)

  (setf (vkbd-key-repeat-prop key-repeat-state :timer)
        (run-with-timer vkbd-key-repeat--timer-delay nil
                        #'vkbd-on-key-repeat-timer
                        key-repeat-state repeat-count)))

(defun vkbd-on-key-repeat-timer (key-repeat-state repeat-count)
  (vkbd-log "Translate Event: on-timer repeat-count=%s" repeat-count)
  (setf (vkbd-key-repeat-prop key-repeat-state :timer) nil)

  (if unread-command-events
      ;; 前回追加したイベントがまだ消費されていない可能性があるので、
      ;; リピート入力を先延ばしする。
      ;;
      ;; タイマーの時間はあまり短くしても意味が無い。
      ;; unread-command-eventsが消費される前に繰り返すことはできないので。
      (progn
        (vkbd-log
         "Translate Event: There are unread events, so postpone repeating")
        (vkbd-schedule-key-repeat-timer key-repeat-state repeat-count))
    ;; 消費済なので次のキー入力に進む。
    (let* ((keyobj (vkbd-key-repeat-prop key-repeat-state :keyobj))
           (last-time (vkbd-key-repeat-prop key-repeat-state
                                            :last-generation-time))
           (wait-time (max
                       ;; 0にするとMessageバッファでリピートすると
                       ;; 止まらなくなる。
                       0.01
                       (- (+ last-time vkbd-key-repeat-interval)
                          (float-time))))
           (result-events (vkbd-translate-keyboard-event--on-keyobj
                           '(down-mouse-1) keyobj nil wait-time)))
      (setf (vkbd-key-repeat-prop key-repeat-state :last-generation-time)
            (float-time))
      (if (and result-events (vkbd-key-object-pressed keyobj))
          ;; Schedule next
          (vkbd-schedule-key-repeat-timer key-repeat-state (1+ repeat-count))
        ;; End of key repeat
        (vkbd-stop-key-repeat (vkbd-key-object-keyboard keyobj)))

      (vkbd-log "Translate Event: Push %s to unread-command-events"
                result-events)
      (setq unread-command-events (append unread-command-events result-events))
      )))


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

(defmacro vkbd-character-modifier-bits (char)
  `(logand ,char ,(logior ?\M-\0 ?\C-\0 ?\S-\0 ?\H-\0 ?\s-\0 ?\A-\0)))

(defmacro vkbd-character-without-modifiers (char)
  `(logand ,char ,(lognot (logior ?\M-\0 ?\C-\0 ?\S-\0 ?\H-\0 ?\s-\0 ?\A-\0))))

(defun vkbd-apply-modifiers-to-symbol (symbol modifiers)
  (let ((name (symbol-name symbol)))
    (when (string-match "\\`\\(A-\\|C-\\|H-\\|M-\\|S-\\|s-\\)+\\(.+\\)\\'"
                        name)
      (setq name (match-string 2 name)))
    (intern
     (concat
      ;; Alphabetical order (See: (info "(elisp)Function Keys"))
      (when (memq 'alt modifiers) "A-")
      (when (memq 'control modifiers) "C-")
      (when (memq 'hyper modifiers) "H-")
      (when (memq 'meta modifiers) "M-")
      (when (memq 'shift modifiers) "S-")
      (when (memq 'super modifiers) "s-")
      name))))
;; TEST: (vkbd-apply-modifiers-to-symbol 'M-C-s-down-mouse-1 '(meta hyper)) => H-M-down-mouse-1

(defun vkbd-apply-modifiers-to-char (char modifiers)
  (logior char
          (if (memq 'alt modifiers) ?\A-\0 0) ;; 0x0400000
          (if (memq 'super modifiers) ?\s-\0 0) ;; 0x0800000
          (if (memq 'hyper modifiers) ?\H-\0 0) ;; 0x1000000
          (if (memq 'shift modifiers) ?\S-\0 0) ;; 0x2000000
          (if (memq 'control modifiers) ?\C-\0 0) ;; 0x4000000
          (if (memq 'meta modifiers) ?\M-\0 0))) ;; 0x8000000

(defun vkbd-apply-modifiers (event modifiers)
  "Apply all modifier keys in MODIFIERS to EVENT.
MODIFIERS is a list of modifier key symbols.
Return the resulting modified key."
  ;; References:
  ;; - `event-apply-modifier'
  ;; - `char-resolve-modifiers'
  (cond
   ((numberp event)
    ;; Apply MODIFIERS
    (setq event (vkbd-apply-modifiers-to-char event modifiers))

    ;; Shift
    (unless (zerop (logand event ?\S-\0))
      (let ((base-char (vkbd-character-without-modifiers event)))
        (cond
         ((<= ?a base-char ?z)
          (setq event (logand (+ event (- ?A ?a))
                              ;; Remove shift
                              (lognot ?\S-\0))))
         ((<= ?A base-char ?Z)
          (setq event (logand event
                              ;; Remove shift
                              (lognot ?\S-\0)))))))
    ;; Control
    (unless (zerop (logand event ?\C-\0))
      (let ((base-char (vkbd-character-without-modifiers event)))
        (cond
         ((<= ?a base-char ?z) ;; 61(a) .. 7A(z)
          (setq event (logior (- (+ base-char (- ?A ?a)) ?@)
                              (logand
                               (vkbd-character-modifier-bits event)
                               ;; Remove control
                               (lognot ?\C-\0)))))
         ((<= ?A base-char ?Z) ;; 41(A) .. 5A(Z)
          (setq event (logior (- base-char ?@)
                              (logand
                               (vkbd-character-modifier-bits event)
                               ;; Remove control
                               (lognot ?\C-\0))
                              ;; Add shift
                              ?\S-\0)))
         ((<= ?@ base-char ?_) ;; 40(@) 5B([) 5C(\) 5D(]) 5E(^) 5F(_)
          (setq event (logior (- base-char ?@)
                              (logand
                               (vkbd-character-modifier-bits event)
                               ;; Remove control
                               (lognot ?\C-\0))))))))
    event)
   ((symbolp event)
    (vkbd-apply-modifiers-to-symbol event modifiers))
   ((symbolp (car-safe event))
    (cons
     (vkbd-apply-modifiers-to-symbol (car event) modifiers)
     (cdr event)))
   (t
    event)))
;; TEST: (vkbd-apply-modifiers '(C-down-mouse-1 foo) '(shift)) => (S-down-mouse-1 foo)
;; TEST: (vkbd-apply-modifiers 'tab '(shift control)) => C-S-tab
;; TEST: (vkbd-apply-modifiers 'tab '(control shift)) => C-S-tab
;; TEST: (vkbd-apply-modifiers 'backspace '(meta shift control)) => C-M-S-backspace
;; TEST: (vkbd-apply-modifiers ?  '(control)) => 67108896
;; TEST: (vkbd-apply-modifiers ?? '(control)) => 67108927
;; TEST: (vkbd-apply-modifiers ?0 '(control)) => 67108912
;; TEST: (vkbd-apply-modifiers ?a '(control)) => 1
;; TEST: (vkbd-apply-modifiers ?A '(control)) => 33554433
;; TEST: (vkbd-apply-modifiers ?a '(control shift)) => 33554433
;; TEST: (vkbd-apply-modifiers ?a '(control shift meta)) => 167772161
;; TEST: (vkbd-apply-modifiers ?` '(control meta)) => 201326688
;; TEST: (vkbd-apply-modifiers ?@ '(control meta)) => 134217728
;; TEST: (vkbd-apply-modifiers ?\] '(control)) => 29
;; TEST: (vkbd-apply-modifiers ?~ '(control meta)) => 201326718
;; Compare (read-event)

;;;;; Key Types

;; A key-type is converted to an Emacs key sequence. It is either a
;; symbol representing a specific sequence or a character code point
;; integer.

;; <key-type> :
;;   nil          : no key
;;   <symbol>     : key symbol of `vkbd-key-type-alist'
;;   <integer>    : character code point

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
    (app :text "App" :seq (apps))
    (men :text "Menu" :seq (menu))
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
;; TEST: (vkbd-fold-key-sequence-modifiers '(?a ?b ?c shift meta return control up control) '(meta hyper)) => ((150995041 98 99 M-S-return C-up) control)
;; TEST: (vkbd-fold-key-sequence-modifiers '(control ?a ?b meta ?c meta control) nil) => ((1 98 134217827) control meta)

(defun vkbd-key-type-to-simple-modifier (key-type)
  (let ((key-seq (vkbd-key-type-to-key-sequence key-type)))
    (when (and (vkbd-modifier-p (car key-seq))
               (null (cdr key-seq))) ;; Simple modifier key not compound key
      (car key-seq))))
;; TEST: (vkbd-key-type-to-simple-modifier 'shf) => shift
;; TEST: (vkbd-key-type-to-simple-modifier 'ctl) => control
;; TEST: (vkbd-key-type-to-simple-modifier 'esc) => nil

(defun vkbd-key-type-to-display-string (key-type &optional _key-width)
  (cond
   ((null key-type) nil)
   ((integerp key-type)
    (char-to-string key-type))
   ((symbolp key-type)
    ;; TODO: Select a string that matches KEY-WIDTH ("Ctl" "Ctrl" "Control")
    (plist-get (alist-get key-type vkbd-key-type-alist) :text))))


;;;;; Key Specs

;; A key-spec specifies the properties of a single key on the keyboard.
;; It consists of 0 to 2 key-types and a property list for additional
;; properties.
;; Use nil to insert space equivalent to one key.
;; Specify a second key-type to define input when the shift key is pressed.

;; <key-spec> :
;;   <key-type>
;;   ([<base-key-type> [<shifted-key-type>]] [<key-spec-plist>])

;; <key-spec-plist> : ( <key-spec-property>... )
;; <key-spec-property> :
;;   :w <key-width-ratio>
;;   :width <key-width-ratio>

(defun vkbd-key-spec-key-types (key-spec)
  "Return the key-types contained in KEY-SPEC.
Return nil or a list in the form ([<no-mod-key-type> [<shifted-key-type>]])."
  (cond
   ((null key-spec)
    nil)
   ((consp key-spec)
    (cl-loop
     with rtypes = nil ;;reversed
     ;; Take until keyword.
     for x in key-spec until (keywordp x) do (push x rtypes)
     ;; Discard the trailing nil.
     ;; (nil) => nil
     ;; (<non-nil> nil) => (<non-nil>)
     ;; (nil <non-nil>) => keep
     finally return (nreverse (seq-drop-while #'null rtypes))))
   ((or (integerp key-spec) (symbolp key-spec))
    (list key-spec))))
;; TEST: (vkbd-key-spec-key-types ?a) => (97)
;; TEST: (vkbd-key-spec-key-types '(?a)) => (97)
;; TEST: (vkbd-key-spec-key-types '(?a ?b)) => (97 98)
;; TEST: (vkbd-key-spec-key-types '(:w 1.5)) => nil
;; TEST: (vkbd-key-spec-key-types '(nil :w 1.5)) => nil
;; TEST: (vkbd-key-spec-key-types '(nil ?a :w 1.5)) => (nil 97)
;; TEST: (vkbd-key-spec-key-types '(esc :w 1.5)) => (esc)
;; TEST: (vkbd-key-spec-key-types nil) => nil
;; TEST: (vkbd-key-spec-key-types '(nil)) => nil
;; TEST: (vkbd-key-spec-key-types '(?a nil)) => (97)
;; TEST: (vkbd-key-spec-key-types '(?a nil :w 1.5)) => (97)

(defun vkbd-key-spec-base-key-type (key-spec)
  "Return base (unmodified) key type of KEY-SPEC."
  (car (vkbd-key-spec-key-types key-spec)))

(defun vkbd-key-spec-properties (key-spec)
  "Return the property list contained in KEY-SPEC."
  (when (consp key-spec)
    (cl-loop for x on key-spec when (keywordp (car x)) return x)))
;; TEST: (vkbd-key-spec-properties nil) => nil
;; TEST: (vkbd-key-spec-properties ?a) => nil
;; TEST: (vkbd-key-spec-properties '(?a)) => nil
;; TEST: (vkbd-key-spec-properties '(:w 1.5)) => (:w 1.5)
;; TEST: (vkbd-key-spec-properties '(esc :w 1.5)) => (:w 1.5)

(defun vkbd-key-spec-modified-key-types-and-consumed-modifiers
    (key-spec pressed-modifiers)
  "Return KEY-SPEC\\='s key-types with PRESSED-MODIFIERS applied and list
of consumed modifiers.

This function returns a cons cell (KEY-TYPES . CONSUMED-MODIFIERS) where:
- KEY-TYPES is the list of key-types to display on the keytop
- CONSUMED-MODIFIERS is the list of modifiers used for key-type switching

When shift is not in PRESSED-MODIFIERS:
  Returns the full key-types list unchanged with no consumed modifiers.
  Both base and shifted types are displayed on the keytop.

When shift is in PRESSED-MODIFIERS:
  If a shifted-key-type exists:
    Returns only the shifted-key-type with shift marked as consumed.
  If no shifted-key-type exists but base is a lowercase letter (a-z):
    Returns the uppercase version with shift marked as consumed.
  Otherwise:
    Returns the base-key-type unchanged with no consumed modifiers.

The car of the returned KEY-TYPES list determines the actual key-type
to be input. Consumed modifiers are tracked to prevent applying the
same modifier twice in subsequent processing."
  (when-let* ((key-types (vkbd-key-spec-key-types key-spec)))
    (if (memq 'shift pressed-modifiers)
        ;; with shift
        (if-let* ((shifted-key-type (cadr key-types)))
            ;; 2nd key-type is valid
            (cons (list shifted-key-type) '(shift)) ;; Consume the shift key
          ;; single key-type
          (if-let* ((kt (car key-types)))
              ;; TODO: Downcase if A ~ Z ? (Related:`vkbd-apply-modifiers')
              (if (and (integerp kt) (<= ?a kt ?z))
                  (cons (list (upcase kt)) '(shift)) ;; Consume the shift key
                (cons (list kt) nil))
            nil))
      ;; without shift
      (cons key-types nil))))
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers nil nil) => nil
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers nil '(shift control)) => nil
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers ?a nil) => ((97))
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers ?a '(shift)) => ((65) shift)
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers ?a '(shift control)) => ((65) shift)
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers 'esc '(shift control)) => ((esc))
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers '(?a :w 2) '(shift control)) => ((65) shift)
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers '(?2 ?\") '(shift control)) => ((34) shift)
;; TEST: (vkbd-key-spec-modified-key-types-and-consumed-modifiers '(?2 ?\") '(control)) => ((50 34))

(defun vkbd-key-spec-modified-key-types-for-display (key-spec pressed-modifiers)
  (car
   (vkbd-key-spec-modified-key-types-and-consumed-modifiers
    key-spec pressed-modifiers)))

(defun vkbd-key-spec-width-ratio (key-spec)
  "Return the width ratio property of KEY-SPEC."
  (let ((key-props (vkbd-key-spec-properties key-spec)))
    (or (plist-get key-props :width)
        (plist-get key-props :w))))


;;;;; Key Objects

(defun vkbd-make-key-object (key-id key-spec keyboard)
  "Create a key object and return it."
  (list 'vkbd-key-object
        :id key-id :spec key-spec :keyboard keyboard :pressed nil))

(defmacro vkbd-key-object-plist (keyobj)
  "Return the property list of KEYOBJ."
  `(cdr ,keyobj))

(defmacro vkbd-key-object-property (keyobj prop)
  "Return the property PROP value of KEYOBJ."
  `(plist-get (vkbd-key-object-plist ,keyobj) ,prop))

(defun vkbd-key-object-id (keyobj)
  "Return identifier of KEYOBJ."
  (vkbd-key-object-property keyobj :id))

(defun vkbd-key-object-key-spec (keyobj)
  "Return key-spec structure of KEYOBJ."
  (vkbd-key-object-property keyobj :spec))

(defun vkbd-key-object-keyboard (keyobj)
  "Return the keyboard object that contains KEYOBJ."
  (vkbd-key-object-property keyobj :keyboard))

(defun vkbd-key-object-pressed (keyobj)
  "Return non-nil if KEYOBJ is pressed."
  (vkbd-key-object-property keyobj :pressed))

(defun vkbd-set-key-object-pressed (keyobj pressed)
  "Set the key PRESSED state to KEYOBJ."
  (setf (vkbd-key-object-property keyobj :pressed) pressed)
  ;; Update display
  (vkbd-keyboard-style--update-key keyobj))

(defun vkbd-key-object-to-simple-modifier (keyobj)
  (when-let* ((key-spec (vkbd-key-object-key-spec keyobj))
              (key-type (vkbd-key-spec-base-key-type key-spec)))
    (vkbd-key-type-to-simple-modifier key-type)))

(defun vkbd-key-object-pressed-modifier-p (keyobj)
  "Return non-nil if KEYOBJ is a pressed modifier key."
  (when-let* ((modifier (vkbd-key-object-to-simple-modifier keyobj)))
    (memq modifier
          (vkbd-keyboard-pressed-modifiers
           (vkbd-key-object-keyboard keyobj)))))

(defun vkbd-key-object-locked-modifier-p (keyobj)
  "Return non-nil if KEYOBJ is a locked modifier key."
  (when-let* ((modifier (vkbd-key-object-to-simple-modifier keyobj)))
    (memq modifier
          (vkbd-keyboard-locked-modifiers
           (vkbd-key-object-keyboard keyobj)))))


(defun vkbd-generate-key-object-pressed-events (keyobj)
  "Convert KEYOBJ to events, updating pressed modifiers in KEYBOARD.

Return a list of events corresponding to KEYOBJ."
  (if-let* ((modifier (vkbd-key-object-to-simple-modifier keyobj)))
      ;; A simple modifier key was pressed.
      ;; (e.g. C-, S-, M-. Excluding C-x, C-c, etc.)
      (progn
        ;; Cycle normal => pressed => lock => normal
        (vkbd-cycle-modifier-state (vkbd-key-object-keyboard keyobj) modifier)
        ;; No events
        nil)
    ;; Not a simple modifier key (character, special key, compound key, etc.)
    (vkbd-generate-key-object-pressed-events--normal keyobj)))

(defun vkbd-generate-key-object-pressed-events--normal (keyobj)
  (let ((keyboard (vkbd-key-object-keyboard keyobj)))
    (vkbd-delay-keyboard-display-update keyboard
      (let* ((key-type (vkbd-generate-key-object-key-type keyobj))
             ;; key-type to events
             (events-and-new-modifiers
              (vkbd-fold-key-sequence-modifiers
               (vkbd-key-type-to-key-sequence key-type)
               (vkbd-keyboard-pressed-modifiers keyboard))))
        (vkbd-set-keyboard-pressed-modifiers keyboard
                                             (cdr events-and-new-modifiers))
        (car events-and-new-modifiers)))))

(defun vkbd-generate-key-object-key-type (keyobj)
  (let* ((keyboard (vkbd-key-object-keyboard keyobj))
         (modifiers (vkbd-keyboard-pressed-modifiers keyboard))
         ;; Apply shift
         (key-types-and-consumed-modifiers
          (vkbd-key-spec-modified-key-types-and-consumed-modifiers
           (vkbd-key-object-key-spec keyobj)
           modifiers)))
    ;; Update modifier state
    (vkbd-set-keyboard-pressed-modifiers
     keyboard
     (seq-difference modifiers (cdr key-types-and-consumed-modifiers)))
    (car (car key-types-and-consumed-modifiers))))


;;;;; Text Keyboard Style

;;;;;; Text Keyboard Input

(defun vkbd-text-keyboard-key-object-at-event (event)
  "Return the key object at the position where EVENT occurred."
  ;; TODO: vkbd-key-objectテキストプロパティがあればキーボードバッファ
  ;; 以外でも動作するようにしたい。
  ;; 次の場所にも影響があるかも。
  ;; - `vkbd-translate-keyboard-buffer-event'
  ;; - `vkbd-event-to-keyboard'
  ;; - `vkbd-keyboard-style--translate-event'
  (when-let* ((buffer (vkbd-event-to-keyboard-buffer event))
              (keyboard (vkbd-keyboard-buffer-keyboard buffer)))
    (with-current-buffer buffer
      (let ((pos (posn-point (event-start event))))
        (when (integerp pos)
          (get-text-property pos 'vkbd-key-object))))))

(defun vkbd-text-keyboard-translate-event (keyboard prompt event)
  (vkbd-translate-keyboard-event
   keyboard prompt event
   ;; Identify the pressed key from the text property where the EVENT occurred.
   #'vkbd-text-keyboard-key-object-at-event))

;;;;;; Text Keyboard Appearance
;;;;;;; Insert Buffer Contents

(defun vkbd-insert-text-keyboard (keyboard)
  (vkbd-insert-text-keyboard-title keyboard)
  (vkbd-insert-text-keyboard-keys keyboard))

;;;;;;; Insert Title Bar

(defun vkbd-insert-text-keyboard-title (keyboard)
  (let ((options (vkbd-keyboard-options keyboard)))
    (vkbd-insert-text-close-button options)
    (unless (eq (plist-get options :menu-button) 'hide)
      (vkbd-insert-text-menu-button options))
    (let ((title (plist-get options :title)))
      (when (stringp title)
        (vkbd-insert-propertized
         title
         'face (vkbd-get-face-opt options 'vkbd-text-title-caption)
         'vkbd-draggable t)))
    (vkbd-insert-propertized
     "\n"
     'face (vkbd-get-face-opt options 'vkbd-text-title-bar)
     'vkbd-draggable t)
    (vkbd-insert-text-row-separator options)))

(defun vkbd-on-close-button-click (keyboard)
  (interactive (list (vkbd-guess-current-keyboard)))
  (vkbd-log "on-close-button-click")
  (when (vkbd-keyboard-p keyboard)
    (vkbd-delete-keyboard--internal keyboard 'close-button)))

(defun vkbd-on-menu-button-click (keyboard)
  (interactive (list (vkbd-guess-current-keyboard)))
  (vkbd-log "on-menu-button-click")
  (when (vkbd-keyboard-p keyboard)
    (unwind-protect
        (vkbd-open-keyboard-menu keyboard last-command-event)
      (vkbd-select-target-frame keyboard))))

(defconst vkbd-text-keyboard-close-button-caption "  x  ")
(defconst vkbd-text-keyboard-menu-button-caption "  =  ")

(defun vkbd-insert-text-close-button (options)
  (vkbd-insert-text-title-button options
                                 vkbd-text-keyboard-close-button-caption
                                 #'vkbd-on-close-button-click
                                 (vkbd-msg "Close")))

(defun vkbd-insert-text-menu-button (options)
  (vkbd-insert-text-title-button options
                                 vkbd-text-keyboard-menu-button-caption
                                 #'vkbd-on-menu-button-click
                                 (vkbd-msg "Menu")))

(defun vkbd-insert-text-title-button (options caption command help-echo)
  (vkbd-insert-propertized
   caption
   'face (vkbd-get-face-opt options 'vkbd-text-title-button)
   'pointer 'hand
   'help-echo help-echo
   'keymap
   (let ((km (make-sparse-keymap)))
     (define-key km [down-mouse-1] #'ignore)
     (define-key km [mouse-1] command)
     (define-key km [touchscreen-begin] #'ignore)
     (define-key km [touchscreen-update] #'ignore)
     (define-key km [touchscreen-end] command)
     (define-key km (kbd "RET") command)
     (define-key km (kbd "SPC") command)
     km))
  (vkbd-insert-text-title-button-separator options))

(defcustom vkbd-text-title-button-separator-width 0.25
  "Width of spacing between buttons (horizontal spacing between buttons)."
  :type 'float
  :group 'vkbd-text-style)

(defconst vkbd-text-title-button-separator-display 'space) ;; or "|"

(defun vkbd-insert-text-title-button-separator (options)
  (vkbd-insert-text-separator options
                              :text-title-button-separator-width
                              vkbd-text-title-button-separator-width
                              vkbd-text-title-button-separator-display
                              'vkbd-text-title-button-separator))

(defun vkbd-insert-text-separator (options
                                   width-key default-width display-spec
                                   face)
  (let ((width (or (plist-get options width-key) default-width)))
    (when (and (numberp width) (> width 0))
      (vkbd-insert-propertized
       " "
       'display
       (if (stringp display-spec)
           display-spec
         `(space :width ,width))
       'face (vkbd-get-face-opt options face)))))

;;;;;;; Insert Keys

(defun vkbd-insert-text-keyboard-keys (keyboard)
  (let ((options (vkbd-keyboard-options keyboard))
        (key-id 0))
    (vkbd-map-keyboard-layout-keys
     (vkbd-keyboard-layout keyboard)
     (lambda (key-spec)
       (vkbd-insert-text-key keyboard key-spec (cl-incf key-id)))
     (lambda () ;; between columns
       (vkbd-insert-text-column-separator options))
     (lambda () ;; between rows
       (insert "\n")
       (vkbd-insert-text-row-separator options)))))

(defun vkbd-insert-text-key (keyboard key-spec key-id)
  (let* ((keyobj (vkbd-make-key-object key-id key-spec keyboard))
         (text (vkbd-text-key-string keyobj)))
    (when (stringp text)
      (insert text))))

;;;;;;; Make Key String

(defun vkbd-text-key-string (keyobj)
  "Generate the display string for a key in text keyboard style."
  (or (vkbd-text-key-string-visible keyobj)
      (vkbd-text-key-string-invisible keyobj)))

(defun vkbd-text-key-string-invisible (keyobj)
  (let ((options (vkbd-keyboard-options (vkbd-key-object-keyboard keyobj))))
    (vkbd-text-key-propertized
     (vkbd-text-key-centering
      ""
      (vkbd-text-key-width options (vkbd-key-spec-width-ratio
                                    (vkbd-key-object-key-spec keyobj))))
     'face (vkbd-get-face-opt options 'vkbd-text-key-invisible)
     'pointer 'arrow)))

(defun vkbd-text-key-string-visible (keyobj)
  (when-let* ((text (vkbd-text-key-spec-string-for-display
                     (vkbd-key-object-key-spec keyobj)
                     (vkbd-key-object-keyboard keyobj))))
    (vkbd-text-key-propertized
     text
     'face (vkbd-text-key-obj-face keyobj)
     'vkbd-key-object keyobj
     'vkbd-key-id (vkbd-key-object-id keyobj)
     'pointer 'hand)))

(defun vkbd-text-key-spec-string-for-display (key-spec keyboard)
  (when-let* ((key-types (vkbd-key-spec-modified-key-types-for-display
                          key-spec
                          (vkbd-keyboard-pressed-modifiers keyboard))))
    (let* ((options (vkbd-keyboard-options keyboard))
           (key-width (vkbd-text-key-width
                       options (vkbd-key-spec-width-ratio key-spec))))
      ;; key-types => type-strs => concat => centering
      (vkbd-text-key-centering
       (vkbd-text-key-concat-key-type-strings
        (vkbd-text-key-stringize-key-types
         key-types
         key-width options)
        key-width)
       key-width))))

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
  :group 'vkbd-text-style)

(defun vkbd-text-key-stringize-key-types (key-types key-width options)
  (let* ((num-types (length key-types))
         (width-per-type (max 1 (/ key-width num-types)))
         (raise-spec (or (plist-get options :text-key-raise)
                         vkbd-text-key-raise)))
    (cl-loop
     for type in key-types
     for type-index from 0
     for raise-factor = (nth type-index (nth (1- num-types) raise-spec))
     for str = (or (vkbd-key-type-to-display-string type width-per-type)
                   " ")
     collect (if raise-factor
                 (propertize str 'display (list 'raise raise-factor))
               str))))

(defun vkbd-text-key-concat-key-type-strings (type-strs key-width)
  (when type-strs
    (let ((type-strs-total-width (apply #'+ (mapcar #'string-width type-strs))))
      (mapconcat
       #'identity type-strs
       ;; Add separators only if there is enough KEY-WIDTH
       ;; [ab] [ab ] [ ab ] [ a b ] [ a b  ] [  a b  ]
       (if (>= (- (- key-width 2) type-strs-total-width)
               (1- (length type-strs)))
           " "
         nil)))))
;; TEST: (vkbd-text-key-concat-key-type-strings '("," "<") 5) => ", <"

(defun vkbd-text-key-centering (text key-width)
  ;; TODO: Use display property (space :width 0.x)
  (let ((text-width (string-width text)))
    (when (< text-width key-width)
      (let* ((short-width (- key-width text-width))
             (left-width (/ short-width 2))
             (right-width (- short-width left-width)))
        (setq text (concat (make-string left-width ?\s)
                           text
                           (make-string right-width ?\s)))))
    (truncate-string-to-width text key-width)))

(defun vkbd-text-key-obj-face (keyobj)
  (let ((options (vkbd-keyboard-options (vkbd-key-object-keyboard keyobj))))
    (cond
     ((vkbd-key-object-locked-modifier-p keyobj)
      (vkbd-get-face-opt options 'vkbd-text-key-locked))
     ((or (vkbd-key-object-pressed keyobj)
          (vkbd-key-object-pressed-modifier-p keyobj))
      (vkbd-get-face-opt options 'vkbd-text-key-pressed))
     (t
      (vkbd-get-face-opt options 'vkbd-text-key)))))

;;;;;;; Inert Column Separator

(defcustom vkbd-text-column-separator-width 0.25
  "Width of spacing between columns (horizontal spacing between keys)."
  :type 'float
  :group 'vkbd-text-style)

(defconst vkbd-text-column-separator-display 'space) ;; or "|"

(defun vkbd-insert-text-column-separator (options)
  (vkbd-insert-text-separator options
                              :text-column-separator-width
                              vkbd-text-column-separator-width
                              vkbd-text-column-separator-display
                              'vkbd-text-column-separator))

;;;;;;; Insert Row Separator

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

;;;;;;; Update Keys

(defun vkbd-text-key-bounds-by-id (key-id)
  "Return the region (START . END) of the text key with KEY-ID or nil."
  (when-let* ((data (text-property-search-forward 'vkbd-key-id key-id #'eq)))
    (cons (prop-match-beginning data) (prop-match-end data))))

(defun vkbd-update-text-key-face (keyobj)
  "Update the face of the text key represented by KEYOBJ.
The face is set to `vkbd-text-key-pressed' if the key is pressed,
otherwise `vkbd-text-key'."
  (with-current-buffer (vkbd-keyboard-buffer (vkbd-key-object-keyboard keyobj))
    (save-excursion
      (goto-char (point-min))
      (when-let* ((bounds
                   (vkbd-text-key-bounds-by-id (vkbd-key-object-id keyobj))))
        (let ((face (vkbd-text-key-obj-face keyobj)))
          (vkbd-log "Update Key Display: bounds=%s face=%s key-id=%s"
                    bounds face (vkbd-key-object-id keyobj))
          (let ((inhibit-read-only t))
            (put-text-property (car bounds) (cdr bounds)
                               'face face)
            (put-text-property (car bounds) (cdr bounds)
                               'font-lock-face face)))))))

(defun vkbd-update-text-key (keyobj)
  "Update the display of KEYOBJ in text style."
  ;; TODO: Update text?
  (vkbd-update-text-key-face keyobj))

(defun vkbd-search-text-key (&optional limit)
  "Search for a key at or after point in the current buffer.
Return nil if no key is found.
If found, move point to the end of the key (just after the key text)
and return (KEYOBJ START) where START is the beginning of the key text."
  (let* ((start (point))
         (keyobj
          (or (get-text-property start 'vkbd-key-object)
              (and (setq start
                         (next-single-property-change start 'vkbd-key-object
                                                      nil limit))
                   (get-text-property start 'vkbd-key-object)))))
    (when (and keyobj (or (null limit) (< start limit)))
      (let ((end (or (next-single-property-change start 'vkbd-key-object
                                                  nil limit)
                     (point-max))))
        (goto-char end)
        (list keyobj start)))))

(defun vkbd-scan-text-keys (key-fun &optional limit)
  "Search all text-style keys in the current buffer and call KEY-FUN for each.

KEY-FUN is called with arguments (KEYOBJ START) where:

  - START points to the beginning of the key
  - (point) points to the end of the key (the next character)

KEY-FUN must leave point at the end of the key."
  (save-excursion
    (goto-char (point-min))
    (let (args-keyobj-start)
      (while (setq args-keyobj-start (vkbd-search-text-key limit))
        ;; KEY-FUN is called with (KEYOBJ START)
        ;; END = (point)
        ;; KEY-FUN must leave point at the end of the key.
        (apply key-fun args-keyobj-start)))))

(defun vkbd-update-text-keyboard (keyboard)
  "Update the display of KEYBOARD in text style."
  (with-current-buffer (vkbd-keyboard-buffer keyboard)
    (let ((inhibit-read-only t))
      (vkbd-scan-text-keys
       (lambda (keyobj start)
         (delete-region start (point))
         ;; Preserve the existing KEYOBJ (don't recreate it).
         (insert (vkbd-text-key-string keyobj)))))))

;;;;;;; Erase Buffer Contents

(defun vkbd-erase-text-keyboard (keyboard)
  "Completely erase the display of KEYBOARD in text style."
  (with-current-buffer (vkbd-keyboard-buffer keyboard)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (goto-char (point-min)))))

;;;;;;; Rebuild Buffer Contents

(defun vkbd-rebuild-text-keyboard (keyboard)
  "Rebuild the display of KEYBOARD in text style."
  (with-current-buffer (vkbd-keyboard-buffer keyboard)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (vkbd-insert-text-keyboard keyboard))))

;;;;;;; Faces

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

(defface vkbd-text-key-pressed
  '((((type x w32 ns haiku pgtk android) (class color)
      (min-colors 88))	; Like default mode line
     :inherit vkbd-text-key-common
     :background "black" :foreground "lightgrey")
    (((type x w32 ns haiku pgtk android))
     :inherit vkbd-text-key-common
     :background "black" :foreground "white"))
  "Face for pressed keys."
  :group 'vkbd-text-style)

(defface vkbd-text-key-locked
  '((((type x w32 ns haiku pgtk android) (class color)
      (min-colors 88))	; Like default mode line
     :inherit vkbd-text-key-common
     :background "black" :foreground "#e00")
    (((type x w32 ns haiku pgtk android))
     :inherit vkbd-text-key-common
     :background "black" :foreground "white"))
  "Face for locked keys."
  :group 'vkbd-text-style)

(defface vkbd-text-key-invisible
  '((t :inherit vkbd-text-key-common))
  "Face for invisible keys.
Invisible keys are used to adjust the position of normal keys."
  :group 'vkbd-text-style)

(defface vkbd-text-column-separator
  '((t (:inherit vkbd-text-keyboard
                 :height 2.0)))
  "Face for spacing between columns.
Changing the height of this face also changes the height of keys."
  :group 'vkbd-text-style)

(defface vkbd-text-row-separator
  '((t (:inherit vkbd-text-keyboard)))
  "Face for spacing between rows."
  :group 'vkbd-text-style)

(defface vkbd-text-title-bar
  '((t (:inherit vkbd-text-keyboard)))
  "Face for title bars."
  :group 'vkbd-text-style)

(defface vkbd-text-title-button
  '((t (:inherit vkbd-text-title-bar
                 :background "#505050")))
  "Face for buttons on title bars."
  :group 'vkbd-text-style)

(defface vkbd-text-title-button-separator
  '((t (:inherit vkbd-text-title-bar
                 :height 1.2)))
  "Face for spacing between buttons.
Changing the height of this face also changes the height of title bars."
  :group 'vkbd-text-style)

(defface vkbd-text-title-caption
  '((t (:inherit vkbd-text-title-bar
                 :foreground "#c0c0c0")))
  "Face for title text."
  :group 'vkbd-text-style)

(defun vkbd-get-face-opt (options face-name)
  (or (when-let* ((faces (plist-get options :faces))) ;; alist
        (alist-get face-name faces))
      face-name))
;; TEST: (vkbd-get-face-opt '(:faces ((face-a . (:height 2)))) 'face-a) => (:height 2)
;; TEST: (vkbd-get-face-opt '(:faces ((face-a . (:height 2)))) 'face-b) => face-b

(defun vkbd-text-key-propertized (string &rest properties)
  (unless (plist-member properties 'face)
    (setq properties (cons 'face
                           (cons 'vkbd-text-keyboard properties))))
  (unless (plist-member properties 'font-lock-face)
    (setq properties (cons 'font-lock-face
                           (cons (plist-get properties 'face) properties))))
  (unless (plist-member properties 'pointer)
    (setq properties (cons 'pointer (cons 'arrow properties))))
  (apply #'propertize string properties))

(defun vkbd-insert-propertized (string &rest properties)
  (insert (apply #'vkbd-text-key-propertized string properties)))

;;;;;; Style Definition

(defconst vkbd-text01-style
  (list 'vkbd-text01-style
        :erase-keyboard #'vkbd-erase-text-keyboard
        :insert-keyboard #'vkbd-insert-text-keyboard
        :update-keyboard #'vkbd-update-text-keyboard
        :update-key #'vkbd-update-text-key
        :translate-event #'vkbd-text-keyboard-translate-event))

;;;;; Keyboard Layouts

;; <keyboard-layout> :
;;   ( <keyboard-row> ... [ <keyboard-plist> ])
;;
;; <keyboard-row> :
;;   ( <key-spec> ... )
;;
;; <keyboard-plist> : Currently undefined

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
  '((esc tab (?` ?~) (?' ?^) (?\" ?&) (?: ?*) (?\( ?\[) (?\) ?\]) (?- ?|) shf)
    (?1 ?2 (?3 ?!) (?4 ?#) (?5 ?$) (?6 ?%) (?7 ?{) (?8 ?}) (?9 ?\\) (?0 ?=))
    (?q ?w ?e ?r ?t ?y ?u ?i       ?o       ?p)
    (?a ?s ?d ?f ?g ?h ?j ?k       ?l       (?\; ?+))
    (?z ?x ?c ?v ?b ?n ?m (?, ?<)  (?. ?>)  (?/ ?_))
    (shf ctl met hom end pup ?@  up  ??  bs)
    (M-x C-x C-c C-g spc pdw lft dwn rit ret))
  "Compact version of `vkbd-layout-10x9' with 7 rows.
Most symbols require shift to enter. Ins and Del keys are removed.")

(defconst vkbd-layout-11x7
  '((esc tab ?^  ?@  ?*  ?+  ?-      ?=      (?\( ?\[) (?\) ?\]) shf)
    (?1  ?2  ?3  ?4  (?5 ?!) (?6 ?#) (?7 ?$) (?8 ?%)   (?9 ?{)  (?0 ?}) (?` ?~))
    (?q  ?w  ?e  ?r  ?t  ?y  ?u      ?i      ?o        ?p       (?' ?&))
    (?a  ?s  ?d  ?f  ?g  ?h  ?j      ?k      ?l        ?:       (?\" ?|))
    (?z  ?x  ?c  ?v  ?b  ?n  ?m      ?,      (?. ?<)   (?\; ?>) (?/ ?\\))
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

(defconst vkbd-layout-special-keys-only
  '((esc C-x C-c M-x C-g ins hom up  end pup)
    (tab shf ctl met men del lft dwn rit pdw))
  "Layout containing only special keys, intended for use with existing
keyboards.

Note: The modifier lock feature does not work correctly when used with
existing keyboards.")

(defconst vkbd-layout-list
  '(vkbd-layout-10x7
    vkbd-layout-10x9
    vkbd-layout-11x7
    vkbd-layout-us
    vkbd-layout-jp
    vkbd-layout-special-keys-only))

(defun vkbd-layout-list ()
  vkbd-layout-list)

(defcustom vkbd-default-keyboard-layout 'vkbd-layout-10x7
  "Default keyboard layout to use.

This can be either a symbol naming a variable that holds a layout list,
or a layout list itself."
  :type `(choice ,@(mapcar (lambda (name) `(const ,name))
                           vkbd-layout-list)
                 (symbol :tag "Variable name")
                 sexp)
  :group 'vkbd)

(defun vkbd-concrete-keyboard-layout-p (layout)
  (and (consp layout) (listp (car layout))))

(defun vkbd-resolve-keyboard-layout-spec (layout)
  "Convert a layout specification LAYOUT to a layout list.

LAYOUT can be a symbol (variable name) or a list."
  (or
   ;; Variable name
   (when (and (symbolp layout) (not (null layout)))
     (default-value layout))
   ;; List of list
   (when (vkbd-concrete-keyboard-layout-p layout)
     layout)))

(defun vkbd-default-keyboard-layout (options)
  "Return the default layout list to use.

OPTIONS is a property list that may contain a `:layout' property."
  (or (vkbd-resolve-keyboard-layout-spec (plist-get options :default-layout))
      (vkbd-resolve-keyboard-layout-spec vkbd-default-keyboard-layout)
      vkbd-layout-10x7))

(defun vkbd-map-keyboard-layout-keys (layout
                                      key-fun between-cols-fun between-rows-fun)
  (when (vkbd-concrete-keyboard-layout-p layout)
    (let ((rows layout))
      (while rows
        (unless (eq rows layout)
          (funcall between-rows-fun))
        (let* ((curr-row (car rows))
               (cols (car rows)))
          (while cols
            (unless (eq cols curr-row)
              (funcall between-cols-fun))
            (let ((key-spec (car cols)))
              (funcall key-fun key-spec))
            (setq cols (cdr cols))))
        (setq rows (cdr rows))))))


;;;;; Keyboard Styles

;; Style Definition :
;; (defconst <style-name>
;;   ;; <keyboard-style> :
;;   '(<style-name>
;;     :insert-keyboard <function (keyboard):void>
;;     :update-keyboard <function (keyboard):void>
;;     :update-key <function (keyobj):void>
;;     :translate-event <function (keyboard prompt event):event-vector|nil>
;;    )

(defcustom vkbd-default-keyboard-style 'vkbd-text01-style
  "Default keyboard style to use.

This can be either a symbol naming a variable that holds a style
descriptor, or a style descriptor itself."
  :type '(choice (const vkbd-text01-style)
                 (symbol :tag "Variable name")
                 sexp)
  :group 'vkbd)

(defun vkbd-concrete-keyboard-style-p (style)
  (and (consp style) (symbolp (car style))))

(defun vkbd-resolve-keyboard-style-spec (style)
  "Convert a style specification STYLE to a style descriptor.

STYLE can be a symbol (variable name) or a style descriptor list."
  (or
   ;; Variable name
   (when (and (symbolp style) (not (null style)))
     (default-value style))
   ;; Style descriptor
   (when (vkbd-concrete-keyboard-style-p style)
     style)))

(defun vkbd-default-keyboard-style (&optional options)
  "Return the default style descriptor to use.

OPTIONS is a property list that may contain a `:style' property."
  (or (vkbd-resolve-keyboard-style-spec (plist-get options :style))
      (vkbd-resolve-keyboard-style-spec vkbd-default-keyboard-style)
      vkbd-text01-style))
;; (vkbd-default-keyboard-style nil)

(defun vkbd-keyboard-style-plist (style)
  "Return the property list of the keyboard STYLE."
  (when (vkbd-concrete-keyboard-style-p style)
    (cdr style)))

(defun vkbd-keyboard-style-property (style prop &optional default)
  "Get property PROP from the keyboard STYLE.

Return DEFAULT if PROP is not found."
  (or
   (plist-get (vkbd-keyboard-style-plist style) prop)
   default))

(defun vkbd-keyboard-style-funcall (keyboard prop &optional default &rest args)
  "Call the function specified by property PROP in the KEYBOARD style.

If PROP is not found or not a function, use DEFAULT instead.
ARGS are passed to the function."
  (let ((fun (vkbd-keyboard-style-property
              (vkbd-keyboard-style keyboard) prop default)))
    (when (functionp fun)
      (apply fun args))))

(defun vkbd-keyboard-style--erase-keyboard (keyboard)
  "Erase KEYBOARD display."
  (vkbd-keyboard-style-funcall keyboard :erase-keyboard nil keyboard))

(defun vkbd-keyboard-style--insert-keyboard (keyboard)
  "Insert KEYBOARD display into the buffer."
  (vkbd-keyboard-style-funcall keyboard :insert-keyboard nil keyboard))

(defun vkbd-keyboard-style--update-keyboard (keyboard)
  "Update KEYBOARD display."
  (vkbd-keyboard-style-funcall keyboard :update-keyboard nil keyboard))

(defun vkbd-keyboard-style--update-key (keyobj)
  "Update the display of the key specified by KEYOBJ."
  (vkbd-keyboard-style-funcall (vkbd-key-object-keyboard keyobj) :update-key nil
                               keyobj))

(defun vkbd-keyboard-style--translate-event (keyboard prompt event)
  "Translate EVENT using the style specified in KEYBOARD.

PROMPT is the prompt string for the current key sequence."
  (vkbd-keyboard-style-funcall keyboard :translate-event nil
                               keyboard prompt event))


;;;; Data Storage

(defun vkbd-data-storage-load (data-storage-spec data-alist)
  "Load data from the storage location specified by DATA-STORAGE-SPEC.

DATA-ALIST is an alist containing the item names of the data to load.
Only the keys of the alist are used; the values are ignored."
  (when data-storage-spec
    (funcall (car data-storage-spec) 'load (cdr data-storage-spec) data-alist)))

(defun vkbd-data-storage-save (data-storage-spec data-alist)
  "Save data to the storage location specified by DATA-STORAGE-SPEC.

DATA-ALIST is an alist of the data to save. Its keys must be comparable
with `eq'.

DATA-STORAGE-SPEC has the form (<data-storage-function> . <ds-option-plist>).

<data-storage-function> is a function that takes
(action <ds-option-plist> DATA-ALIST) as arguments.
The action argument is the symbol `load' or `save'.

The only <data-storage-function> provided by this library is the
`vkbd-data-storage' function."
  (when data-storage-spec
    ;; `vkbd-data-storage'
    (funcall (car data-storage-spec) 'save (cdr data-storage-spec) data-alist)))

;; <data-storage-spec> :
;;   <default-data-storage-spec>
;;   <custom-data-storage-spec>

;; <custom-data-storage-spec> : (<data-storage-function> . <plist>)

;; <default-data-storage-spec> : (vkbd-data-storage . <ds-params>)
;; Related function: `vkbd-data-storage'

;; <ds-params> : <ds-param>...
;; <ds-param> :
;;  :default-file (<default-file-prop>...)
;;  :default-destination <save-destination>
;;  :item-destination ((<item-name> . <save-destination>)...)
;; <item-name> : <symbol>
;; <default-file-prop> :
;;   :file <filename>
;;   :value-place <value-place>

;; <save-destination> : (<store-location> <value-place>)
;; Related variable: `vkbd-data-storage-cus-type-save-destination'
(defmacro vkbd-data-storage--dest-store-location (dest) `(car ,dest))
(defmacro vkbd-data-storage--dest-value-place (dest) `(cadr ,dest))

;; <store-location> :
;;   (file <filename>?)
;;   (variable <varname>)
;;   (custom-variable <varname>)
;;   (function <function>)
;; Related variable: `vkbd-data-storage-cus-type-save-destination'
(defmacro vkbd-data-storage--location-type (store-location)
  `(car ,store-location))
(defmacro vkbd-data-storage--location-file (store-location)
  `(cadr ,store-location))
(defmacro vkbd-data-storage--location-name (store-location)
  `(cadr ,store-location))
(defmacro vkbd-data-storage--location-function (store-location)
  `(cadr ,store-location))
(defun vkbd-data-storage--make-store-location-file (filename)
  (list 'file filename))

;; <value-place> :
;;   (value-only)
;;   (alist <use-item-name> <alist-keys>)
;;   nil
;; Related variable: `vkbd-data-storage-cus-type-value-place'
(defmacro vkbd-data-storage--value-place-type (value-place)
  `(car ,value-place))
(defmacro vkbd-data-storage--value-place-alist-use-item-name (value-place)
  `(cadr ,value-place))
(defmacro vkbd-data-storage--value-place-alist-keys (value-place)
  `(caddr ,value-place))

(defconst vkbd-data-storage-cus-type-value-place
  '(choice :tag "Value place"
           (const :tag "Value only" (value-only))
           (list :tag "In alist"
                 (const :format "" alist)
                 (boolean :tag "Use item name as key" :value t)
                 (repeat :tag "Parent keys";; :value (default)
                         symbol))
           (const :tag "Default" nil)))

(defconst vkbd-data-storage-cus-type-save-destination
  `(choice
    :value ((file) nil)
    (list :tag "File"
          (cons :format "%v"
                (const :format "" file)
                (choice :tag "Save to"
                        (const :tag "Default file" nil)
                        (list :tag "Specific file"
                              (file :tag "Save file name" :value "vkbd"))))
          ,vkbd-data-storage-cus-type-value-place)
    (list :tag "Variable"
          (list :format "%v"
                (const :format "" variable)
                (variable :tag "Variable Name"))
          ,vkbd-data-storage-cus-type-value-place)
    (list :tag "Customization Variable"
          (list :format "%v"
                (const :format "" custom-variable)
                (variable :tag "Variable Name"))
          ,vkbd-data-storage-cus-type-value-place)
    (cons :tag "Function"
          (list :format "%v"
                (const :format "" function)
                (function :tag "Function"))
          ,vkbd-data-storage-cus-type-value-place)
    (const :tag "Don't save" nil)))

(defconst vkbd-data-storage-cus-type
  `(choice
    :value (vkbd-data-storage)
    (cons :tag "Default data storage"
          (const :format "" vkbd-data-storage)
          (set :tag "Options"
               (list :inline t :tag "Default file"
                     (const :format "" :default-file)
                     (set :format "%v"
                          (list :inline t :format "%v"
                                (const :format "" :file)
                                (file :tag "Save file name" :value "vkbd"))
                          (list :inline t :format "%v"
                                (const :format "" :value-place)
                                ,vkbd-data-storage-cus-type-value-place)))
               (list :inline t :tag "Default save destination"
                     (const :format "" :default-destination)
                     ,vkbd-data-storage-cus-type-save-destination)
               (list :inline t :tag "Item-specific save destination"
                     (const :format "" :item-destination)
                     (alist
                      :key-type (choice
                                 :tag "Item name"
                                 ,@(mapcar (lambda (name)
                                             `(const ,name))
                                           vkbd-keyboard-user-data-item-names)
                                 (symbol :tag "Item name"))
                      :value-type
                      ,vkbd-data-storage-cus-type-save-destination))))
    (cons :tag "Custom data storage"
          (function :tag "User data storage function")
          (repeat :tag "Parameters"
                  (list :tag "Param" :inline t
                        (symbol :tag "Key")
                        (sexp :tag "Value"))))
    (const :tag "Don't save" nil)))

(defcustom vkbd-global-keyboard-user-data-storage
  '(vkbd-data-storage
    :default-file (:file "vkbd" :value-place (alist t (global-keyboard)))
    :default-destination ((file) nil)
    :item-destination nil)
  "User data storage specification for the global keyboard.
See `vkbd-data-storage-save' and `vkbd-data-storage' for how to specify this."
  :group 'vkbd
  :type vkbd-data-storage-cus-type)

(defun vkbd-global-keyboard-user-data-storage ()
  vkbd-global-keyboard-user-data-storage)

(defun vkbd-data-storage--make-location-items-alist (ds-params data-alist)
  (let ((default-file (plist-get ds-params :default-file))
        (default-dest (plist-get ds-params :default-destination))
        (item-dest-alist (plist-get ds-params :item-destination))
        (location-item-alist nil))
    (dolist (item data-alist)
      (let* ((item-name (car item))
             (item-value (cdr item))
             (item-dest (alist-get item-name item-dest-alist))
             (dest (or item-dest default-dest))
             (store-location (vkbd-data-storage--dest-store-location dest))
             (value-place (vkbd-data-storage--dest-value-place dest))
             (store-type (vkbd-data-storage--location-type store-location)))
        ;; Normalize STORE-LOCATION and VALUE-TYPE
        (pcase store-type
          ('file
           (setq store-location
                 (vkbd-data-storage--make-store-location-file
                  (locate-user-emacs-file
                   (or (vkbd-data-storage--location-file store-location)
                       (plist-get default-file :file)
                       "vkbd"))))
           (unless value-place
             (setq value-place (or (plist-get default-file :value-place)
                                   '(alist t (default))))))
          ((or 'variable 'custom-variable)
           ;; TODO: Check boundp ?
           (unless (vkbd-data-storage--location-name store-location)
             (setq store-location nil))
           (unless value-place
             (setq value-place (if item-dest '(value-only) '(alist t nil)))))
          ('function
           (unless (functionp
                    (vkbd-data-storage--location-function store-location))
             (setq store-location nil))
           (unless value-place
             (setq value-place (if item-dest '(value-only) '(alist t nil))))))

        (when (and store-location value-place)
          (push
           ;; item-access:
           ;; (value-place item-name item-value)
           (list value-place item-name item-value)
           (alist-get store-location location-item-alist nil nil #'equal)))))
    location-item-alist))
;; TEST: (vkbd-data-storage--make-location-items-alist '(:default-file (:file "vkbd" :value-place (alist t (global-keyboard))) :default-destination ((file) nil)) '((frame-position . (10 . 20)) (layout . vkbd-layout-10x9))) => (((file "~/.emacs.d/vkbd") ((alist t (global-keyboard)) layout vkbd-layout-10x9) ((alist t (global-keyboard)) frame-position (10 . 20))))


(defun vkbd-data-storage (action ds-params data-alist)
  "Save or load data to/from the storage location specified by DS-PARAMS,
which is a plist.

When action is the symbol `load', data is loaded; when `save', data is saved.

During loading, only the keys of DATA-ALIST are used as item names to
load; values are ignored.

The properties that can be specified in DS-PARAMS are as follows:

  :default-file (<default-file-prop>...)
  :default-destination <save-destination>
  :item-destination ((<item-name> . <save-destination>)...)

The :default-file property specifies the default filename and the
location within it for data storage. Its value is a plist with the
following properties:

  :file <filename>
  :value-place <value-place>

  If <filename> is a relative path, the absolute path is determined by
  `locate-user-emacs-file'. Typically, it is a relative path from
  ~/.emacs.d/.

  <value-place> specifies how data is stored within the file (described
  later). If nil or unspecified, it defaults to (alist t (default)),
  which creates an alist under the top-level key `default', with item
  names as keys and item values as values.

The :default-destination property specifies the default save destination.

The :item-destination property specifies save destinations for each item
name as an alist, where <item-name> keys are symbols.

A <save-destination> that specifies a storage location has the following
list format, indicating the storage location and how values are
represented within it:

  (<store-location> <value-place>)

<store-location> is one of the following:

  (file [<filename>])
  (variable <varname>)
  (custom-variable <varname>)
  (function <function>)

<value-place> is one of the following:
  (value-only)
  (alist <use-item-name> <alist-keys>)
  nil

If nil is specified for <value-place>, a context-dependent default value
is used."
  (pcase action
    ('save (vkbd-data-storage--save ds-params data-alist))
    ('load (vkbd-data-storage--load ds-params data-alist))))

(defun vkbd-data-storage--save (ds-params data-alist)
  (let ((customized nil))
    (dolist (store-location-and-items
             (vkbd-data-storage--make-location-items-alist ds-params
                                                           data-alist))
      (let* ((store-location (car store-location-and-items))
             (item-list (cdr store-location-and-items))
             ;; Load from store
             (store-value (vkbd-data-storage--load-store-value store-location)))
        ;; Modify store value
        (dolist (item-access item-list)
          (setq store-value
                (apply
                 #'vkbd-data-storage--set-value
                 store-value
                 ;; (value-place item-name item-value)
                 item-access)))

        ;; Save to store
        (vkbd-data-storage--save-store-value store-location store-value)

        (when (eq (vkbd-data-storage--location-type store-location)
                  'custom-variable)
          (setq customized t))))
    ;; Save custom file
    (when customized
      ;; `customize-save-customized' calls `custom-save-all'
      (customize-save-customized))))

(defun vkbd-data-storage--set-value (store-value
                                     value-place item-name item-value)
  (pcase (vkbd-data-storage--value-place-type value-place)
    ('value-only
     (setq store-value item-value))
    ('alist
     (let ((use-item-name
            (vkbd-data-storage--value-place-alist-use-item-name value-place))
           (keys
            (vkbd-data-storage--value-place-alist-keys value-place))
           (root (cons nil store-value)))
       (when use-item-name
         (setq keys (append keys (list item-name))))
       (ignore-errors
         (let ((node root))
           (dolist (k keys)
             (let ((sub-node (assq k (cdr node))))
               (unless sub-node
                 (setq sub-node (cons k nil))
                 (push sub-node (cdr node)))
               (setq node sub-node)))
           (setcdr node item-value)))
       (setq store-value (cdr root)))))
  store-value)

(defun vkbd-data-storage--load (ds-params data-alist)
  (let (result)
    (dolist (store-location-and-items
             (vkbd-data-storage--make-location-items-alist ds-params
                                                           data-alist))
      (let* ((store-location (car store-location-and-items))
             (item-list (cdr store-location-and-items))
             ;; Load from store
             (store-value (vkbd-data-storage--load-store-value store-location)))
        ;; Collect item values from store value
        (dolist (item-access item-list)
          (let ((item
                 (apply
                  #'vkbd-data-storage--get-value
                  store-value
                  ;; (value-place item-name item-value)
                  item-access)))
            (when item
              (push item result))))))
    (nreverse result)))

(defun vkbd-data-storage--get-value (store-value
                                     value-place item-name _item-value)
  (pcase (vkbd-data-storage--value-place-type value-place)
    ('value-only
     (cons item-name store-value))
    ('alist
     (let ((use-item-name
            (vkbd-data-storage--value-place-alist-use-item-name value-place))
           (keys
            (vkbd-data-storage--value-place-alist-keys value-place)))
       (when use-item-name
         (setq keys (append keys (list item-name))))
       (ignore-errors
         (let ((node store-value))
           (while (and keys (setq node (alist-get (car keys) node)))
             (setq keys (cdr keys)))
           (cons item-name node)))))))

(defun vkbd-data-storage--load-store-value (store-location)
  (pcase (vkbd-data-storage--location-type store-location)
    ;; (file <filename>)
    ('file
     (vkbd-data-storage--load-file
      (vkbd-data-storage--location-file store-location)))
    ;; (variable|custom-variable <name>)
    ((or 'variable 'custom-variable)
     ;; TODO: Call `custom-load-symbol' ?
     (default-value
      (vkbd-data-storage--location-name store-location)))
    ;; (function <function>)
    ('function
     (funcall
      (vkbd-data-storage--location-name store-location) 'load))))

(defun vkbd-data-storage--save-store-value (store-location store-value)
  (pcase (vkbd-data-storage--location-type store-location)
    ;; (file <filename>)
    ('file
     (vkbd-data-storage--save-file
      (vkbd-data-storage--location-file store-location)
      store-value))
    ;; (variable <name>)
    ('variable
     (setf (default-value (vkbd-data-storage--location-name store-location))
           store-value))
    ;; (custom-variable <name>)
    ('custom-variable
     ;; TODO: custom-set-variable? setopt?
     (customize-set-variable
      (vkbd-data-storage--location-name store-location)
      store-value))
    ;; (function <function>)
    ('function
     (funcall (vkbd-data-storage--location-name store-location) 'save
              store-value))))

(defun vkbd-data-storage--load-file (file)
  (condition-case _err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (read (current-buffer)))
    (error nil)))

(defun vkbd-data-storage--save-file (file value)
  (with-temp-file file
    (insert
     ";;; vkbd - Virtual Keyboard Data ---   -*- mode: lisp-data -*-\n")
    (pp value (current-buffer))))


;;;; Utilities

;;;;; Frame Management

;; Basic frame functions (with recycling mechanism):
;; - `vkbd-make-frame'
;; - `vkbd-delete-frame'
;; - `vkbd-delete-all-unused-frames'
;; - `vkbd-frame-live-p'

(defconst vkbd-unmodifiable-frame-parameters
  '(border-width)
  "List of frame parameters that cannot be modified after frame creation.
Attempting to modify these parameters after creation causes errors.")

(defun vkbd-remove-unmodifiable-frame-parameters (frame-parameters)
  "Remove parameters from FRAME-PARAMETERS that cannot be modified after
frame creation."
  (seq-remove (lambda (param-value)
                (memq (car param-value) vkbd-unmodifiable-frame-parameters))
              frame-parameters))

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
  ;; Transfor focus to other
  (when (eq (selected-frame) frame)
    (if-let* ((parent-frame (and (frame-live-p frame)
                                 (frame-parent frame))))
        (select-frame-set-input-focus parent-frame)
      (other-frame 1)))

  (if vkbd-recycle-frames
      (vkbd-delete-frame-for-reuse frame)
    (vkbd-delete-frame-immediately frame)))

(defun vkbd-delete-frame-for-reuse (frame)
  "Delete FRAME created by `vkbd-make-frame'.
FRAME is saved as an unused frame and may be recycled."
  (when (frame-live-p frame)
    (vkbd-kill-dedicated-buffer (frame-root-window frame))
    (redirect-frame-focus frame nil)

    (make-frame-invisible frame t)
    ;; Invisible frames can interfere with motion events, so move them
    ;; out of the way. (Emacs 29.1 for Windows)
    ;;(set-frame-position frame -1000 -1000)
    (modify-frame-parameters frame '((left . (+ -1000))
                                     (top . (+ -1000))))

    (setq vkbd-unused-frames (nconc vkbd-unused-frames (list frame)))
    (vkbd-prune-unused-frames)))

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

(defun vkbd-read-event-silent (&optional not-keep-echo-area seconds)
  ;; Suppress display of events
  ;; (e.g. down-mouse-1-) in echo area
  (let ((echo-keystrokes 0))
    (if not-keep-echo-area
        (vkbd-read-event-from-internal nil nil seconds)
      (let ((old-message (current-message)))
        (prog1 (vkbd-read-event-from-internal nil nil seconds)
          (vkbd-echo old-message))))))

(defun vkbd-input-event-p (event)
  (and
   event
   (or (integerp event)
       (mouse-event-p event) ;; (|double-|triple-)(|down-|drag-)mouse-(1|2|3)
       (memq (car-safe event)
             '(touchscreen-begin touchscreen-update touchscreen-end)))))

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
