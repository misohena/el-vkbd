;;; vkbd-tool-bar.el --- Virtual Keyboard Tool Bar -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Keyboard, Input Method, Mouse, Touch, Toolbar

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

;; Add/Remove global keyboard button:
;;   - `vkbd-add-to-tool-bar'
;;   - `vkbd-remove-from-tool-bar'

;;; Code:

(require 'tool-bar)

(autoload 'vkbd-toggle-global-keyboard "vkbd" nil t)

;;;; Global Keyboard Button on Toolbar

(defconst vkbd-keyboard-icon
  ;; Source: ./icons.org
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 240 240\"
 width=\"24\" height=\"24\" version=\"1.1\"
 stroke-width=\"2\" stroke=\"none\" fill=\"black\">
<rect ry=\"16\" rx=\"16\" stroke=\"black\" fill=\"#ebebeb\" stroke-width=\"8\"
 height=\"120\" width=\"224\" y=\"72\" x=\"8\" />
<rect fill=\"black\" stroke=\"none\" stroke-width=\"2\"
 height=\"19\" width=\"19\" y=\"94\" x=\"26\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"54\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"82\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"110\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"138\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"166\" />
<rect height=\"19\" width=\"19\" y=\"94\" x=\"194\" />
<rect height=\"19\" width=\"31\" y=\"124\" x=\"26\" />
<rect height=\"19\" width=\"19\" y=\"124\" x=\"66\" />
<rect height=\"19\" width=\"19\" y=\"124\" x=\"94\" />
<rect height=\"19\" width=\"19\" y=\"124\" x=\"122\" />
<rect height=\"19\" width=\"19\" y=\"124\" x=\"150\" />
<rect height=\"19\" width=\"35\" y=\"124\" x=\"178\" />
<rect height=\"19\" width=\"23\" y=\"154\" x=\"26\" />
<rect height=\"19\" width=\"19\" y=\"154\" x=\"58\" />
<rect height=\"19\" width=\"71\" y=\"154\" x=\"86\" />
<rect height=\"19\" width=\"19\" y=\"154\" x=\"166\" />
<rect height=\"19\" width=\"19\" y=\"154\" x=\"194\" />
<path d=\"M122 72C126 56 139 20 151 40S173 22 177 6\"
 fill=\"none\" stroke=\"#101010\" stroke-width=\"8\" /></svg>")

(defvar vkbd-global-keyboard-tool-bar-button-scale 'default)

(defun vkbd-make-tool-bar-icon-image ()
  (create-image vkbd-keyboard-icon 'svg t
                :scale vkbd-global-keyboard-tool-bar-button-scale))

;;;###autoload
(defun vkbd-add-to-tool-bar ()
  "Add toolbar button that shows/hides the virtual keyboard.

This enables `vkbd-tool-bar-mode'."
  (interactive)
  (vkbd-tool-bar-mode))

;;;###autoload
(defun vkbd-remove-from-tool-bar ()
  "Remove toolbar button that shows/hides the virtual keyboard.

This disables `vkbd-tool-bar-mode'."
  (interactive)
  (vkbd-tool-bar-mode -1))

;;;###autoload
(define-minor-mode vkbd-tool-bar-mode
  "Toggle display of toolbar button for virtual keyboard.

Vkbd Toolbar Mode adds a toolbar button that invokes the
`vkbd-toggle-global-keyboard' command."
  :group 'vkbd
  :global t
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [tool-bar vkbd-toggle-global-keyboard]
                `(menu-item "VKbd" vkbd-toggle-global-keyboard
                            :image ,(vkbd-make-tool-bar-icon-image)))
    km))


(provide 'vkbd-tool-bar)
;;; vkbd-tool-bar.el ends here
