;;
;; Copyright 2022, Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;


;; A simple module for making "template" strings á la `format` (but less flexible, more
;; broken, and based on named/keyed parameters rather than positional parameters).
;; TL;DR, "hi #{{role}}!!" instead of "hi ~A!!"
;; (Helper module for feedsnake.)
(module named-format
	(named-format)

(import scheme
		(chicken base) (chicken string)
		srfi-1 srfi-13 (only srfi-130 string-split)
		format)


;; Replaces all named "parameters" within a string with their associated values in the given alist.
;; A string with two parameters might look something like this:
;;      "You're about to eat #{{food||rice}} for #{{cost}}! Bonne apetit!"
;; Whose parameter alist might look like this:
;;      '((food "omlette") (cost "$9.00"))
;; If you don't specify a parameter's value in the alist, it's #{{mention}} in the string will just
;; be removed, or replaced with a default value, if provided like so:
;;      #{{food||rice}}
;; has a default value of "rice".
(define (named-format string parameter-alist)
  (replace-parameters string
					  parameter-alist
					  (reverse (string-contains-all string "~{{" 0))
					  #f))


;; Replaces the parameter mentions in the given string at the given indices with their
;; associated alist values. The `parameter-indices` list must be given in reverse order
;; (e.g., 100, 99, 73).
(define (replace-parameters string parameters parameter-indices previous-index)
  (let* ([index (car parameter-indices)]
		 [next-index (if (null? (cdr parameter-indices))
						 #f
						 (cadr parameter-indices))]
		 [substituted
		  (replace-parameter string parameters index (or previous-index (string-length string)))])
	(if next-index
		(replace-parameters substituted parameters (cdr parameter-indices) index)
		substituted)))


;; Substitute a single string's parameter beginning at the given index.
;; This is bad, I'm genuinely ashamed. Please forgive me ;-;
(define (replace-parameter string parameters index max-index)
  (let* ([end-of-substitution (+ 2 (string-contains string "}}" index max-index))]
		 [columns (string-split
				   (string-copy string (+ 3 index) (- end-of-substitution 2))
				   "||")]
		 [key-symbols (map string->symbol (cddr columns))]
		 [values (filter (lambda (a) a)
						 (map (lambda (key)
								(let ([value (alist-ref key parameters)])
								  (if value (car value) #f)))
							  key-symbols))]
		 [value (if (null? values)
					(cadr columns)
					(car values))]
		 [formatting (if (and (string? value) (string-null? value))
						 "~A"
						 (car columns))])
	(string-replace string (format formatting value) index end-of-substitution)))


;; A list of all instances of a substring within a string, by index
(define (string-contains-all string needle from-index)
  (let* ([match-index (string-contains string needle from-index)]
		 [next-match-index (string-contains string needle (+ 1 match-index))])
	(if next-match-index
		(append (list match-index)
				(string-contains-all string needle (+ 1 match-index)))
		(list match-index))))

) ;; named-format module
