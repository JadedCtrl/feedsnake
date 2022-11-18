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

;; Simple helper module for feedsnake; converts between datestrings of different
;; formats and srfi-19 date objects.
;; Some of these formats have dedicated output codes in srfi-19 (~c, ~x, etc),
;; but it looks like the chicken doesn't support them.
(module date-strings
	(date->rfc339-string rfc339-string->date date->rfc228-string date->mbox-string)

(import scheme
		(chicken condition) (chicken format)
		srfi-19 srfi-13)


;; Converts a date into an rfc399 string
(define (date->rfc339-string date)
  (date->string date "~Y-~m-~dT~H:~M:~S~z"))


;; Converts an rfc339 string into a datetime, timezone optional
;; … copied but not exported by feedsnake? mayyyybe >w>"
(define (rfc339-string->date string)
  (handle-exceptions exn
	  (handle-exceptions exn
		  (string->date string "~Y-~m-~dT~H:~M:~S")
		#f)
	(string->date string "~Y-~m-~dT~H:~M:~S~z")))


;; Date into an RFC228 (e-mail) string
(define (date->rfc228-string date)
  (let* ([month (string-titlecase (date->string date "~b"))]
		 [weekday (string-titlecase (date->string date "~a"))]
		 [timezone-raw (date->string date "~z")]
		 [timezone (if (string=? timezone-raw "Z") "+0000" timezone-raw)])
	(format (date->string date "~~A, ~d ~~A ~Y ~T ~~A") weekday month timezone)))


;; Converts a date into an mbox From-compatible string
(define (date->mbox-string date)
  (let* ([month (string-titlecase (date->string date "~b"))]
		 [weekday (string-titlecase (date->string date "~a"))])
	(format (date->string date "~~A ~~A  ~d ~T ~Y") weekday month)))

) ;; date-strings module
