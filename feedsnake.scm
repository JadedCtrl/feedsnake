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

(load "date-strings.scm")
(load "named-format.scm")


(module feedsnake
	(updated-feed-string read-feed entries-since entry->string)

(import scheme
		(chicken base) (chicken condition) (chicken io) (chicken port)
		srfi-1 srfi-19 srfi-69
		date-strings
		http-client
		named-format
		atom rss)


;; Read the given port into a feedsnake-feed (alist), no matter the format! c:<
(define (read-feed in-port)
  (let (;;[rss (rss:read in-port)]
		[atom (read-atom-feed in-port)])
	(if atom
		(atom-doc->feedsnake-feed atom)
		#f)))


;; A list of entries updated since the given date
(define (entries-since feed date-utc)
  (let ([entry-date (lambda (entry) (car (alist-ref 'updated entry)))]
		[since-entries '()])
	(map
	 (lambda (entry)
	   (if (date>=? (entry-date entry) date-utc)
		   (set! since-entries
			 (append since-entries (list entry)))))
	 (car (alist-ref 'entries feed)))
	since-entries))


;; Returns either the updated string of a feed (in comparison to old string),
;; or #f if literally nothing's changed
(define (updated-feed-string url old-string)
  (let* ([new-string (fetch-feed-string url)]
		 [updated? (not (eq? (hash old-string) (hash new-string)))])
	(if updated?
		new-string
		#f)))


;; Download a feed (AKA fetch over HTTP to a string)
(define (fetch-feed-string url)
  (call-with-output-string
   (lambda (out) (fetch-http url out))))


(define (entry->string entry template)
  (named-format
   template
   (entry-string-templating-parameters entry template)))


;; Returns an alist of string replacements/parameters for a given entry
;; For use with named-format
(define (entry-string-templating-parameters entry template)
  (let* ([alist-car
		  (lambda (key alist)
			(let ([value (alist-ref key alist)])
			  (if value
				  (car value))))]
		 [updated (or (alist-car 'updated entry) (alist-car 'published entry))]
		 [published (or (alist-car 'published entry) updated)]
		 [urls (alist-car 'url entry)])
	`((title ,(alist-car 'title entry))
	  (updated ,(if updated (date->rfc228-string updated)))
	  (published ,(if published (date->rfc228-string published)))
	  (summary ,(alist-car 'summary entry))
	  (url ,(cond
			 [(list? urls) (car urls)]
			 [(string? urls) urls]))
	  (urls ,(cond
			  [(list? urls) urls]
			  [(string? urls) (list urls)])))))


;; Parse an atom feed into a feedsnake-friendly alist
(define (atom-doc->feedsnake-feed atom)
  `((title ,(last (feed-title atom)))
	(updated ,(feed-updated atom))
	(entry-updated ,(atom-feed-latest-entry-date atom))
	(entries ,(map atom-entry->feedsnake-entry (feed-entries atom)))))


;; Parse an atom entry into a feedsnake entry :>
(define (atom-entry->feedsnake-entry entry)
  (let ([published (rfc339-string->date (entry-published entry))]
		[updated (rfc339-string->date (entry-updated entry))])
	`((title ,(last (entry-title entry)))
	  (updated ,(or updated published))
	  (published ,(or published updated))
	  (summary ,(last (entry-summary entry)))
	  (url ,(map link-uri (entry-links entry))))))


;; Get an atom feed's latest date for an entry's updating/publishing
(define (atom-feed-latest-entry-date atom)
  (let ([entry-date
		 (lambda (entry)
		   (or (rfc339-string->date (entry-updated entry))
			   (rfc339-string->date (entry-published entry))))])
	(reduce
	 (lambda (a b)
	   (if (date>=? a b) a b))
	 #f
	 (map entry-date (feed-entries atom)))))


;; Download a file over HTTP to the given port.
(define (fetch-http url out-port)
  (call-with-input-request
   url #f
   (lambda (in-port) (copy-port in-port out-port))))

) ;; feedsnake module



;; The UNIX-style frontend for feedsnake
(module feedsnake-unix
	(update-feed-file latest-entries feed-files)

(import scheme
		(chicken base) (chicken condition) (chicken file) (chicken io)
		(chicken process-context) (chicken process-context posix)
		srfi-1 srfi-19
		date-strings
		feedsnake
		xattr)


;; Switch the cached version of the feed with a newer version, if available
(define (update-feed-file feed-path)
  (let* ([old-string (call-with-input-file feed-path
					   (lambda (in-port) (read-string #f in-port)))]
		 [new-string (updated-feed-string
					  (get-xattr feed-path "user.xdg.origin.url")
					  old-string)])
	(if new-string
		(call-with-output-file feed-path
		  (lambda (out) (write-string new-string #f out))))
	new-string))


;; List of entries updated/published since last feed parsing
(define (latest-entries feed-path)
  (let* ([feed (call-with-input-file feed-path read-feed)]
		 [xattr-last-update (get-xattr feed-path "user.feedsnake.parsed")]
		 [last-update (if xattr-last-update
						  (rfc339-string->date xattr-last-update)
						  (date->utc-date (make-date 0 0 0 0 01 01 1971)))])
	(set-xattr feed-path "user.feedsnake.parsed"
			   (date->rfc339-string (current-date-utc)))
	(entries-since feed last-update)))


;; List of all entries of the feed
(define (all-entries feed-path)
  (let ([feed (call-with-input-file feed-path read-feed)])
	(car (alist-ref 'entries feed))))


;; The user's presumed config root.
(define (config-directory)
  (or (get-environment-variable "XDG_CONFIG_HOME")
	  (string-append (sixth (user-information (current-user-id))) "/.config")))


;; Path of the feedsnake config directory
(define (feedsnake-directory)
  (create-directory (string-append (config-directory) "/feedsnake") #t))


;; Path of the feeds directory
(define (feeds-directory)
  (create-directory (string-append (feedsnake-directory) "/feeds") #t))


;; Lists all configured feeds (files in feed directory)
(define (feed-files)
  (map (lambda (relative-path)
		 (string-append (feeds-directory) "/" relative-path))
	   (directory (feeds-directory))))


;; Convert a date of arbitrary timezone to UTC
(define (date->utc-date date)
  (time-utc->date (date->time-utc date)))


;; The current date, with UTC (-0; Z) timezone
(define (current-date-utc)
  (date->utc-date (current-date)))

) ;; feedsnake-unix module



(define *retpoŝto*
  "Subject: ~{{~A||Unnamed post||title}}
From:~{{ ~A ||||from-name}}<~{{~A||feedsnake@localhost||from-address}}>
To:~{{ ~A ||You||to-name}}<~{{~A||you@localhost||to-address}}>
Date: ~{{~A||||updated}}

~{{~{~a~^, ~}~%~%***~%||||urls}}
~{{~A||||summary}}")
