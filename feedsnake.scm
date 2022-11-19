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

;; Misc helper functions used in both feedsnake and feedsnake-unix
(module feedsnake-helpers
	(alist-car-ref)

(import scheme
		(chicken base))

;; Just car's the value of alist-ref (if it exists)
(define (alist-car-ref key alist)
  (let ([value (alist-ref key alist)])
	(if value
		(car value)
		#f))))


;; The main feedsnake module; parses atom feeds into alists and strings
(module feedsnake
	(updated-feed-string read-feed entries-since entry->string)

(import scheme
		(chicken base) (chicken condition) (chicken io) (chicken port)
		srfi-1 srfi-19 srfi-69
		date-strings
		feedsnake-helpers
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
   (append entry
		   (entry-templating-parameters entry template))))


;; Returns an alist of string replacements/parameters for a given entry
;; For use with named-format
(define (entry-templating-parameters entry template)
  (append
   entry
   (entry-url-templating-parameters entry)
   (entry-author-templating-parameters entry)
   (entry-date-templating-parameters entry)))


;; URL-related named-format templating parameters for given entry
(define (entry-url-templating-parameters entry)
  (let ([urls (alist-car-ref 'urls entry)])
	`((url ,(cond
			 [(list? urls) (car urls)]
			 [(string? urls) urls])))))


;; Author-related named-format templating parameters for given entry
(define (entry-author-templating-parameters entry)
  (let* ([authors (alist-car-ref 'authors entry)]
		 [author (if authors (car authors) (alist-car-ref 'feed-title entry))])
	`((author ,author))))


;; Date-related named-format templating parameters for given entry
(define (entry-date-templating-parameters entry)
  (let* ([updated (or (alist-car-ref 'updated entry) (alist-car-ref 'published entry))]
		 [published (or (alist-car-ref 'published entry) updated)])
	`((updated-rfc228 ,(if updated (date->rfc228-string updated)))
	  (published-rfc228 ,(if published (date->rfc228-string published)))
	  (updated-mbox ,(if updated (date->mbox-string updated)))
	  (published-mbox ,(if published (date->mbox-string published))))))


;; Parse an atom feed into a feedsnake-friendly alist
(define (atom-doc->feedsnake-feed atom)
  `((title ,(last (feed-title atom)))
	(urls ,(feed-links atom))
	(authors ,(map author-name (feed-authors atom)))
	(updated ,(feed-updated atom))
	(entry-updated ,(atom-feed-latest-entry-date atom))
	(entries ,(map
			   (lambda (entry)
				 (atom-entry->feedsnake-entry entry atom))
			   (feed-entries atom)))))


;; Parse an atom entry into a feedsnake entry :>
(define (atom-entry->feedsnake-entry entry atom)
  (let ([published (rfc339-string->date (entry-published entry))]
		[updated (rfc339-string->date (entry-updated entry))]
		[feed-authors (map author-name (feed-authors atom))]
		[entry-authors (map author-name (entry-authors entry))])
	`((title ,(last (entry-title entry)))
	  (updated ,(or updated published))
	  (published ,(or published updated))
	  (summary ,(last (entry-summary entry)))
	  (urls ,(map link-uri (entry-links entry)))
	  (authors ,(if (null? entry-authors) feed-authors entry-authors))
	  (feed-title ,(feed-title atom)))))


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
	(update-feed-file latest-entries all-entries write-entry write-entries feed-files *mbox-template*)

(import scheme
		(chicken base) (chicken condition) (chicken file) (chicken io)
		(chicken process-context) (chicken process-context posix)
		srfi-1 srfi-19
		date-strings
		feedsnake feedsnake-helpers
		xattr)


(define *maildir-template*
  `((entry-template
	 ,(string-append
	   "From: ~{{~A ||||from-name}}"
	   "<~{{~A||feedsnake||FROM_USER||author-user||feed-title}}"
	   "@"
	   "~{{~A||localhost||FROM_HOST||author-domain||feed-domain}}>"
	   "\n"
 	   "To:~{{ ~A ||You||TO_NAME||USER}}"
	   "<~{{~A||you||TO_USER||USER}}"
	   "@"
	   "~{{~A||localhost||TO_HOST||HOSTNAME}}>"
	   "\n"
	   "Subject: ~{{~A||Unnamed post||title}}\n"
	   "Date: ~{{~A||||updated||published}}\n"
	   "\n"
	   "~{{~{~a~^, ~}~%***~%||||urls}}\n"
	   "~{{~A||||summary}}\n"))
	(multifile-output? #t)))


(define *mbox-template*
  `((entry-template ,(string-append
					  "From FEEDSNAKE ~{{~A||||updated-mbox||published-mbox}}\n"
					  (car (alist-ref 'entry-template *maildir-template*))
					  "\n"))
	(multifile-output? #f)))


(define *html-template*
  `((entry-template
	 "<li><b>~{{~A||Unnamed post||title}}</b> <i>~{{~A||||updated}}<i> <p>~{{~A||No summary||summary}}</p></li>")
	(multifile-output? #f)
	(output-header "<!DOCTYPE html>\n<html>\n<head>\n<title>~{{~A||Unnamed feed||title}}</title>\n</head>\n<body>")
	(output-footer "</body></html>")
	(multifile-output? #f)))


(define *default-template*
  (append *maildir-template*
		  '((output-dir "./"))))

(define *default-values*
  '((output-dir "./")))

(define *default-multifile-values*
  '((filename-template "~{{~A||||updated||published}}.~{{~A||you||USER}}@~{{~A||localhost|HOSTNAME}}.~{{~A||||title||title}}")))

(define *default-singlefile-values*
  '())


;; Writes a given feed entry to the out-path, as per the feedsnake-unix-format template alist
(define (write-entry entry template-alist out-path)
  (let ([file-mode (if (alist-car-ref 'multifile-output? template-alist) #:text #:append)]
		[header (or (alist-car-ref 'output-header template-alist) "")]
		[footer (or (alist-car-ref 'output-footer template-alist) "")]
		[entry-w-env-vars (append (get-environment-variables) entry)])
	(call-with-output-file
		out-path
	  (lambda (out-port)
		(write-string
		 (string-append header
						(entry->string entry-w-env-vars (alist-car-ref 'entry-template template-alist))
						footer)
		 #f
		 out-port))
	  file-mode)))


;; Writes all entries in a list to an out-path (mere convenience function)
(define (write-entries entries template-alist out-path)
  (map (lambda (entry)
		 (write-entry entry template-alist out-path))
	   entries))


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

