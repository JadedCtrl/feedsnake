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


;; Module for misc. helper functions used by both feedsnake & feedsnake-unix
(module feedsnake-helpers
	(alist-car-ref date->utc-date current-date-utc)

(import scheme
		(chicken base)
		srfi-19)


;; Just car's the value of alist-ref (if it exists)
(define (alist-car-ref key alist)
  (let ([value (alist-ref key alist)])
	(if value
		(car value)
		#f)))


;; Convert a date of arbitrary timezone to UTC
(define (date->utc-date date)
  (time-utc->date (date->time-utc date)))


;; The current date, with UTC (-0; Z) timezone
(define (current-date-utc)
  (date->utc-date (current-date)))


) ;; feedsnake-helper module



;; The main feedsnake module; parses atom feeds into alists and strings
(module feedsnake
	(updated-feed-string read-feed filter-entries write-entry update-feed-file
	 write-entry-to-file write-entries-to-file all-entries entry->string
	 *maildir-template* *mbox-template*)

(import scheme
		(chicken base) (chicken condition) (chicken io) (chicken file)
		(chicken process-context)  (chicken pathname) (chicken port)
		srfi-1 srfi-13 srfi-19 srfi-69
		date-strings
		feedsnake-helpers
		http-client
		named-format
		xattr
		atom rss)


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
	   "Date: ~{{~A||||updated-rfc228||published-rfc228}}\n"
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

(define *default-template*
  (append *maildir-template*
		  '((output-dir "./"))))

(define *default-values*
  '((output-dir "./")))

(define *default-multifile-values*
  '((filename-template "~{{~A||||updated||published}}.~{{~A||you||USER}}@~{{~A||localhost|HOSTNAME}}")
	(multifile-output? #t)))

(define *default-singlefile-values*
  '((filename-template "feed.out")
	(multifile-output? #f)))


;; Read the given port into a feedsnake-feed (alist), no matter the format! c:<
(define (read-feed in-port)
  (let (;;[rss (rss:read in-port)]
		[atom (read-atom-feed in-port)])
	(if atom
		(atom-doc->feedsnake-feed atom)
		#f)))


 ;; Construct a filter function for feeds, given the script's arguments
(define (filter-entries feed filter)
  (let ([entry-date (lambda (entry) (car (alist-ref 'updated entry)))]
		[unfiltered (if feed
						(alist-car-ref 'entries feed)
						#f)]
		[entries '()])
	(if unfiltered
		(map
		 (lambda (entry)
		   (if (apply filter (list entry))
			   (set! entries
				 (append entries (list entry)))))
		 unfiltered))
	entries))


;; Returns either the updated string of a feed (in comparison to old string),
;; or #f if literally nothing's changed
(define (updated-feed-string url old-string)
  (let* ([new-string (fetch-http-string url)]
		 [updated? (not (string=? old-string new-string))])
	(if updated?
		new-string
		#f)))


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


;; Writes a given feed entry to the out-port, as per the feedsnake-unix-format template alist
(define (write-entry entry template-alist out-port)
  (write-string
   (entry->string (append (get-environment-variables) entry)
				  (alist-car-ref 'entry-template template-alist))
   #f
   out-port))


;; Write an entry to the given file (directory for multifile; normal file otherwise)
(define (write-entry-to-file entry template-alist out-path)
  (let* ([template (if (alist-car-ref 'multifile-output? template-alist)
					   (append template-alist *default-multifile-values* *default-values*)
					   (append template-alist *default-singlefile-values* *default-values*))]
		 [file-mode (if (alist-car-ref 'multifile-output? template) #:text #:append)])
	(call-with-output-file
		(entry-output-path entry template out-path)
	  (lambda (out-port)
		(write-entry entry template out-port))
	  file-mode)))


;; Writes all entries in a list to an out-path (mere convenience function)
(define (write-entries-to-file entries template-alist out-path)
  (map (lambda (entry)
		 (write-entry-to-file entry template-alist out-path))
	   entries))


;; Decides the correct output path for an entry, given the template's filename rules etc.
(define (entry-output-path entry template-alist base-out-path)
  (let ([multifile? (alist-car-ref 'multifile-output? template-alist)])
	(if multifile?
		(multifile-entry-path entry template-alist base-out-path)
		(singlefile-entry-path entry template-alist base-out-path))))


;; Output path for entry with a single-file template
(define (singlefile-entry-path entry template-alist base-out-path)
  (if (directory-exists? base-out-path)
		(signal
		 (make-property-condition
		  'exn 'location 'file
		  'message (string-append base-out-path " shouldn't be a directory.")))
		base-out-path))


;; Output path for an entry w multifile template
(define (multifile-entry-path entry template-alist base-out-path)
  (let* ([file-leaf (named-format (alist-car-ref 'filename-template template-alist) entry)])
	(if (create-directory base-out-path)
		(string-append base-out-path "/" file-leaf)
		(signal
		 (make-property-condition
		  'exn 'location 'file
		  'message (string-append base-out-path " either isn't accessible or isn't a directory."))))))


;; Switch the cached version of the feed with a newer version, if available.
;; If the feed-path doesn't exist, then the feed will be downloaded fresh.
(define (update-feed-file feed-path feed-url)
  (let* ([old-string (if (file-exists? feed-path)
						 (call-with-input-file feed-path
						   (lambda (in-port)
							 (read-string #f in-port)))
						 "")]
		 [new-string (updated-feed-string
					  feed-url
					  old-string)])
	(if new-string
		(call-with-output-file feed-path
		  (lambda (out-port)
			(write-string new-string #f out-port)))
		#f)))


;; List of all entries of the feed
(define (all-entries feed)
  (alist-car-ref 'entries feed))


;; Atom parsing
;; ————————————————————————————————————————

;; Parse an atom feed into a feedsnake-friendly alist
(define (atom-doc->feedsnake-feed atom)
  `((title ,(last (feed-title atom)))
	(url ,(atom-feed-preferred-url atom))
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
	  (urls ,(map (lambda (link) (atom-link->string link atom))
				  (entry-links entry)))
	  (authors ,(if (null? entry-authors) feed-authors entry-authors))
	  (feed-title ,(last (feed-title atom))))))


;; The preferred/given URL for an atom feed
(define (atom-feed-preferred-url atom)
  (car
   (filter
	  (lambda (link)
		(string=? (link-relation link) "self"))
	  (feed-links atom))))


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


;; Convert an atom-link into a proper, valid url
(define (atom-link->string link atom)
  (if (string-contains (link-uri link) "://")
	  (link-uri link)
	  (string-append (pathname-directory (atom-feed-preferred-url atom))
					 "/"
					 (link-uri link))))


;; Misc. functions
;; ————————————————————————————————————————
;; Download a file over HTTP to the given port.
(define (fetch-http url out-port)
  (call-with-input-request
   url #f
   (lambda (in-port) (copy-port in-port out-port))))


;; Download a feed (AKA fetch over HTTP to a string)
(define (fetch-http-string url)
  (call-with-output-string
   (lambda (out) (fetch-http url out))))


) ;; feedsnake module



;; The UNIX-style frontend for feedsnake
(module feedsnake-unix
	(main main)

(import scheme
		(chicken base) (chicken condition) (chicken file) (chicken file posix)
		(chicken io) (chicken port) (chicken process-context)
		(chicken process-context posix)
		srfi-1 srfi-19
		date-strings
		feedsnake feedsnake-helpers
		getopt-long
		uri-common
		xattr)


(define *help-msg*
  (string-append
   "usage: feedsnake [-hnuU] [-s|S] [-o|d] FILE...\n"
   "       feedsnake [-hn] [-c] [-s] [-o|d] URL...\n"
   "       feedsnake [-h] [-s] [-o|d]\n\n"
   "Feedsnake is a program for converting Atom feeds into mbox/maildir files.\n"
   "Any Atom feeds passed as input will be output in mbox or maildir format.\n\n"
   "If a FILE value is '-' or not provided, feedsnake will read a feed over standard\n"
   "input. --since-last and similar arguments have no impact on these feeds.\n\n"
   "If you want to subscribe to feeds with Feedsnake, you'll probably do something\n"
   "like so:\n"
   "       feedsnake --cache ~/feeds/hacker_news.xml \\\n"
   "                 --output ~/feeds/hacker_news.mbox \\\n"
   "                 https://news.ycombinator.com/rss\n\n"
   "Then, to update your subscription, just run:\n"
   "       feedsnake --update --since-last \\\n"
   "                 --output ~/feeds/hacker_news.mbox \\\n"
   "                 ~/feeds/hacker_news.xml\n\n"
   "For updating all feeds:\n"
   "       feedsnake --update --since-last ~/feeds/*.xml > ~/feeds/all.mbox\n\n"
   "The FILE given as input can be any Atom/RSS file. If you'd like to update\n"
   "the FILE (with --update or --update-since), then it must have the\n"
   "'user.xdg.origin.url' extended attribute set as the feed URL. You can create\n"
   "such a file as in the above example, by passing a URL with a --cache file set.\n\n"))


(define *opts*
  '((help
	 "Print a usage message"
	 (single-char #\h))
	(outdir
	 "Output directory, used for maildir output"
	 (single-char #\d)
	 (value (required DIR)))
	(output
	 "Output file, used for mbox output. Default is stdout ('-')."
	 (single-char #\o)
	 (value (required FILE)))
	(cache
	 "The cache file used if a URL is passed as argument."
	 (single-char #\c)
	 (value (required FILE)))
	(update
	 "Update a feed FILE by downloading its newest version to the same path."
	 (single-char #\u))
	(update-since
	 "Alias for --update and --since-last. This is probably the option you want."
	 (single-char #\U))
	(since
	 "Output entries after the given date, in YYYY-MM-DD hh:mm:ss format."
	 (single-char #\s)
	 (value (required DATETIME)))
	(since-last
	 "Output entries dating from the last saved parsing of the file."
	 (single-char #\S))
	(since-update
	 "Output entries dating from the last update of the file.")
	(no-save-date
	 "Don't save parse/update time of this operation, to avoid influencing --since-*."
	 (single-char #\n))))


;; Prints cli usage to stderr.
(define (help)
  (write-string *help-msg* #f (open-output-file* fileno/stderr))
  (write-string (usage *opts*) #f (open-output-file* fileno/stderr)))


;; Wrap around the main function, so that the user isn't scared off by exceptions
(define-syntax exception-condom
  (syntax-rules ()
	((exception-condom expr)
	 (handle-exceptions exn
		 (begin
		   (write-string
			(string-append (get-condition-property exn 'exn 'message)
						   " ("
;;						   (symbol->string (get-condition-property exn 'exn 'location))
						   ")\n")
			#f (open-output-file* fileno/stderr))
		   (exit 2))
	   expr))))


  ;; Just ignore whatever exceptions the expression throws our way
(define-syntax ignore-errors
  (syntax-rules ()
	((ignore-errors expr)
	 (handle-exceptions exn #f expr))))


;; The `main` procedure that should be called to run feedsnake-unix for use as script.
(define (main)
  (exception-condom
   (let* ([args (getopt-long (command-line-arguments) *opts*)]
		  [free-args (alist-ref '@ args)])
	 (if (alist-ref 'help args)
		 (help)
		 (map (lambda (feed-pair)
				(process-feed args feed-pair))
			  (get-feeds free-args args))))))


;; Turn the scripts free-args into parsed Feedsnake feed alists
(define (get-feeds free-args args)
  (let ([feed-paths
		 (if (eq? (length free-args) 0)
			 '("-")
			 free-args)])
	(map (lambda (path) (get-feed path args))
		 feed-paths)))


;; Turn a given feed-path (free-arg) into a parsed Feedsnake feed, if possible
(define (get-feed feed-path args)
  (let*
	  ([uri (ignore-errors (absolute-uri feed-path))]
	   [out-path (cond
				   [(and uri (alist-ref 'cache args))
					(alist-ref 'cache args)]
				   [uri "-"]
				   [#t  feed-path])]
	   [feed
		(cond
		 [(string=? feed-path "-")
		  (call-with-input-string (read-string)
								  read-feed)]
		 [(and uri (not (string=? "-" out-path)))
		  (begin
			(update-feed-file out-path (uri->string uri))
			(ignore-errors (call-with-input-file out-path read-feed)))]
		 [uri
		  (call-with-input-string (updated-feed-string (uri->string uri) "")
								  read-feed)]
		 [#t
		  (ignore-errors (call-with-input-file out-path read-feed))])])

	;; Set the origin URL, if newly-created cache file
	(if (and uri (not (string=? "-" out-path)))
		(set-xattr out-path "user.xdg.origin.url" (uri->string uri)))

	(list out-path feed)))


;; Process a parsed feed, given arguments passed to the script
(define (process-feed args feed-pair)
  (let* ([feed (last feed-pair)]
		 [feed-path (first feed-pair)]
		 [update? (or (alist-ref 'update args) (alist-ref 'update-since args))])

	;; Update the feed
	(if update?
		(begin
		  (update-feed-file feed-path
							(get-xattr feed-path "user.xdg.origin.url"))
		  (set! feed (call-with-input-file feed-path read-feed))))

	;; Print all entries to stdout
	(output-entries args `(,feed-path ,feed))

	;; Change file's update-date
	(if (and update?
			 (not (alist-ref 'no-save-date args)))
		(set-xattr feed-path "user.feedsnake.updated"
				   (date->rfc339-string (current-date-utc))))

	;; Save the file's parsing date
	(if (and (file-exists? feed-path)
			 (not (alist-ref 'no-save-date args)))
		(set-xattr feed-path "user.feedsnake.parsed"
				   (date->rfc339-string (current-date-utc))))))


;; Output the appropriate entrise of the given feed, using script's args
(define (output-entries args feed-pair)
  (let* ([feed (last feed-pair)]
		 [output-dir (alist-ref 'outdir args)]
		 [output (or (alist-ref 'output args) output-dir)]
		 [template (if output-dir *maildir-template* *mbox-template*)]
		 [filter (entry-filter feed-pair args)]
		 [entries (filter-entries feed filter)])
	(cond
	 [(not entries)
		  #f]
	 [output
	  (write-entries-to-file entries template output)]
	 [(not output)
	  (map (lambda (entry)
			 (write-entry entry template
							 (open-output-file* fileno/stdout)))
			  entries)])))


;; Construct a filter function for feeds, given the script's arguments
(define (entry-filter feed-pair args)
  (let* ([since-string (alist-ref 'since args)]
		 [since (if since-string
					(date->utc-date (string->date since-string "~Y-~m-~d ~H:~M:~S"))
					#f)]
		 [entry-date (lambda (entry)
					   (or (alist-car-ref 'updated entry)
						   (alist-car-ref 'published entry)))]
		 [last-parse-string (or (ignore-errors
						   (get-xattr (first feed-pair) "user.feedsnake.parsed"))
						  "1971-01-01T00:00:00Z")]
		 [last-parse (rfc339-string->date last-parse-string)]
		 [last-update-string (or (ignore-errors
								 (get-xattr (first feed-pair) "user.feedsnake.updated"))
								"1971-01-01T00:00:00Z")]
		 [last-update (rfc339-string->date last-update-string)])
	(lambda (entry)
	  (cond [since
			 (date>=? (entry-date entry) since)]
			[(or (alist-ref 'since-last args) (alist-ref 'update-since args))
			 (date>=? (entry-date entry) (or last-parse last-update))]
			[(alist-ref 'since-update args)
			 (date>=? (entry-date entry) last-update)]
			[#t
			 #t]))))


;; Supposed config root of the user (as per XDG, or simple ~/.config)
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


) ;; feedsnake-unix module
