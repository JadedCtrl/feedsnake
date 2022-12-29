clean:
	rm combined.scm feedsnake

client:
	cat named-format.scm date-strings.scm feedsnake.scm client.scm > combined.scm
	csc -o feedsnake combined.scm

dependencies:
	chicken-install srfi-1 srfi-13 srfi-19 srfi-69 atom getopt-long http-client rss xattr

all: client

