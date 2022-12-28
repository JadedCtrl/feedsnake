clean:
	rm combined.scm feedsnake

client:
	cat named-format.scm date-strings.scm feedsnake.scm client.scm > combined.scm
	csc -o feedsnake combined.scm

all: client

