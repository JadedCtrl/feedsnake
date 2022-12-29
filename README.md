# Feedsnake
Simple Atom-to-mail (Mbox, Maildir) program: You pass it an Atom feed, it'll turn it into an mail client-friendly set of e-mails. It's pretty much [feed2maildir](https://github.com/sulami/feed2maildir) combined with [feed2mail](https://github.com/vmapps/feed2mail), but less cool. It's also pretty much my attempt replacing an old project, [Pogger](https://github.com/jadedctrl/pogger), but for LiGNUx.

Feedsnake is scriptable, and can be easily used for one-off conversions or for managing your feed subscriptions generally. It pairs well with something like [Mutt](https://duckduckgo.com/l/?uddg=http%3A%2F%2Fwww.mutt.org&rut=bdcef874f2615434d459413d18463f18c41b3dbc415ccba8bdb35b350ac16340) to read your feeds.


## Usage
```
usage: feedsnake [-hnuU] [-s|S] [-o|d] FILE...
       feedsnake [-hn] [-c] [-s] [-o|d] URL...
       feedsnake [-h] [-s] [-o|d]

Feedsnake is a program for converting Atom feeds into mbox/maildir files.
Any Atom feeds passed as input will be output in mbox or maildir format.

If a FILE value is '-' or not provided, feedsnake will read a feed over standard
input. --since-last and similar arguments have no impact on these feeds.

If you want to subscribe to feeds with Feedsnake, you'll probably do something
like so:
       feedsnake --cache ~/feeds/hacker_news.xml \
                 --output ~/feeds/hacker_news.mbox \
                 https://news.ycombinator.com/rss

Then, to update your subscription, just run:
       feedsnake --update --since-last \
                 --output ~/feeds/hacker_news.mbox \
                 ~/feeds/hacker_news.xml

For updating all feeds:
       feedsnake --update --since-last ~/feeds/*.xml > ~/feeds/all.mbox

The FILE given as input can be any Atom/RSS file. If you'd like to update
the FILE (with --update or --update-since), then it must have the
'user.xdg.origin.url' extended attribute set as the feed URL. You can create
such a file as in the above example, by passing a URL with a --cache file set.

 -h, --help               Print a usage message
 -d, --outdir=DIR         Output directory, used for maildir output
 -o, --output=FILE        Output file, used for mbox output. Default is stdout ('-').
 -c, --cache=FILE         The cache file used if a URL is passed as argument.
 -u, --update             Update a feed FILE by downloading its newest version to the same path.
 -U, --update-since       Alias for --update and --since-last. This is probably the option you want.
 -s, --since=DATETIME     Output entries after the given date, in YYYY-MM-DD hh:mm:ss format.
 -S, --since-last         Output entries dating from the last saved parsing of the file.
 --since-update           Output entries dating from the last update of the file.
 -n, --no-save-date       Don't save parse/update time of this operation, to avoid influencing --since-*.
```


## Installation
Feedsnake is made using [Chicken Scheme](https://call-cc.org), a cute little Scheme that's very friendly. Once you've got Chicken Scheme installed, you can go ahead and build Feedsnake like so:
```
$ sudo make dependencies
$ make client
$ sudo cp ./feedsnake /usr/local/bin/
```

… actually, that's somewhat of a lie. You need to manually install the [xattr](https://github.com/jadedctrl/xattr) library first, since it's not in the egg depot.


## Examples
### Feeds, generally
To subscribe to a feed, you're really just creating a cache file for the feed. It's simple. Here:

`$ feedsnake --no-save-date --cache ~/Feeds/Hacker\ News.xml https://news.ycombinator.com/rss`

Then, just update the file and output new posts from your feed(s), every once in a while:
```
$ feedsnake --since-last --update --output=~/Feeds/Mail.mbox ~/Feeds/*.xml
$ mutt -f ~/Feeds/Mail.mbox

# Or, alternatively, for maildir:
$ feedsnake --since-last --update --outdir=~/Feeds/Mail/ ~/Feeds/*.xml
$ mutt -f ~/Feeds/Mail/
```


### YouTube
I like to "subscribe" to channels by having an following its Atom feed on Invidious— it's also nice to just download those videos in one go.

Let's subscribe to a channel:
```
# With Feedsnake:
$ feedsnake -nc ~/Feeds/YouTube/Linsday\ Ellis.xml \
    "https://yewtu.be/feed/channel/UCG1h-Wqjtwz7uUANw6gazRw"
# Manually:
$ touch ~/Feeds/YouTube/Stop\ Skeletons\ From\ Fighting.xml
$ attr -s xdg.origin.url -V "https://yewtu.be/feed/channel/UC5Xeb9-FhZXgvw340n7PsCQ" \
       ~/Feeds/YouTube/Stop\ Skeletons\ From\ Fighting.xml
```

And now, lets downloading all videos uploaded since the last update:

`$ ytdl $(feedsnake -U ~/Feeds/YouTube/*.xml | grep '^\*\*\*' | sed 's%^... %%')`

(All URLs associated with a post are listed in the message body in a line starting with three asterisks; hence the search & removal of said asterisks.)

## Meta
* **Author:** Jaidyn Ann (jadedctrl@posteo.at)
* **License:** GPLv3
* https://github.com/jadedctrl/feedsnake
* https://notabug.org/jadedctrl/feedsnake
* https://xwx.moe/en/
