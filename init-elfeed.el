;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; ELFEED CONFIGURATION
;;;;============================================================================


;;; Update settings
;;;================
(setq url-queue-parallel-processes 1)
(setq url-queue-timeout 30)


;;; Storage settings
;;;=================
(setq elfeed-db-directory "~/sync/elfeed")


;;; Feed settings
;;;==============
(setq
 elfeed-feeds
 '("http://250bpm.com/feed/pages/pagename/start/category/blog/t/250bpm-blogs/h/"
   "http://alasdairstuart.com/?feed=rss2"
   "http://allkindsofhistory.wordpress.com/feed/" 
   "http://amitp.blogspot.com/feeds/posts/default"
   "http://blog.8thlight.com/feed/atom.xml"
   "http://blog.plover.com/index.rss"
   "http://campcomic.com/rss"
   "http://cdn.drivecomic.com/rss.xml"
   "http://cdn.sheldoncomics.com/rss.xml"
   "http://cthulhuslippers.com/feed/"
   "http://egscomics.com/rss.php" 
   "http://emacsredux.com/atom.xml"
   "http://erfworld.com/erf_stream/view?view=rss"
   "http://feeds.feedburner.com/NotInventedHere"
   "http://feeds.feedburner.com/Rik0TechTemple"
   "http://feeds.feedburner.com/ScenesFromAMultiverse"
   "http://feeds.feedburner.com/StuartSierra"
   "http://feeds.feedburner.com/abominable"
   "http://feeds.feedburner.com/davesquared"
   "http://feeds.feedburner.com/hijinksensue?format=xml"
   "http://feeds.feedburner.com/podiobooks?format=xml"
   "http://feeds.feedburner.com/rsspect/fJur"
   "http://feeds.feedburner.com/uclick/basicinstructions"
   "http://feeds.feedburner.com/uclick/foxtrot"
   "http://feeds.penny-arcade.com/pa-mainsite/" 
   "http://flakypastry.runningwithpencils.com/flakypastryrss.xml"
   "http://golem.ph.utexas.edu/category/atom10.xml"
   "http://gottwurfelt.wordpress.com/feed/"
   "http://gowers.wordpress.com/feed/"
   "http://igorpak.wordpress.com/feed/"
   "http://j2kun.svbtle.com/feed"
   "http://jeremykun.wordpress.com/feed/"
   "http://kotka.de/blog/index.rss"
   "http://lastmechanicalmonster.blogspot.com/feeds/posts/default"
   "http://mathbabe.org/feed/"
   "http://oremacs.com/atom.xml"
   "http://over-encumbered.com/rss"
   "http://plox-comic.com/feed/" 
   "http://pragmaticemacs.com/feed/"
   "http://programming-puzzler.blogspot.com/feeds/posts/default"
   "http://ps238.nodwick.com/?feed=rss2"
   "http://pvponline.com/feed"
   "http://quomodocumque.wordpress.com/feed/"
   "http://rigtriv.wordpress.com/feed/"
   "http://ronininstitute.org/feed/"
   "http://samandfuzzy.com/rss.php"
   "http://simblob.blogspot.com/feeds/posts/default"
   "http://skin-horse.com/feed/"
   "http://stackoverflow.com/feeds/tag?tagnames=clojure&sort=newest"
   "http://stackoverflow.com/feeds/tag?tagnames=emacs&sort=newest"
   "http://swannodette.github.io/atom.xml"
   "http://tabletitans.com/feed"
   "http://terrytao.wordpress.com/feed/"
   "http://thealexandrian.net/feed"
   "http://what-if.xkcd.com/feed.atom"
   "http://www.alicegrove.com/rss"
   "http://www.ams.org/featurecolumn.rss" 
   "http://www.baldwinpage.com/annagalactic/feed/"
   "http://www.bugmartini.com/feed/"
   "http://www.cad-comic.com/rss/rss.xml"
   "http://www.dumbingofage.com/feed/"
   "http://www.giantitp.com/comics/oots.rss"
   "http://www.girlgeniusonline.com/ggmain.rss"
   "http://www.goodmath.org/blog/?feed=rss2"
   "http://www.kiwiblitz.com/rss.php"
   "http://www.questionablecontent.net/QCRSS.xml"
   "http://www.scottaaronson.com/blog/?feed=rss2"
   "http://www.smbc-comics.com/rss.php"
   "http://www.somethingpositive.net/sp.xml" 
   "http://www.useswordonmonster.com/?feed=rss2"
   "http://www.weeklystorybook.com/comic_strip_of_the_daycom/atom.xml"
   "http://www.zachtronicsindustries.com/feed/"
   "http://xkcd.com/atom.xml"
   "http://youdamnkid.com/?feed=rss2"
   "https://aphyr.com/posts.atom"
   "https://yoo2080.wordpress.com/feed/"
   "http://frivolesque.com/feed"))

(provide 'init-elfeed)
