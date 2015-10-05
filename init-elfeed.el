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
 '(;; Comics
   ("http://campcomic.com/rss" comics link)
   ("http://cdn.drivecomic.com/rss.xml" comics)
   ("http://cdn.sheldoncomics.com/rss.xml" comics)
   ("http://cthulhuslippers.com/feed/" comics)
   ("http://egscomics.com/rss.php" comics link)
   ("http://emacsredux.com/atom.xml" comics)
   ("http://erfworld.com/erf_stream/view?view=rss" comics)
   ("http://feeds.feedburner.com/NotInventedHere" comics link)
   ("http://feeds.feedburner.com/ScenesFromAMultiverse" comics)
   ("http://feeds.feedburner.com/abominable" comics)
   ("http://feeds.feedburner.com/hijinksensue?format=xml" comics)
   ("http://feeds.feedburner.com/rsspect/fJur" comics)
   ("http://feeds.feedburner.com/uclick/basicinstructions" comics link)
   ("http://feeds.feedburner.com/uclick/foxtrot" comics link)
   ("http://feeds.penny-arcade.com/pa-mainsite/" comics link)
   ("http://flakypastry.runningwithpencils.com/flakypastryrss.xml" comics link)
   ("http://lastmechanicalmonster.blogspot.com/feeds/posts/default" comics)
   ("http://over-encumbered.com/rss" comics)
   ("http://plox-comic.com/feed/" comics)
   ("http://ps238.nodwick.com/?feed=rss2" comics)
   ("http://pvponline.com/feed" comics link)
   ("http://samandfuzzy.com/rss.php" comics link)
   ("http://skin-horse.com/feed/" comics link)
   ("http://tabletitans.com/feed" comics link)
   ("http://www.alicegrove.com/rss" comics)
   ("http://www.baldwinpage.com/annagalactic/feed/" comics)
   ("http://www.bugmartini.com/feed/" comics)
   ("http://www.cad-comic.com/rss/rss.xml" comics link)
   ("http://www.dumbingofage.com/feed/" comics link)
   ("http://www.giantitp.com/comics/oots.rss" comics)
   ("http://www.girlgeniusonline.com/ggmain.rss" comics link)
   ("http://www.kiwiblitz.com/rss.php" comics link)
   ("http://www.questionablecontent.net/QCRSS.xml" comics link)
   ("http://www.smbc-comics.com/rss.php" comics)
   ("http://www.somethingpositive.net/sp.xml" comics link)
   ("http://www.useswordonmonster.com/?feed=rss2" comics link)
   ("http://xkcd.com/atom.xml" comics)
   ("http://youdamnkid.com/?feed=rss2" comics link)
   ("http://frivolesque.com/feed" comics link)
   ;; Math
   ("http://blog.plover.com/index.rss" math)
   ("http://golem.ph.utexas.edu/category/atom10.xml" math)
   ("http://gottwurfelt.wordpress.com/feed/" math)
   ("http://gowers.wordpress.com/feed/" math)
   ("http://igorpak.wordpress.com/feed/" math)
   ("http://j2kun.svbtle.com/feed" math)
   ("http://jeremykun.wordpress.com/feed/" math)
   ("http://quomodocumque.wordpress.com/feed/" math)
   ("http://rigtriv.wordpress.com/feed/" math)
   ("http://terrytao.wordpress.com/feed/" math)
   ("http://www.ams.org/featurecolumn.rss" math link)
   ("http://www.goodmath.org/blog/?feed=rss2" math)
   ("http://www.scottaaronson.com/blog/?feed=rss2" math)
   ;; Programming
   ("http://250bpm.com/feed/pages/pagename/start/category/blog/t/250bpm-blogs/h/"
    programming)
   ("http://amitp.blogspot.com/feeds/posts/default" programming)
   ("http://blog.8thlight.com/feed/atom.xml" programming)
   ("http://blog.cleancoder.com/atom.xml" programming)
   ("http://feeds.feedburner.com/Rik0TechTemple" programming)
   ("http://feeds.feedburner.com/StuartSierra" programming)
   ("http://feeds.feedburner.com/davesquared" programming)
   ("http://kotka.de/blog/index.rss" programming)
   ("http://oremacs.com/atom.xml" programming)
   ("http://pragmaticemacs.com/feed/" programming)
   ("http://programming-puzzler.blogspot.com/feeds/posts/default" programming)
   ("http://simblob.blogspot.com/feeds/posts/default" programming)
   ("http://swannodette.github.io/atom.xml" programming)
   ("https://aphyr.com/posts.atom" programming)
   ("https://yoo2080.wordpress.com/feed/" programming)
   ;; Stackexchange
   ("http://stackoverflow.com/feeds/tag/clojure" programming stackexchange)
   ("http://stackoverflow.com/feeds/tag/emacs" programming stackexchange)
   ("http://superuser.com/feeds/tag/emacs" programming stackexchange)
   ("http://superuser.com/feeds/tag/autohotkey" programming stackexchange)
   ("http://emacs.stackexchange.com/feeds" programming stackexchange)
   ;; Other
   ("http://alasdairstuart.com/?feed=rss2" blog)
   ("http://allkindsofhistory.wordpress.com/feed/" history)
   "http://feeds.feedburner.com/podiobooks?format=xml"
   ("http://mathbabe.org/feed/" blog)
   ("http://thealexandrian.net/feed" rpg blog)
   ("http://what-if.xkcd.com/feed.atom" comics blog)
   ("http://www.weeklystorybook.com/comic_strip_of_the_daycom/atom.xml"
    comics blog link)
   ("http://www.zachtronicsindustries.com/feed/" games blog)))

(provide 'init-elfeed)
