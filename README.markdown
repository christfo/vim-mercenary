mercenary.vim
============

I'm not going to lie to you; mercenary.vim may very well be the worst
Mercurial wrapper of all time.

Commands
--------

There are two types of commands presented here. Ones that offer a view of history, and
ones that offer detail from a given revision. The former will launch into a new split,
and the later will try and find a suitable window to show themselves. 

### History Views
* HGlog
* HGblame
* HGgrep [ searchregex==<cword> ]
* Hgqueue

### Revision Detail
* HGshow
* HGcat
* HGdiff [ revision == 'p1()' ]

In any mercenary window, there is a local mapping 'q' to quit out of the view, and 
where there is some kind of changeset identifier on the current line, there are 
mappings 's', 'c', and 'd' for 'show', 'cat' and 'diff' respectively.

Show and Tell
-------------

Here are some screenshots showing off some the mercenary's dirty work:

`:HGblame` will annotate the current file with the blame.

![:HGblame](http://i.imgur.com/O7WUC.png)

`:HGdiff {rev}` will diff the current file against the specified revision.

![:HGdiff](http://i.imgur.com/KRava.png)

`:HGshow {rev}` will show the commit message and diff for the specified 
revision.

![:HGshow](http://i.imgur.com/x2RzL.png)


`:HGcat {rev} {path}` will show the specified file at the specified revision.

![:HGcat](http://i.imgur.com/g8OpJ.png)

Installation
------------

If you don't have a preferred installation method, I recommend
installing [vundle](https://github.com/gmarik/vundle) and adding

    Bundle 'phleet/vim-mercenary'

to your `.vimrc` then running `:BundleInstall`.

If you prefer [pathogen.vim](https://github.com/tpope/vim-pathogen), after 
installing it, simply copy and paste:

    cd ~/.vim/bundle
    git clone git://github.com/christfo/vim-mercenary.git

Once help tags have been generated, you can view the manual with
`:help mercenary`.

License
-------
Mercenary is (c) Jamie Wong.

Distributed under the same terms as Vim itself.  See `:help license`.

Heavily inspired by vim-fugitive by Tim Pope: 
https://github.com/tpope/vim-fugitive

This started out as a fork of fugitive, but I eventually discovered that the 
differences between git and mercurial while minor in functionality are vast in 
implementation. So I started from scratch, using fugitive's code as a reference 
but re-implementing everything to be less of a monkey patch for mercurial.
