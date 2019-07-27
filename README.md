## Readline example in Common Lisp

Example from cl-readline's documentation: https://vindarel.github.io/cl-readline/

It showcases custom completion and keybindings. Type a command (with TAB-completion), and type arguments (with TAB-completion of the choices).

To run the example:

    sbcl --script example.lisp

or build an executable:

    make build  # and then ./clreadline

(we added handling of a `C-c`).

See the `custom completion`: first, type and complete a verb (`eat`, `throw`,…). Later words are completed as fruits (`orange`, `banana`,…). Type "o<TAB>" and see the completion to "orange" (no dropdown list).

![](readline.png)

Also type `C-o` to insert custom text.



Some command line apps using cl-readline:

* [cl-repl](https://github.com/koji-kojiro/cl-repl) - an SBCL repl.
* [cl-torrents](https://github.com/vindarel/cl-torrents) - searching and downloading torrents from popular trackers.
* [replic](https://github.com/vindarel/replic/) - a library to quickly build a readline app from a lisp library.
* [shtookovina](https://github.com/mrkkrp/shtookovina/) (unmaintained)


## Changelog

We added:

* july, 2019: rewrote without curry, removed the dependency on Alexandria, added a dependency on cl-str.
* asdf system declaration
* `make build` to build a binary
* catch a `C-c` (and others) and quit gracefully (in executable only).
