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

Also type `C-o` to insert a predefined custom text.



Some command line apps using cl-readline:

* [cl-repl](https://github.com/koji-kojiro/cl-repl) - an SBCL repl.
* [cl-torrents](https://github.com/vindarel/cl-torrents) - searching and downloading torrents from popular trackers.
* [replic](https://github.com/vindarel/replic/) - a library to quickly build a readline app from a lisp library.
* [sbcli](https://github.com/hellerve/sbcli/) - a simple wrapper around the bare-bones SBCL REPL with completion of Lisp symbols, syntax highlighting, no interactive debugger by default.
* [cl-repl](https://github.com/koji-kojiro/cl-repl/) - an advanced terminal Lisp REPL for the terminal: interactive debugger, syntax highlighting, code editing with a text editor…
* [OpenBookStore](https://github.com/OpenBookStore/openbookstore) - a book management software, with web and readline interfaces.
* [shtookovina](https://github.com/mrkkrp/shtookovina/) - a language learning app (unmaintained).

Please add yours here and/or in [readline's wiki](https://github.com/vindarel/cl-readline/wiki)!


## Changelog

We added:

* Nov, 2022: handle a C-d (NIL input), output something to show using the text variable.
* july, 2019: rewrote without curry, removed the dependency on Alexandria, added a dependency on cl-str.
* asdf system declaration
* `make build` to build a binary
* catch a `C-c` (and others) and quit gracefully (in executable only).
