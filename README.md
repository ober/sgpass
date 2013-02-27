sgpass
======

SuperGenPass for Emacs

Elisp implementation of SuperGenPass


For additional details see:

http://supergenpass.com/


To Use:

(require 'supergenpass)
(supergenpass master-secret-password "google.com")

This will populate the kill-ring so that the password can be pasted.

TODO:
Remove from kill-ring after paste.
Cleanup this no doubt ugly elisp