Dotfiles managed by `stow`.

To create the symlinks:

```
cd ~/.dotfiles
stow .
```

To remove the symlinks:

```
cd ~/.dotfiles
stow -D .
```


Run emacs by:
```
~/bin/em-mac-29-2 -init-directory ~/.config/emacs-profiles/29.2
```


~/bin/em-mac-29-2:
```
#!/bin/bash
exec ~/develop-emacs/emacs-mac/mac/Emacs.app/Contents/MacOS/Emacs "$@"
```
