# NinetyFive for Emacs

To automatically start NinetyFive after Emacs loads, add this to your config file (`.emacs`)
```el
(add-hook 'emacs-startup-hook #'ninetyfive-start)
```