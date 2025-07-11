# NinetyFive for Emacs

To automatically start NinetyFive after Emacs loads, add this to your config file (`.emacs`)
```el
(add-hook 'emacs-startup-hook #'ninetyfive-start)
```

## Development
1. [Install MELPA](https://melpa.org/#/getting-started)
1. Copy `ninetyfive.el` to `~/.emacs.d/`.
2. Set up your emacs to load and start NinetyFive.

```el
;; Install ninetyfive
(add-to-list 'load-path "~/.emacs.d/")
(load "ninetyfive.el")

;; Start ninetyfive
(add-hook 'emacs-startup-hook #'ninetyfive-start)
```