# NinetyFive for Emacs

To automatically start NinetyFive after Emacs loads, add this to your config file (`.emacs`)
```el
(add-hook 'emacs-startup-hook #'ninetyfive-start)
```

## Configuration usage

Ensure NinetyFive is installed to your `~/.emacs.d/*`

Add NinetyFive to your ~/.emacs file like below:

```emacs
(use-package ninetyfive
  :load-path "~/.emacs.d/lisp/"
  :init
  (setq ninetyfive-indexing-mode "yes")
  (setq ninetyfive-cache-consent t)
  :config
  (add-hook 'emacs-startup-hook #'ninetyfive-start))
```

The above configuration allows NinetyFive to index your code for better completions. NinetyFive will cache this consent globally (at `~/.ninetyfive/consent.json`) for all extensions. You can opt out of this by setting the cache consent to nil.

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