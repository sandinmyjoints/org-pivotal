[![Build Status](https://travis-ci.org/org-pivotal/org-pivotal.png?branch=master)](https://travis-ci.org/org-pivotal/org-pivotal)
# org-pivotal

## Summary

org-pivotal is an emacs minor mode to extend org-mode with Pivotal Tracker abilities

## Installing

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

    cd org-pivotal
    make && make install

## Manual Installation

```
git clone git@github.com:org-pivotal/org-pivotal.git ~/.emacs.d/private/local

# add to additional-packages
dotspacemacs-additional-packages
'(
  ...
  helm
  (org-pivotal :location local))

# Usage

;; Pivoltracker - set under user-config
(require 'org-pivotal)
(setq org-pivotal-api-token (getenv "PIVOTAL_API_TOKEN"))
(evil-leader/set-key-for-mode 'org-mode "pu" 'org-pivotal-push-story)
(evil-leader/set-key-for-mode 'org-mode "pp" 'org-pivotal-pull-stories)
(evil-leader/set-key-for-mode 'org-mode "pi" 'org-pivotal-install-project-metadata)

```

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2018 Huy Duong.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
