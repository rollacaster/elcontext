# elcontext

> Bring your editing experience to the next level with context-based actions for emacs


## Prerequisites

Currently only macOS is supported.


## Installation

```emacs-lisp
(add-to-list 'load-path "path-to-elcontext")
(require 'elcontext)
(elcontext-global-mode)
```


## Usage

Use `M-x elcontext` for on overview of all contexts. Within this overview several hydras will guide through the API, press `?` to open the help hydra.

A contexts consists of a

-   name,
-   location,
-   timespan
-   and action.

![img](https://github.com/rollacaster/elcontext/blob/master/screenshot.png)

When the current time is within the timespan and your current position within 100 meters of the location the action is triggered once per day.
