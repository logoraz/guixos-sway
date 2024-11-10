# dotfiles

Custom Guix System Distribution + Sway Configuration.


## Screenshots

![View 1](files/assets/screenshots/guix-sway-expose-1__2024-11-09.png)

![View 2](files/assets/screenshots/guix-sway-expose-2__2024-11-09.png)


## Project Scaffolding

```scm

    dotfiles/                     ;; Project root directory
    |- config/
       |- home/                   ;; Guix Home directory
          |- dot-bash_profile.sh
          |- dot-bashrc.sh
          |- home-config.scm
          |- services/
             |- emacs-guile.scm
             |- environment.scm
             |- raz-emacs.scm
             |- sway-desktop.scm
             |- udiskie.scm
             |- xdg-files.scm
       |- packages/               ;; Custom Packages
          |- raz-emacs.scm
          |- tbd
       |- services/               ;; System Services
          |- tbd
          |- tbd
       |- system/                 ;; Guix System directory
          |- base-system.scm
          |- channels.scm
          |- system-config.scm
         
```


## Goals

  - Create a highly functional WM setup that is efficient & aesthetically pleasing 
  - Construct a modulur Guix System configuration as a Guile program
  - Handle customizations as custom Guix packages or service extensions
  - Use Guix, as much as possible, to handle every other part of my workflow
  - Optimize to create an efficient system for programing in Guile Scheme
  - Eventually create a System/Home service utility that enhances Guix   


## Download & Install

    First download and install Guix System from either of the release images:
    
    1. https://gitlab.com/nonguix/nonguix/-/releases
    2. https://github.com/SystemCrafters/guix-installer/releases/tag/v202410220127

    Once Guix has been installed from the images do a `guix pull` and `system reconfigure`
    to get to the latest.

    Next, download this project repo and install as follows:

```bash

    $ git clone https://codeberg.org/loraz/dotfiles ~/.dotfiles
    $ cd ~/.dotfiles

    $ sudo guix system -L ~/.dotfiles/ reconfigure ~/.dotfiles/config/system/system-config.scm
    $ guix home -L ~/.dotfiles/ reconfigure ~/.dotfiles/config/home/home-config.scm
        
```


## References:

  - TBD
