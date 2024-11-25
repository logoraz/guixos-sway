# dotfiles

GuixOS "Sway Train": Custom Guix System Distribution + Sway Configuration.


## Screenshots

![View 1](files/assets/screenshots/guix-sway-exhibit-1__2024-11-24.png)

![View 2](files/assets/screenshots/guix-sway-exhibit-2__2024-11-24.png)


## Project Scaffolding

```scm
    dotfiles/                        ;; Project root
    |- config/
       |- home/                      ;; Guix Home config
          |- home-config.scm         ;; ** Home Config **
          |- dot-bash_profile.sh               
          |- dot-bashrc.sh
          |- services/               ;; Home Services
             |- emacs-guile.scm
             |- environment.scm
             |- raz-emacs.scm
             |- streaming.scm
             |- sway-desktop.scm
             |- udiskie.scm
             |- xdg-files.scm
       |- packages/                  ;; Custom Guix Packages
          |- raz-emacs.scm
          |- video.scm
       |- services/                  ;; Custom System Services
          |- tbd
       |- system/                    ;; Guix System config
          |- system-config.scm       ;; ** System Config **
          |- base-system.scm
          |- channels.scm
    |- files/                        ;; XDG_CONFIG_HOME Files
       |- assets/...
       |- guile/...
       |- gtk-3.0/...
       |- foot/...
       |- vterm/...
       |- fuzzel/...
       |- mako/...
       |- wlogout/...
       |- qutebrowser/...
       |- gnupg/...    
       |- sway/                      ;; Sway WM config
          |- config
          |- bin/
             |- swaybar-status.sh
             |- toggle-display.sh    ;; WIP
```


## Philosophy

 - Create a highly functional WM setup that is efficient & aesthetic 
 - Construct a modulur Guix System configuration as a Guile program
 - Handle customizations as custom Guix packages & services
 - Configure everything using Guile/Guix where possible!
 - Generalize to support setup on multiple machines
 - Optimize to create an efficient system for hacking in Guile Scheme
 - Eventually create a System/Home service utility that enhances Guix   


## Download & Install

First download and install Guix System from either of the release images below:
    
 - https://gitlab.com/nonguix/nonguix/-/releases
 - https://github.com/SystemCrafters/guix-installer

Once Guix has been installed from the images do a `guix pull` and
`guix system reconfigure` to get to the latest.

Next, download this project repo and install as follows:

```bash
    $ git clone https://codeberg.org/loraz/dotfiles ~/.dotfiles
    $ cd ~/.dotfiles

    $ sudo guix system -L ~/.dotfiles/ reconfigure ~/.dotfiles/config/system/system-config.scm
    $ guix home -L ~/.dotfiles/ reconfigure ~/.dotfiles/config/home/home-config.scm
```


## Establishing Wifi

This can be done with the UI via `nm-connection-editor`, but I like to keep
reference to `nmcli`. Working on figuring out how to establish a 'clickable'
swaybar icon to automatically bring up `nm-connection-editor`. stay tuned...

```bash
  # List Wifi networks available
  $ sudo nmcli device wifi list

  # Connect to ESSID
  $ sudo nmcli device wifi connect <ESSID> password <network-password>
```

List known connections and delete them from list in Network Manager.

```bash
  # Get ESSID (name) or UUID of connection to delete
  $ sudo nmcli connection show

  # Delete connection via ID (aka name or ESSID)
  $ sudo nmcli connection delete <ESSID>
```


## Establishing Bluetooth

Bluetooth works and is automatically deployed via bluez in Guix system config,
however, initial device connection first needs to be established via
`blueman-manager`.

Still working on better integration, such as auto-starting `blueman-applet`
in sway config, however, it currently has a bug in it's dbus service that has
yet to be addressed, see https://issues.guix.gnu.org/71528. Note this handles
bluetooth connection interfacing with sway, i.e. detection and notifications
so you can start it up in a shell (and get a ugly warning message) once in
your sway session - I usually don't and things still work, but with out
notifications...

## TODOs (Wish List)

 - Configure Sway in Guile/Guix via `home-sway-service-type`
 - Minimize dependency on `xdg-files (reserved for one-off's where possible)
 - Create custom system service to handle package substitutes & transformations
 - Create base system config to apply to machine-specific configurations. 
 - Research solution to blueman-applet: https://issues.guix.gnu.org/71528
 - Update Emacs config to (use Guile/Guix) 'home-emacs-service-type` once available

   
## References:

  - TBD
