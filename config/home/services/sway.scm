(define-module (config home services sway)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services utils)
  #:use-module (gnu home services sway)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (config packages gubar)
  #:export (sway-configuration-extension
            home-sway-configuration-service-type))


;;; #:TODO/251026 - Update to dyanimcall set bg-path & outputs/workspaces
;;; --> See commented out entries for incorporation
(define %bg-path "~/guixos/files/assets/wallpapers/primary-wall.jpg")

(define (make-primary-outputs identifiers)
  (map (lambda (identifier)
         (sway-output
           (identifier identifier)
           (extra-content '("scale 1.6"))))
       identifiers))

(define workspace-list
  '((ws0 "0" "$laptop")
    (ws2 "2" "$primary $laptop")
    (ws3 "3" "$laptop")
    (ws4 "4" "$laptop")
    (ws5 "5" "$laptop")
    (ws1 "1" "$primary $laptop")))

(define %sway-config-base-variables
  `((mod . "Mod4") ;; Super key (note Super := Mod4, Alt := Mod1)
    (system_theme . "Adwaita-dark")
    (system_icons . "Qogir-Dark")
    (system_font  . "Iosevka Aile 11")
    (cursor_theme . "Bibata-Modern-Classic")
    (cursor_size . "20")
    (titlebar_font . "Iosevka Aile")
    (titlebar_text_size . "11")
    (system_dpi_scaling_factor . "1.6")
    (system_text_scaling_factor . "1")
    (gnome_schema . "org.gnome.desktop.interface")
    (laptop . "eDP-1")
    ;;#:TODO/251026
    ;; (primary . "TBD")
    ;; (map (match-lambda
    ;;        ((ws num _)
    ;;         (cons ws num)))
    ;;      workspace-list)
    (ws0 . "0")
    (ws1 . "1")
    (ws2 . "2")
    (ws3 . "3")
    (ws4 . "4")
    (ws5 . "5")
    (bgcolor . "#1d1f21dd")
    (bordercolor . "#5e81accc")
    (lock . ,(string-append "swaylock "
                            "-f --screenshots --clock "
                            "--effect-blur 9x7 "
                            "--effect-vignette 0.25:0.5 "
                            "--grace 60 --fade-in 0.25"))
    (qlock . ,(string-append "swaylock "
                             "-f --screenshots --clock "
                             "--effect-blur 9x7 "
                             "--effect-vignette 0.25:0.5"))
    (lbrt_down . "--locked XF86MonBrightnessDown")
    (rbrt_down . "--release XF86MonBrightnessDown")
    (lbrt_up . "--locked XF86MonBrightnessUp")
    (rbrt_up . "--release XF86MonBrightnessUp")
    (lad_mute . "--locked XF86AudioMute")
    (rad_mute . "--release XF86AudioMute")
    (lad_lv . "--locked  XF86AudioLowerVolume")
    (rad_lv . "--release  XF86AudioLowerVolume")
    (lad_rv . "--locked  XF86AudioRaiseVolume")
    (rad_rv . "--release  XF86AudioRaiseVolume")))

(define %sway-config-base-inputs
  (list (sway-input
          (identifier "type:keyboard")
          (layout (keyboard-layout "us,il" #:options '("ctrl:nocaps"))))
        (sway-input
          (identifier "type:touchpad")
          (tap #t)
          (disable-while-typing #t)
          (extra-content
           '("events disabled")))))

(define %sway-config-base-outputs
  (append
   ;;#:TODO/251026
   ;; (make-primary-outputs primary-outputs)
   (list
    (sway-output
      (identifier 'eDP-1)
      ;; Investigate
      ;; (position (point (x 1920)
      ;;                  (y 135)))
      (extra-content '("scale 1.6")))
    (sway-output
      (identifier '*)
      (background `(,%bg-path . fill))))))

(define %sway-config-base-keybindings
  `( ;; Sway System Controls
    ($mod+Shift+q . "kill")
    ($mod+Shift+x . "exit")
    ($mod+Shift+r . "reload")
    ;; Toggle Trackpad
    ($mod+Shift+t . ,(string-append
                      "exec swaymsg"
                      " input type:touchpad"
                      " events toggle enabled disabled"))
    ;; Window Focus
    ($mod+h . "focus left")
    ($mod+l . "focus right")
    ;; Move workspace to display (Alt --> Mod1)
    ($mod+Alt+h . "move workspace to output left")
    ($mod+Alt+l . "move workspace to output right")
    ($mod+Alt+Left . "move workspace to output left")
    ($mod+Alt+Right .  "move workspace to output right")
    ;; Alternatively, you can use cursor keys:
    ($mod+Shift+h . "move left 30 px")
    ($mod+Shift+j . "move down 30 px")
    ($mod+Shift+k . "move up 30 px")
    ($mod+Shift+l . "move right 30 px")

    ($mod+f . "fullscreen toggle")
    ($mod+Shift+f . "floating toggle")
    ($mod+Shift+p . "sticky toggle")
    ;; change focus between tiling / floating windows
    ($mod+Control+space . "focus mode_toggle")
    ;; App launcher - configured in fuzzel.ini (example of g-exp!)
    ($mod+space . ,#~(string-append "exec "
                                    #$fuzzel
                                    "/bin/fuzzel"))
    ;; Switch to workspace
    ($mod+grave . "workspace $ws0")
    ($mod+1 . "workspace $ws1")
    ($mod+2 . "workspace $ws2")
    ($mod+3 . "workspace $ws3")
    ($mod+4 . "workspace $ws4")
    ($mod+5 . "workspace $ws5")

    ;; Move focused container to workspace
    ($mod+Shift+grave . "move container to workspace $ws0")
    ($mod+Shift+1 . "move container to workspace $ws1")
    ($mod+Shift+2 . "move container to workspace $ws2")
    ($mod+Shift+3 . "move container to workspace $ws3")
    ($mod+Shift+4 . "move container to workspace $ws4")
    ($mod+Shift+5 . "move container to workspace $ws5")
    ;; Lock Screen
    ($mod+Shift+o . "exec $qlock")
    ;; Sway session controls
    ($mod+Shift+space . "exec wlogout -p layer-shell")
    ;; Screenshots
    (Print . "exec grimshot --notify save output")
    (Alt+Print . "exec grimshot --notify save area")))

(define %sway-config-base-extra-content
  `( ;; Mouse
    "seat seat0 xcursor_theme $cursor_theme $cursor_size"

    ;; Fonts
    "font pango:$titlebar_font $titlebar_text_size"

    ;; Brightness control
    "bindsym $lbrt_down exec brightnessctl set 5%-"
    "bindsym $rbrt_down exec pkill -SIGRTMIN+4 -n gubar"
    "bindsym $lbrt_up exec brightnessctl set 5%+"
    "bindsym $rbrt_up exec pkill -SIGRTMIN+4 -n gubar"

    ;; Volume control
    "bindsym $lad_mute exec pactl set-sink-mute @DEFAULT_SINK@ toggle"
    "bindsym $rad_mute exec pkill -SIGRTMIN+2 -n gubar"
    "bindsym $lad_lv exec pactl set-sink-volume @DEFAULT_SINK@ -5%"
    "bindsym $rad_lv exec pkill -SIGRTMIN+2 -n gubar"
    "bindsym $lad_rv exec pactl set-sink-volume @DEFAULT_SINK@ +5%"
    "bindsym $rad_rv exec pkill -SIGRTMIN+2 -n gubar"

    ;; Audio Player controls
    "bindsym --locked XF86AudioPlay exec playerctl play-pause"
    "bindsym --locked XF86AudioNext exec playerctl next"
    "bindsym --locked XF86AudioPrev exec playerctl previous"

    ;; Set defaults
    "default_orientation horizontal"
    "workspace_layout tabbed"

    ;; Configure gaps and borders
    "default_border pixel 1"
    "gaps outer 0"
    "gaps inner 6"
    "smart_borders off"
    "hide_edge_borders --i3 none"

    ;; Move specific workspaces to outputs
    "workspace $ws0 output $laptop"
    "workspace $ws2 output $primary $laptop"
    "workspace $ws3 output $laptop"
    "workspace $ws4 output $laptop"
    "workspace $ws5 output $laptop"
    ;; Set ws 1 last to land there on startup
    "workspace $ws1 output $primary $laptop"

    ;; Floating Screens
    "floating_modifier $mod"

    ;; Workspaces
    ;;#:TODO/251026
    ;; ,@(map (match-lambda
    ;;          ((ws num outputs)
    ;;           (format #f "workspace $~a output ~a" ws outputs)))
    ;;        workspace-list))

    ;; Style the UI
    ;; border | background | text | indicator | child border
    "client.focused $bordercolor $bgcolor #ffffffff #5e81accc #5e81accc"
  "client.unfocused $bordercolor #1c1f2bef #ffffffff #5e81accc #5e81accc"))

(define %sway-base-config-startup+reload-programs
  `( ;; GTK
    ;; This is the only place where you must set GTK scaling
    "gsettings set $gnome_schema gtk-theme $system_theme"
    "gsettings set $gnome_schema icon-theme $system_icons"
    "gsettings set $gnome_schema text-scaling-factor $system_text_scaling_factor"
    "gsettings set $gnome_schema cursor-theme $cursor_theme"
    "gsettings set $gnome_schema cursor-size $cursor_size"))

(define %sway-base-config-startup-programs
  `( ;; Set default brightness & backlight
    "brightnessctl set 50%"
    "brightnessctl -d chromeos::kbd_backlight set 10%"
    ;; Idle screen configuration
    ,(string-append "swayidle -w "
                    "timeout 900 '$lock' "
                    "timeout 960 'swaymsg \"output * dpms off\"' "
                    "resume 'swaymsg \"output * dpms on\"' "
                    "before-sleep '$lock'")
    ;; Night Light (CA lat/lon)
    ,(string-append "wlsunset -l 34.1 -L -117.7")
    ;; Utility applications
    "mako"
    "udiskie -s"
    "blueman-applet"
    ;;Update DBUS activation records to ensure Flatpak apps work
    ,(string-append "dbus-update-activation-environment "
                    "--systemd DISPLAY WAYLAND_DISPLAY "
                    "XDG_CURRENT_DESKTOP=sway")))

(define %sway-base-packages
  (list swaybg
        swayidle
        fuzzel
        wlogout
        mako
        grimshot ;; grimshot --notify copy area
        wlsunset
        wl-clipboard
        librsvg
        gubar
        foot))

(define* (swaybar->config #:key (identifier 'bar0))
  "Return a sway-bar configuration record."
  (sway-bar
    (identifier identifier)
    (position 'top)
    (status-command "gubar")
    (colors
     (sway-color
       (statusline "#eceff4")
       (background "#2e3440")
       (focused-workspace (sway-border-color
                            (border "#81a1c1")
                            (background "#5e81ac")
                            (text "#eceff4")))
       (active-workspace (sway-border-color
                           (border "#81a1c1")
                           (background "#5e81ac")
                           (text "#eceff4")))
       (inactive-workspace (sway-border-color
                             (border "#2e3440")
                             (background "#4c566a")
                             (text "#d8dee9")))))
    (extra-content
     '("font pango:JetBrains Mono 11"
       "icon_theme Qogir-Dark"
       "tray_bindsym button1 Activate"
       "tray_bindsym button2 ContextMenu"))))

(define (sway-configuration-extension config)
  (let ((base (or config %empty-sway-configuration)))
    (sway-configuration
      (inherit base)

      (variables
       (append
        (sway-configuration-variables base)
        %sway-config-base-variables))

      (inputs
       (append
        (sway-configuration-inputs base)
        %sway-config-base-inputs))

      (outputs
       (append
        (sway-configuration-outputs base)
        %sway-config-base-outputs))

      (keybindings
       (append
        (sway-configuration-keybindings base)
        %sway-config-base-keybindings))

      (extra-content
       (append
        (sway-configuration-extra-content base)
        %sway-config-base-extra-content))

      (bar (swaybar->config #:identifier 'gubar))

      (startup+reload-programs
       (append
        (sway-configuration-startup+reload-programs base)
        %sway-base-config-startup+reload-programs))

      (startup-programs
       (append
        (sway-configuration-startup-programs base)
        %sway-base-config-startup-programs))

      (packages
       (append
        (sway-configuration-packages base)
        %sway-base-packages)))))


;; Edit setting the Home User
(define %user-name "logoraz")

(define %source (string-append "/home"
                               "/" %user-name
                               "/guixos"))

(define* (resolve dir #:key file)
  "Resolve local config dir & file"
  (let ((filename (if file (string-append "/" file) "")))
    (local-file (string-append
                 %source "/"
                 dir filename)
                #:recursive? #t)))

(define (home-sway-files-service config)
  "Provide symlinks for other programs configs that Sway uses."
  `( ;;Sway Bar --> gubar Configuration
    (".config/gubar"
     ,(resolve "files/gubar"))

    ;; Application Selector
    (".config/mako"
     ,(resolve "files/mako"))

    ;; Screen Locking
    (".config/fuzzel"
     ,(resolve "files/fuzzel"))

    ;; UI Logout Application
    (".config/wlogout"
     ,(resolve "files/wlogout"))

    ;; Default Sway/Wayland Terminal
    (".config/foot"
     ,(resolve "files/foot"))))

(define home-sway-configuration-service-type
  (service-type (name 'home-sway-service-extension)
                (description "Custom home service to confiure Sway.")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-sway-files-service)))
                (default-value #f)))
