# swaybar status configuration script
#

#
# Date & Time
#
date_and_week=$(date "+%a %Y-%m-%d W%U")
current_time=$(date "+%I:%M:%S")


#
# Battery/Charger Status
#
battery_charge=$(upower --show-info $(upower --enumerate | grep 'BAT') \
               | grep -E "percentage" | awk '{print $2}' | grep -E -o [0-9]*)

# battery_status=$(upower --show-info $(upower --enumerate | grep 'BAT') \
#                | egrep "state" | awk '{print $2}')

# Alternate method: Returns the battery status: "Full", "Discharging", or "Charging".
battery_status=$(cat /sys/class/power_supply/BAT0/status)

if [ "$battery_status" != "Discharging" ] || [ "$battery_status" = "Not charging" ]; then
    battery_pluggedin='⚡'
elif [ $battery_charge -lt 34 ]; then
    battery_pluggedin=''
elif [ $battery_charge -le 50 ]; then
    battery_pluggedin=''
elif [ $battery_charge -lt 90 ]; then
    battery_pluggedin=''
elif [ $battery_charge -gt 90 ]; then
    battery_pluggedin=''
else
    battery_pluggedin=''
fi


#
# Display
#
current_brightness=$(brightnessctl | grep -E "Current brightness" \
                   | awk '{print $4}' | grep -E -o [0-9%]*)

brightness_symbol='🔆'


#
# Audio
#
audio_volume=$(wpctl get-volume \@DEFAULT_AUDIO_SINK@ | awk '{print 100*$2}')
audio_is_muted=$(wpctl get-volume \@DEFAULT_AUDIO_SINK@ | awk '{print $3}')
audio_is_bluetooth=$(wpctl status | grep -E -o "bluez5")
# Not sure if sink ID's are mutable... works for now!
bluetooth_is_muted=$(wpctl get-volume 64 | awk '{print $3}')

if [ $audio_is_muted = "[MUTED]" ] || [ $bluetooth_is_muted = "[MUTED]" ]; then
    audio_active=''
elif [ $audio_is_bluetooth = "bluez5" ]; then
    audio_active='🎧'
elif [ $audio_volume -ge 50 ]; then
    audio_active=''
else
    audio_active=''
fi


#
# Multimedia
#
player_status=$(playerctl status)
media_artist=$(playerctl metadata artist)
media_track=$(playerctl metadata title)
media_track_trunk=${media_track:0:24}

if [ $player_status = "Playing" ]; then
    media_status="▶ $media_artist: $media_track_trunk... |"
elif [ $player_status = "Paused" ]; then
    media_status="⏸ $media_artist: $media_track_trunk... |"
else
    media_status=""
    # media_status="⏹ $media_artist: $media_track_trunk | "
fi


#
# Status Bar Configuration
#
echo $media_status "" \
     $audio_active $audio_volume% " " \
     $brightness_symbol $current_brightness " " \
     $battery_pluggedin $battery_charge% " " \
     $date_and_week $current_time " "
