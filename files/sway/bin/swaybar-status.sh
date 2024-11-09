# swaybar status configuration script

# Modified from:
# https://git.sr.ht/~oscarcp/ghostfiles/tree/master/item/sway_wm/scripts/sway_bar.sh
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
# Audio
#
audio_volume=$(wpctl get-volume \@DEFAULT_AUDIO_SINK@ | awk '{print 100*$2}')
audio_is_muted=$(wpctl get-volume \@DEFAULT_AUDIO_SINK@ | awk '{print $3}')
audio_is_bluetooth=$(wpctl status | grep -E -o "bluez5")

if [ $audio_is_bluetooth = "bluez5" ]; then
    audio_active='🎧'
elif [ $audio_is_muted = "[MUTED]" ]; then
    audio_active=''
elif [ $audio_volume -ge 50 ]; then
    audio_active=''
else
    audio_active=''
fi

#
# Multimedia
#
media_artist=$(playerctl metadata artist)
media_song=$(playerctl metadata title)
player_status=$(playerctl status)

if [ $player_status = "Playing" ]; then
    media_status="▶ $media_artist: $media_song | "
elif [ $player_status = "Paused" ]; then
    media_status="⏸ $media_artist: $media_song | "
else
    media_status=""
    # media_status="⏹ $media_artist: $media_song | "
fi

echo $media_status \
     $audio_active $audio_volume% \
     " | " $battery_pluggedin $battery_charge% \
     " | " $date_and_week $current_time
