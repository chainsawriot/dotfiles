battery_level=$(cat /sys/class/power_supply/BAT0/capacity)
battery_status=$(cat /sys/class/power_supply/BAT0/status)
current_date=$(date +'%Y-%m-%d %H:%M:%S')
uptime_formatted=$(uptime | cut -d ',' -f1  | cut -d ' ' -f4,5)
echo Uptime: $uptime_formatted / $battery_status "("$battery_level%")" / $current_date
