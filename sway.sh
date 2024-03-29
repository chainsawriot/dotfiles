# Read `man 5 sway` for a complete reference.

include /etc/sway/config.d/*
include /etc/sway/config-vars.d/*

### Variables
# Logo key. Use Mod1 for Alt.
set $mod Mod4
# Your preferred terminal emulator
set $term gnome-terminal
# Your preferred application launcher
set $menu rofi -modi drun -show drun

### Output configuration
#
# Example configuration:
#
#   output HDMI-A-1 resolution 1920x1080 position 1920,0
#
# You can get the names of your outputs by running: swaymsg -t get_outputs

output * bg ~/Documents/wall.jpg fill

output HDMI-A-1 resolution 1920x1080 position 0,0
output eDP-1 disable
### Idle configuration
#
exec swayidle -w \
         timeout 180 'swaylock -f -c 000000' \
         timeout 300 'swaymsg "output HDMI-A-1 dpms off"' resume 'swaymsg "output HDMI-A-1 dpms on"' \
         before-sleep 'swaylock -f -c 000000'


### Key bindings
#

# Exit sway (logs you out of your Wayland session)
bindsym $mod+Shift+e exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'
#
# Moving around:
#
# Move your focus around
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# Move the focused window with the same, but add Shift
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right
#
#
# Layout stuff:
#
# You can "split" the current object of your focus with
# $mod+b or $mod+v, for horizontal and vertical splits
# respectively.
bindsym $mod+b splith
bindsym $mod+v splitv

# Switch the current container between different layout styles
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# Make the current focus fullscreen
bindsym $mod+f fullscreen

# Toggle the current focus between tiling and floating mode
bindsym $mod+Shift+space floating toggle

# Swap focus between the tiling area and the floating area
bindsym $mod+space focus mode_toggle

# Move focus to the parent container
bindsym $mod+a focus parent

## my sh*t
mode "stumpwm" {
     bindsym Return mode "default"
     bindsym Escape mode "default"
     bindsym g mode "default"
     bindsym Control+g mode "default"
     bindsym f fullscreen; mode "default"
     bindsym r reload; mode "default"
     bindsym bracketright exec $menu; mode "default"
     bindsym n focus left; mode "default"
     bindsym p focus right; mode "default"
     bindsym k kill; mode "default"
     bindsym q exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'; mode "default"
     bindsym Shift+q exec swaymsg exit
     bindsym b exec swaymsg "[app_id=firefox] focus" || exec firefox; mode "default"
     bindsym c exec swaymsg "[app_id=gnome-terminal-server] focus" || exec $term; mode "default"
     bindsym e exec swaymsg "[instance=emacs] focus" || exec emacsclient -c; mode "default"
     bindsym s exec swaymsg "[app_id=psst-gui] focus" || exec psst-gui; mode "default"
     ## C-. o to use only the laptop screen; C-. O to change it back
     bindsym o exec swaymsg "Output HDMI-A-1 disable"; exec swaymsg "Output eDP-1 enable"; mode "default"
     bindsym Shift+o exec swaymsg "Output HDMI-A-1 enable"; exec swaymsg "Output eDP-1 disable"; mode "default"
}

bindsym Control+period mode "stumpwm"

bindsym Print exec grim

#
# Status Bar:
#
# Read `man 5 sway-bar` for more information about this section.
bar {
    position top

    # When the status_command prints a new line to stdout, swaybar updates.
    # The default just shows the current date and time.
    status_command while ~/.config/sway/status.sh; do sleep 10; done

    colors {
        statusline #ebdbb2
        background #282828
        inactive_workspace #32323200 #32323200 #5c5c5c
    }
}

workspace_layout tabbed
font "FiraCode" 10

############## Things that I don't use
# Workspaces (I don't use Workspaces):
#
    # Switch to workspace
    #bindsym $mod+1 workspace number 1
    # bindsym $mod+2 workspace number 2
    # bindsym $mod+3 workspace number 3
    # bindsym $mod+4 workspace number 4
    # bindsym $mod+5 workspace number 5
    # bindsym $mod+6 workspace number 6
    # bindsym $mod+7 workspace number 7
    # bindsym $mod+8 workspace number 8
    # bindsym $mod+9 workspace number 9
    # bindsym $mod+0 workspace number 10
    # Move focused container to workspace
    #bindsym $mod+Shift+1 move container to workspace number 1
    # bindsym $mod+Shift+2 move container to workspace number 2
    # bindsym $mod+Shift+3 move container to workspace number 3
    # bindsym $mod+Shift+4 move container to workspace number 4
    # bindsym $mod+Shift+5 move container to workspace number 5
    # bindsym $mod+Shift+6 move container to workspace number 6
    # bindsym $mod+Shift+7 move container to workspace number 7
    # bindsym $mod+Shift+8 move container to workspace number 8
    # bindsym $mod+Shift+9 move container to workspace number 9
    # bindsym $mod+Shift+0 move container to workspace number 10
    # Note: workspaces can have any name you want, not just numbers.
    # We just use 1-10 as the default.
#
# Scratchpad (I don't use Scratchpad):
#
    # Sway has a "scratchpad", which is a bag of holding for windows.
    # You can send windows there and get them back later.

    # Move the currently focused window to the scratchpad
    #bindsym $mod+Shift+minus move scratchpad

    # Show the next scratchpad window or hide the focused scratchpad window.
    # If there are multiple scratchpad windows, this command cycles through them.
    #bindsym $mod+minus scratchpad show

#
# Resizing containers (I don't resize, but keep it just in case):
#
mode "resize" {
    # left will shrink the containers width
    # right will grow the containers width
    # up will shrink the containers height
    # down will grow the containers height
    #bindsym $left resize shrink width 10px
    #bindsym $down resize grow height 10px
    #bindsym $up resize shrink height 10px
    #bindsym $right resize grow width 10px

    # Ditto, with arrow keys
    bindsym Left resize shrink width 10px
    bindsym Down resize grow height 10px
    bindsym Up resize shrink height 10px
    bindsym Right resize grow width 10px

    # Return to default mode
    bindsym Return mode "default"
    bindsym Escape mode "default"
}
bindsym $mod+r mode "resize"

# Basics:
#
# Drag floating windows by holding down $mod and left mouse button.
# Resize them with right mouse button + $mod.
# Despite the name, also works for non-floating windows.
# Change normal to inverse to use left mouse button for resizing and right
# mouse button for dragging.
#floating_modifier $mod normal

# Reload the configuration file
##bindsym $mod+Shift+c reload

### Input configuration
#
# Example configuration:
#
#   input "2:14:SynPS/2_Synaptics_TouchPad" {
#       dwt enabled
#       tap enabled
#       natural_scroll enabled
#       middle_emulation enabled
#   }
#
# You can get the names of your inputs by running: swaymsg -t get_inputs
# Read `man 5 sway-input` for more information about this section.

# Home row direction keys, like vim (don't make it like vim!)
#set $left h
#set $down j
#set $up k
#set $right l
