#!/usr/bin/env bash

get_tmux_option() {
  local option=$1
  local default_value=$2
  local option_value=$(tmux show-option -gqv "$option")
  if [ -z $option_value ]; then
    echo $default_value
  else
    echo $option_value
  fi
}

main()
{
  # set current directory variable
  current_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

  # set configuration option variables
  show_battery=$(get_tmux_option "@dracula-show-battery" false)
  show_network=$(get_tmux_option "@dracula-show-network" true)
  show_weather=$(get_tmux_option "@dracula-show-weather" false)
  show_fahrenheit=$(get_tmux_option "@dracula-show-fahrenheit" false)
  show_powerline=$(get_tmux_option "@dracula-show-powerline" true)
  show_left_icon=$(get_tmux_option "@dracula-show-left-icon" session)
  show_military=$(get_tmux_option "@dracula-military-time" false)
  show_left_sep=$(get_tmux_option "@dracula-show-left-sep" "")
  show_right_sep=$(get_tmux_option "@dracula-show-right-sep" "")
  show_border_contrast=$(get_tmux_option "@dracula-border-contrast" false)
  show_cpu_usage=$(get_tmux_option "@dracula-cpu-usage" false)
  show_ram_usage=$(get_tmux_option "@dracula-ram-usage" false)
  show_gpu_usage=$(get_tmux_option "@dracula-gpu-usage" false)

  # Dracula Color Pallette
  white='#a89984'
  gray='#282828'
  dark_gray='#282828'
  light_purple='#b16286'
  dark_purple='#458588'
  cyan='#8ec07c'
  green='#b8bb26'
  orange='#d3869b'
  red='#fb4934'
  pink='#d3869b'
  yellow='#fabd2f'

  # Handle left icon configuration
  case $show_left_icon in
      smiley)
          left_icon="☺ ";;
      session)
          left_icon="#S ";;
      window)
	  left_icon="#W ";;
      *)
          left_icon=$show_left_icon;;
  esac
  left_icon=

  # Handle powerline option
  if $show_powerline; then
      right_sep="$show_right_sep"
      left_sep="$show_left_sep"
  fi

  # start weather script in background
  if $show_weather; then
    $current_dir/sleep_weather.sh $show_fahrenheit &
  fi
  
  # sets refresh interval to every 5 seconds
  tmux set-option -g status-interval 5

  # set clock to 12 hour by default
  tmux set-option -g clock-mode-style 24

  # set length 
  tmux set-option -g status-left-length 100
  tmux set-option -g status-right-length 100

  # pane border styling
  if $show_border_contrast; then
    tmux set-option -g pane-active-border-style "fg=${light_purple}"
  else
    tmux set-option -g pane-active-border-style "fg=${dark_purple}"
  fi
  tmux set-option -g pane-border-style "fg=${gray}"

  # message styling
  tmux set-option -g message-style "bg=${gray},fg=${white}"

  # status bar
  tmux set-option -g status-style "bg=${gray},fg=${white}"


  # Powerline Configuration
  if $show_powerline; then

      tmux set-option -g status-left "#[bg=${green},fg=${dark_gray}]#{?client_prefix,#[bg=${yellow}],} ${left_icon} #[fg=${green},bg=${gray}]#{?client_prefix,#[fg=${yellow}],}${left_sep}"
      tmux set-option -g  status-right ""
      powerbg=${gray}

      if $show_battery; then # battery
        tmux set-option -g  status-right "#[fg=${pink},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${dark_gray},bg=${pink}] #($current_dir/battery.sh)"
        powerbg=${pink}
      fi

      if $show_ram_usage; then
	 tmux set-option -ga status-right "#[fg=${cyan},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${dark_gray},bg=${cyan}] #($current_dir/ram_info.sh)"
	 powerbg=${cyan}
      fi

      if $show_cpu_usage; then
	 tmux set-option -ga status-right "#[fg=${orange},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${dark_gray},bg=${orange}] #($current_dir/cpu_info.sh)"
	 powerbg=${orange}
      fi

      if $show_gpu_usage; then
	 tmux set-option -ga status-right "#[fg=${pink},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${dark_gray},bg=${pink}] #($current_dir/gpu_usage.sh)"
	 powerbg=${pink}
      fi	

      if $show_network; then # network
        tmux set-option -g status-right "#[fg=${dark_gray},bg=${green}]${right_sep}#[fg=${dark_gray},bg=${green}] #($current_dir/network.sh)"
        powerbg=${green}
      fi

      if $show_weather; then # weather
        tmux set-option -ga status-right "#[fg=${orange},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${dark_gray},bg=${orange}] #(cat $current_dir/../data/weather.txt)"
        powerbg=${orange}
      fi

      # if $show_military; then # military time
	# tmux set-option -ga status-right "#[fg=${dark_purple},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${white},bg=${dark_purple}] %a %m/%d %R #(date +%Z) "
      # else
	# tmux set-option -ga status-right "#[fg=${dark_purple},bg=${powerbg},nobold,nounderscore,noitalics] ${right_sep}#[fg=${white},bg=${dark_purple}] %a %m/%d %I:%M %p #(date +%Z) "
      # fi

      tmux set-window-option -g window-status-current-format "#[fg=${gray},bg=${dark_purple}]${left_sep}#[fg=${dark_gray},bg=${dark_purple}] #I #W #[fg=${dark_purple},bg=${gray}]${left_sep}"
  
  # Non Powerline Configuration
  else
    tmux set-option -g status-left "#[bg=${green},fg=${dark_gray}]#{?client_prefix,#[bg=${yellow}],} ${left_icon}"

    tmux set-option -g  status-right ""

      if $show_battery; then # battery
        tmux set-option -g  status-right "#[fg=${dark_gray},bg=${pink}] #($current_dir/battery.sh) "
      fi
      if $show_ram_usage; then
	tmux set-option -ga status-right "#[fg=${dark_gray},bg=${cyan}] #($current_dir/ram_info.sh) "
      fi

      if $show_cpu_usage; then
	tmux set-option -ga status-right "#[fg=${dark_gray},bg=${orange}] #($current_dir/cpu_info.sh) "
      fi

      if $show_gpu_usage; then
	tmux set-option -ga status-right "#[fg=${dark_gray},bg=${pink}] #($current_dir/gpu_usage.sh) "
      fi	

      if $show_network; then # network
        tmux set-option -ga status-right "#[fg=${dark_gray},bg=${cyan}] #($current_dir/network.sh) "
      fi

      if $show_weather; then # weather
          tmux set-option -ga status-right "#[fg=${dark_gray},bg=${orange}] #(cat $current_dir/../data/weather.txt) "
      fi

      tmux set-window-option -g window-status-current-format "#[fg=${white},bg=${dark_purple}] #I #W "

  fi
  
  tmux set-window-option -g window-status-format "#[fg=${white}]#[bg=${gray}] #I #W "
}

# run main function
main
