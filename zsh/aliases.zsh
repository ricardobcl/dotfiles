# reload zsh config
# alias reload!='source ~/.zshrc'

# Detect which `ls` flavor is in use
# if ls --color > /dev/null 2>&1; then # GNU `ls`
    # colorflag="--color"
# else # OS X `ls`
    # colorflag="-G"
# fi


# Filesystem aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....="cd ../../.."
alias .....="cd ../../../.."

# alias l="ls -lah ${colorflag}"
# alias la="ls -AF ${colorflag}"
# alias ll="ls -lFh ${colorflag}"
# alias lld="ls -l | grep ^d"
# alias rmf="rm -rf"

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"
alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"

# Flush Directory Service cache
alias flush="dscacheutil -flushcache"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Trim new lines and copy to clipboard
# alias trimcopy="tr -d '\n' | pbcopy"

# Recursively delete `.DS_Store` files
alias cleanup="find . -name '*.DS_Store' -type f -ls -delete"

# File size
# alias fs="stat -f \"%z bytes\""

# Empty the Trash on all mounted volumes and the main HDD
# alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; rm -rfv ~/.Trash"

# Stuff I never really use but cannot delete either because of http://xkcd.com/530/
# alias stfu="osascript -e 'set volume output muted true'"
# alias pumpitup="osascript -e 'set volume 10'"

# tmux
# alias tma='tmux attach -d -t'
# alias tml='tmux list-sessions'
# alias tmd='tmux detach'

# Better cli tools from
# https://remysharp.com/2018/08/23/cli-improved
# If I want to run the original command, which is sometimes I do need to do, then there's two ways I can do this (I'm on a Mac so your mileage may vary):
# -> \cat # ignore aliases named "cat" - explanation: https://stackoverflow.com/a/16506263/22617
# -> command cat # ignore functions and aliases
alias cat="bat"
alias ping="prettyping --nolegend"
alias preview="fzf --preview 'bat --color \"always\" {}'"
# add support for ctrl+o to open selected file in VS Code
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"
alias top="sudo htop" # alias top and fix high sierra bug
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias help='tldr'
alias ll="exa -l"
