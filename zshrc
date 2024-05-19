# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="byorgey"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(cabal git github screen sudo wd)
# common-aliases?

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
autoload -Uz compinit && compinit
fpath=(~/.zsh/completions $fpath)
source ~/.zsh_env
source ~/.zsh_aliases
source ~/.zsh_local

autoload bashcompinit
bashcompinit
source ~/.local/lib/todo_completion
complete -F _todo t

source <(jj util completion zsh)

# source ~/.xsh

# $HOME/local/mybin/doomsday-test
 
if [ -e /home/brent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/brent/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# opam configuration
test -r /home/brent/.opam/opam-init/init.zsh && . /home/brent/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
. "/home/brent/.acme.sh/acme.sh.env"
