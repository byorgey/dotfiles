if [ $UID -eq 0 ]; then CARETCOLOR="red"; else CARETCOLOR="blue"; fi

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

_my_theme_vcs_info() {
  jj_prompt_template 'self.change_id().shortest(3)' \
  || git_prompt_info
}

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}±%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN='.'

ZSH_THEME_GIT_PROMPT_BEHIND_REMOTE='↓'
ZSH_THEME_GIT_PROMPT_AHEAD_REMOTE='↑'
ZSH_THEME_GIT_PROMPT_DIVERGED_REMOTE='↕'

PROMPT='%m %{${fg_bold[blue]}%}:: %{$reset_color%}%{${fg[green]}%}%3~ %{${fg_bold[$CARETCOLOR]}%}»%{${reset_color}%} '

RPROMPT='${return_code} $(_my_theme_vcs_info)%{$fg[yellow]%}$(git_remote_status)%{$reset_color%} %{${fg[cyan]}%}[$(battery) %t]%{$reset_color%}'

