#!/usr/bin/sh

__ps1() {
    # website for colors: https://misc.flogisoft.com/bash/tip_colors_and_formatting
    # the \[\] is because without it, bash breaks
    EXIT_CODE=$?
    RESET='\[[0m\]'
    FG_RED='\[[31m\]'
    FG_GREEN='\[[32m\]'
    FG_BLUE='\[[34m\]'
    FG_CYAN='\[[36m\]'
    BOLD='\[[1m\]'

    set_exit_code_color() {
        [ "$EXIT_CODE" -eq 0 ] && printf '%s' "$FG_GREEN" || printf '%s' "$FG_RED"
    }

    CWD=${PWD##*/}
    NEWLINE=" "
    if [ ${#CWD} -ge 15 ]; then
        NEWLINE="
"
    fi

    printf '%s%s[\W]%s%s' \
           "$BOLD" \
           "$FG_BLUE" \
           "$RESET" \
           "$FG_CYAN"
    powergit
    printf '%s%s%s; ' \
           "$NEWLINE" \
           "$EXIT_CODE"\
           "$RESET"
}

PROMPT_COMMAND='PS1=$(__ps1)'
