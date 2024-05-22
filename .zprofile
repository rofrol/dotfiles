# What Goes to ~/.zprofile and What Goes to ~/.zshrc?
# Since ~/.zprofile is loaded only once at login time, it’s best to put things that are loaded only once and can be inherited by subshells. An excellent example of this is environment variables.

# On the other hand, ~/.zshrc is typically reserved for things that are not inheritable by subshells, such as aliases and functions, custom prompts, history customizations, and so on.

# In addition, put the commands that should run every time you launch a new shell in the .zshrc file, too.
# https://www.zerotohero.dev/zshell-startup-files/

source ~/.zshrc
