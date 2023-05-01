### deploy all dot files

[ -f ~/.zshrc ] && rm ~/.zshrc
ln -s ~/dev/dotfiles/.zshrc ~/.zshrc
[ -f ~/.myaliases.sh ] && rm ~/.myaliases.sh
ln -s ~/dev/dotfiles/.myaliases.sh ~/.myaliases.sh
[ -f ~/.emacs ] && rm ~/.emacs
ln -s ~/dev/dotfiles/.emacs ~/.emacs
[ -f ~/.gitconfig ] && rm ~/.gitconfig
ln -s ~/dev/dotfiles/.gitconfig ~/.gitconfig

##kitty the terminal emulator
[ -f ~/.config/kitty/kitty.conf ] && rm ~/.config/kitty/kitty.conf
ln -s ~/dev/dotfiles/kitty.conf ~/.config/kitty/kitty.conf

[ -f ~/.Rprofile ] && rm ~/.Rprofile
ln -s ~/dev/dotfiles/.Rprofile ~/.Rprofile

# rm ~/.hyper.js
# ln -s ~/dev/dotfiles/.hyper.js ~/.hyper.js

[ -f ~/.stumpwmrc ] && rm ~/.stumpwmrc
ln -s ~/dev/dotfiles/stumpwm.lisp ~/.stumpwmrc

# [ -f ~/.config/awesome ] && rm ~/.config/awesome
# ln -s ~/dev/dotfiles/awesome ~/.config/awesome

#[ -f ~/.xmonad/xmonad.hs ] && rm ~/.xmonad/xmonad.hs
#ln -s ~/dev/dotfiles/xmonad.hs ~/.xmonad/xmonad.hs

[ -f ~/.config/sway/config ] && rm ~/.config/sway/config
ln -s ~/dev/dotfiles/sway.sh ~/.config/sway/config

chmod +x ~/dev/dotfiles/status.sh
[ -f ~/.config/sway/status.sh ] && rm ~/.config/sway/status.sh
ln -s ~/dev/dotfiles/status.sh ~/.config/sway/status.sh

[ -f ~/.localrc ] && rm ~/.localrc
[ -f ~/dev/dotfiles/.localrc ] && ln -s ~/dev/dotfiles/.localrc ~/.localrc

[ -f ~/.cargo/config ] && rm ~/.cargo/config
ln -s ~/dev/dotfiles/cargo ~/.cargo/config

[ -f ~/.config/bat/config ] && rm ~/.config/bat/config
ln -s ~/dev/dotfiles/bat.config ~/.config/bat/config
