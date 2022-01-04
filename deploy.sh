### deploy all dot files

rm ~/.zshrc
ln -s ~/dev/dotfiles/.zshrc ~/.zshrc
rm ~/.myaliases.sh
ln -s ~/dev/dotfiles/.myaliases.sh ~/.myaliases.sh
rm ~/.emacs
ln -s ~/dev/dotfiles/.emacs ~/.emacs
rm ~/.gitconfig
ln -s ~/dev/dotfiles/.gitconfig ~/.gitconfig

##kitty the terminal emulator
rm ~/.config/kitty/kitty.conf
ln -s ~/dev/dotfiles/kitty.conf ~/.config/kitty/kitty.conf

rm ~/.Rprofile
ln -s ~/dev/dotfiles/.Rprofile ~/.Rprofile

# rm ~/.hyper.js
# ln -s ~/dev/dotfiles/.hyper.js ~/.hyper.js

[ -f ~/.localrc ] && rm ~/.localrc
[ -f ~/dev/dotfiles/.localrc ] && ln -s ~/dev/dotfiles/.localrc ~/.localrc
