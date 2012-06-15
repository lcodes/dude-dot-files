# Install Zsh with Oh My Zsh
cd ~
apt-get install -y zsh
[ ! -d .oh-my-zsh ] && git clone https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
if [ -e .zshrc ]; then
    cp .zshrc .zshrc.back
    rm .zshrc
fi
cp .oh-my-zsh/templates/zshrc.zsh-template .zshrc
echo "export PATH=$PATH" >> .zshrc
chsh -s zsh