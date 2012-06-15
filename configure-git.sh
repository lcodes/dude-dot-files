# Configure Git
read -p "Name for git?" gituser
read -p "Email for git?" email
git config --global user.name $gituser
git config --global user.email $email
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'
git config --global color.diff auto
git config --global color.status auto
git config --global color.branch auto
