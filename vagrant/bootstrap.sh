# Install erlang
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
sudo apt-get update
sudo apt-get install erlang -y
rm -rf rebar3

# Install rebar3
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
su - vagrant -c "./rebar3 local install"
echo "export PATH=/home/vagrant/.cache/rebar3/bin:$PATH" >> /home/vagrant/.bashrc

# Enable erlang shell history
echo "export ERL_AFLAGS=\"-kernel shell_history enabled\"" >> /home/vagrant/.bashrc

# Setup passwordless ssh (for tests)
ssh-keygen -t rsa -P '' -f ~/.ssh/id_rsa
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
