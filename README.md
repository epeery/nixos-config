# My NixOS configuration

## To install
```bash
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --update
git clone https://github.com/epeery/nixos-config
mv -f nixos-config ~/.config/nixpkgs
sudo ln -sfr ~/.config/nixpkgs/machines/gtx.nix /etc/nixos/configuration.nix
```
