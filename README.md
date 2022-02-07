# My NixOS configuration

## To install
```bash
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo nix-channel --update
mkdir -p ~/.config
git clone https://github.com/epeery/nixos-config ~/.config/nixpkgs
cd ~/.config/nixpkgs
nix build --impure
./result/activate
```
