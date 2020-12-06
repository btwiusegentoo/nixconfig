build:
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "README.org")'
install:
	doas nixos-rebuild switch --flake .
desktop1:
	doas nixos-rebuild switch --flake .#desktop1
laptop1:
	doas nixos-rebuild switch --flake .#laptop1
server1:
	doas nixos-rebuild switch --flake .#server1
