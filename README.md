# Emacs

My Emacs configuration directory to use on Ruby on Rails and Javascript development.

## Requirements

- Vim keybindings knowledge. This is mandatory because I use a vim emulator called evil (hate that name btw). If you're not familiar with vim, go ahead and launch `vimtutor` on the terminal.
- silversearcher-ag
- Emacs 26+
- RVM (for ruby projects)
- rubocop (for ruby files)

## Quick Start

`$ git clone https://github.com/leodcs/emacs.d ~/.emacs.d`

Open Emacs and run: `M-x all-the-icons-install-fonts <RET>`

## Keybindings

> M - Alt <br/>
> C - Control <br/>
> s - Super (command on macOS) <br/>
> S - Shift <br/>
> SPC - Space <br/>
> RET - Return <br/>

| Keybinding | Action |
| ---------- | ------ |
| `SPC-SPC` | Switch to buffer |
| `SPC-RET` | Go back to previous buffer |
| `s-S-<up>` | Move code up |
| `s-S-<down>` | Move code down |
| `C-n` | Multiple cursor on next occurrence |
| `C-(` | Rubocop on current buffer file |
| `C-)` | Rubocop autocorrect on current buffer file |
| `M-o` | Switch windows |
| `SPC-d` | Delete other windows |
| `s-k` | Kill current buffer |
| `s-e` | Toggle neotree plugin |
| `s-r` | Rename current buffer |
| `s-n` | Create an untitled buffer |
| `s-p` | Search files in project |
| `s-F` | Search code in project |


## Search and replace

Run `s-F`, search for text and press `C-c C-o` <br/>
To disable read-only mode press `C-x C-q` <br/>
Do your changes and finally press `C-c C-c` to confirm

## Plugins documentations

- https://github.com/joaotavora/yasnippet
- https://github.com/asok/projectile-rails
- https://github.com/bbatsov/projectile
- https://github.com/emacs-evil/evil
- https://github.com/gabesoft/evil-mc

# Thanks
Many thanks to [@otavioschwanck](https://github.com/otavioschwanck) for providing me with a setup repo for using Emacs when I was looking for one. Check it out at https://github.com/otavioschwanck/emacs_on_rails
