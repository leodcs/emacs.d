# Emacs

My Emacs configuration directory to use on Ruby on Rails and Javascript development.

![demonstration.png](https://i.imgur.com/KLaF6ny.png)

## Requirements

- Vim keybindings knowledge. This is mandatory because I use a vim emulator called evil (hate that name btw). If you're not familiar with vim, go ahead and launch `vimtutor` on the terminal.
- silversearcher-ag
- Emacs 26+
- RVM (for ruby projects)
- rubocop (for ruby files)

## Quick Start

`$ git clone https://github.com/leodcs/emacs.d ~/.emacs.d`

Open Emacs and run:
- `M-x all-the-icons-install-fonts`
- `M-x projectile-discover-projects-in-directory`
- Press `C-c p p` to find your newly discovered project.

## Useful keybindings

> M - Alt <br/>
> C - Control <br/>
> s - Super (command on macOS) <br/>
> S - Shift <br/>
> SPC - Space <br/>
> RET - Return <br/>

| Keybinding | Action |
| ---------- | ------ |
| `s-p` | Search files in project |
| `s-e` | Toggle neotree plugin |
| `s-F` | Search text in project (auto includes selected text in visual mode) |
| `s-n` | Create an untitled buffer |
| `s-r` | Rename current buffer |
| `s-k` | Kill current buffer |
| `s->` | Multiple cursor on next occurrence |
| `M-o` | Switch windows |
| `s-RET` | Switch to buffer |
| `s-<up>` | Move text up |
| `s-<down>` | Move text down |
| `s-w` | Close current window |
| `s-W` | Delete other windows |
| `s-S` | Rubocop autocorrect on current buffer file |

## Search and replace

Run `s-F`, search for text and press `C-c C-o` <br/>
To disable read-only mode press `C-x C-q` <br/>
Do your changes and finally press `C-c C-c` to confirm

## Plugins documentations

- https://github.com/joaotavora/yasnippet
- https://github.com/asok/projectile-rails
- https://github.com/bbatsov/projectile
- https://github.com/emacs-evil/evil

# Thanks
Many thanks to [@otavioschwanck](https://github.com/otavioschwanck) for providing me with a setup repo for using Emacs when I was looking for one. Check it out at https://github.com/otavioschwanck/emacs_on_rails
