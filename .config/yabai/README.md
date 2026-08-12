# yabai + skhd (XMonad-style)

Mod key is **Alt** (Option). Layout mode is tracked **per space** by `bin/ylayout`.

## Layouts

| Mode | What it does |
|------|----------------|
| **bsp** (default) | Plain yabai binary space partitioning |
| **center** | CenterMaster: master in the middle, remaining windows stacked on the left and right (space uses `float` + absolute frames) |

Cycle with `Alt-Space`: `bsp` ↔ `center`.

### Helpers

| Path | Role |
|------|------|
| `bin/ylayout` | Switch layout mode, dispatch focus/promote, re-apply center on window events |
| `bin/center-master` | Apply CenterMaster geometry |

Center master width (center mode): per-space ratio in `~/.local/state/yabai/ratios.json` (default `YLAYOUT_MASTER_RATIO`, `0.50`). Step: `YLAYOUT_RATIO_STEP` (default `0.05`). Padding/gap: `YLAYOUT_PAD` / `YLAYOUT_GAP` (default `8`).

### Ghostty tabs

macOS native tabs are separate windows in the WM API ([Ghostty docs](https://ghostty.org/docs/help/macos-tiling-wms)); there is no perfect fix.

**What breaks:** tab close remaps CG window ids. yabai often keeps a zombie (`has-ax-reference: false`) while the real window is still on screen — looks unmanaged / floating. Space-switch recovery blinks, so we do not use it.

| Approach | Notes |
|----------|--------|
| **Prefer splits** (`Cmd-D` / `Cmd-Shift-D`) over tabs | Best: one NSWindow, no yabai noise |
| **yabairc** | `manage=on`; create/destroy → unfloat (bsp) or restamp slot (center) |
| **center mode** | One Ghostty tile. If yabai lost AX, System Events still moves the real window. Fingerprint retile on Ghostty **appear**, never on transient **disappear** |
| **bsp mode** | Unfloat floating Ghostty windows. No space-switch, no full-space `--layout bsp` |

## Keybinds

### Focus

| Key | Action |
|-----|--------|
| `Alt-j` / `Alt-k` | Focus down / up |
| `Ctrl-Alt-h/j/k/l` | Focus west / south / north / east |

### Swap / master

| Key | Action |
|-----|--------|
| `Alt-Shift-j/k/h/l` | Swap with south / north / west / east |
| `Alt-Return` | Promote focused window to master (`--swap first` in bsp; center master in center mode) |

### Resize

| Key | Action |
|-----|--------|
| `Alt-h` / `Alt-l` | Shrink / grow master (center ratio) or bsp split |
| `Alt-i` / `Alt-o` | Shrink / grow vertical (bsp only; center keeps equal stacks) |
| `Alt-Shift-0` | Reset master ratio `0.50` (center) or balance (bsp) |

### Spaces

| Key | Action |
|-----|--------|
| `Alt-1` … `Alt-9` | Focus space N |
| `Alt-Shift-1` … `Alt-Shift-9` | Send window to space N and follow |

### Layout mode

| Key | Action |
|-----|--------|
| `Alt-Space` | Cycle bsp ↔ center |
| `Alt-m` | CenterMaster |
| `Alt-Shift-m` / `Alt-Shift-Space` / `Alt-b` | bsp |

### Window

| Key | Action |
|-----|--------|
| `Alt-Shift-c` | Close window |
| `Alt-t` | Toggle float |
| `Alt-f` | Zoom fullscreen |
| `Alt-Shift-f` | Native fullscreen |
| `Alt-s` | Toggle sticky |
| `Alt-e` | Toggle split |

### Tree / display

| Key | Action |
|-----|--------|
| `Alt-r` | Rotate tree 90° |
| `Alt-y` / `Alt-x` | Mirror y-axis / x-axis |
| `Alt-w` / `Alt-e` | Focus previous / next display |
| `Alt-Shift-w` / `Alt-Shift-e` | Move window to previous / next display and follow |

### Mouse

| Input | Action |
|-------|--------|
| `Fn` + drag (button1) | Move |
| `Fn` + drag (button2) | Resize |
| Focus follows mouse | `autoraise` |

## Dependencies

- [yabai](https://github.com/koekeishiya/yabai) v7+
- [skhd](https://github.com/koekeishiya/skhd)
- `jq`, `python3` (CenterMaster sizing)

```sh
brew install koekeishiya/formulae/yabai koekeishiya/formulae/skhd
```

Reload:

```sh
yabai --restart-service
skhd --restart-service
```

## Files

```
.config/yabai/yabairc          # yabai config + layout signals
.config/yabai/bin/ylayout      # layout dispatcher
.config/yabai/bin/center-master
.config/skhd/skhdrc            # keybinds
```
