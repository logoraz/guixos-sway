# borrowed from:
# https://github.com/daviwil/dotfiles/.files/.config/qutebrowser/config.py

# Load the autoconfig file (quteconfig.py)
config.load_autoconfig()

# Session Controls
c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True
c.content.autoplay = False

# Better default fonts
c.fonts.web.family.standard = "JetBrains Mono"
c.fonts.web.family.serif = "JetBrains Mono"
c.fonts.web.family.sans_serif = "JetBrains Mono"
c.fonts.web.family.fixed = "JetBrains Mono"
c.fonts.statusbar = "JetBrains Mono"

# Use dark mode where possible
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = "never"
# c.colors.webpage.bg = "black"

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')

# Password Integration with KeepassXC
# --> GPG key is needed for KeepassXC's keyfile... using GPG Key ID below
config.bind('<Alt-Shift-u>',
            'spawn --userscript qute-keepassxc --key DBCAD6D0FE9E5C0E7',
            mode='insert')

config.bind('pw',
            'spawn --userscript qute-keepassxc --key DBCAD6D0FE9E5C0E7',
            mode='normal')

# Load onedark-theme
config.source('onedark-theme.py')

# Load qutemacs file (qutemacs.py) for Emacs Keybindings.
#config.source('qutemacs.py')
