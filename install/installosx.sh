#!/usr/bin/env sh

echo "Always open everything in Finder's list view"
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

echo "show all filename extensions"
defaults write NSGlobalDomain AppleShowAllExtensions YES

echo "show hidden files by default"
defaults write com.apple.finder AppleShowAllFiles YES

echo "only use UTF-8 in Terminal.app"
defaults write com.apple.terminal StringEncodings -array 4

echo "expand save dialog by default"
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode YES

echo "show the ~/Library folder in Finder"
chflags nohidden ~/Library

# echo "disable resume system wide"
# defaults write NSGlobalDomainNSQuitAlwaysKeepWindows NO

echo "Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

echo "Enable subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# echo "Enable the 2D Dock"
# defaults write com.apple.dock no-glass YES

# Automatically hide and show the Dock
# defaults write com.apple.dock autohide YES

# echo "Make Dock icons of hidden applications translucent"
# defaults write com.apple.dock showhidden YES

#echo "Enable iTunes track notifications in the Dock"
#defaults write com.apple.dock itunes-notifications YES

# Disable menu bar transparency
#defaults write NSGlobalDomain AppleEnableMenuBarTransparency NO

echo "Show remaining battery time; hide percentage"
defaults write com.apple.menuextra.battery ShowPercent -string "YES"
# defaults write com.apple.menuextra.battery ShowTime -string "YES"

# echo "Always show scrollbars"
# defaults write NSGlobalDomain AppleShowScrollBars -string "Auto"

#echo "Allow quitting Finder via ⌘ + Q; doing so will also hide desktop icons"
#defaults write com.apple.finder QuitMenuItem YES

# Disable window animations and Get Info animations in Finder
# defaults write com.apple.finder DisableAllAnimations YES

echo "Use current directory as default search scope in Finder"
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

echo "Show Path bar in Finder"
defaults write com.apple.finder ShowPathbar YES

echo "Show Status bar in Finder"
defaults write com.apple.finder ShowStatusBar YES

# echo "Expand print panel by default"
# defaults write NSGlobalDomain PMPrintingExpandedStateForPrint YES

echo "Disable the “Are you sure you want to open this application?” dialog"
defaults write com.apple.LaunchServices LSQuarantine NO

#echo "Disable shadow in screenshots"
#defaults write com.apple.screencapture disable-shadow YES

# echo "Enable highlight hover effect for the grid view of a stack (Dock)"
# defaults write com.apple.dock mouse-over-hilte-stack YES

# echo "Enable spring loading for all Dock items"
# defaults write enable-spring-load-actions-on-all-items YES

echo "Show indicator lights for open applications in the Dock"
defaults write com.apple.dock show-process-indicators YES

# Don’t animate opening applications from the Dock
# defaults write com.apple.dock launchanim NO

#echo "Display ASCII control characters using caret notation in standard text views"
# Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
#defaults write NSGlobalDomain NSTextShowsControlCharacters YES

echo "Disable press-and-hold for keys in favor of key repeat"
defaults write NSGlobalDomain ApplePressAndHoldEnabled NO

echo "Set a blazingly fast keyboard repeat rate"
defaults write NSGlobalDomain KeyRepeat -int 1

echo "Set a shorter Delay until key repeat"
defaults write NSGlobalDomain InitialKeyRepeat -int 15

echo "Disable auto-correct"
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled NO

# Disable opening and closing window animations
# defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled NO

# echo "Disable disk image verification"
# defaults write com.apple.frameworks.diskimages skip-verify YES
# defaults write com.apple.frameworks.diskimages skip-verify-locked YES
# defaults write com.apple.frameworks.diskimages skip-verify-remote YES

# echo "Automatically open a new Finder window when a volume is mounted"
# defaults write com.apple.frameworks.diskimages auto-open-ro-root YES
# defaults write com.apple.frameworks.diskimages auto-open-rw-root YES
# defaults write com.apple.finder OpenWindowForNewRemovableDisk YES

# echo "Display full POSIX path as Finder window title"
# defaults write com.apple.finder _FXShowPosixPathInTitle YES

echo "Increase window resize speed for Cocoa applications"
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

echo "Avoid creating .DS_Store files on network volumes"
defaults write com.apple.desktopservices DSDontWriteNetworkStores YES

# echo "Disable the warning when changing a file extension"
# defaults write com.apple.finder FXEnableExtensionChangeWarning NO

# echo "Show item info below desktop icons"
# /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

echo "Enable snap-to-grid for desktop icons"
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

# echo "Disable the warning before emptying the Trash"
# defaults write com.apple.finder WarnOnEmptyTrash NO

# Empty Trash securely by default
# defaults write com.apple.finder EmptyTrashSecurely YES

#echo "Require password immediately after sleep or screen saver begins"
#defaults write com.apple.screensaver askForPassword -int 1
#defaults write com.apple.screensaver askForPasswordDelay -int 0

echo "Enable tap to click (Trackpad)"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking YES

#echo "Map bottom right Trackpad corner to right-click"
#defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
#defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick YES

# echo "Disable Safari’s thumbnail cache for History and Top Sites"
# defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2

echo "Hide Safari's bookmark bar"
defaults write com.apple.Safari ShowFavoritesBar NO

echo "Enable Safari’s debug menu"
defaults write com.apple.Safari IncludeInternalDebugMenu YES

# echo "Make Safari’s search banners default to Contains instead of Starts With"
# defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly NO

# Remove useless icons from Safari’s bookmarks bar
# defaults write com.apple.Safari ProxiesInBookmarksBar "()"

# echo "Add a context menu item for showing the Web Inspector in web views"
# defaults write NSGlobalDomain WebKitDeveloperExtras YES

# echo "Disable the Ping sidebar in iTunes"
# defaults write com.apple.iTunes disablePingSidebar YES

# echo "Disable all the other Ping stuff in iTunes"
# defaults write com.apple.iTunes disablePing YES

# echo "Make ⌘ + F focus the search input in iTunes"
# defaults write com.apple.iTunes NSUserKeyEquivalents -dict-add "Target Search Field" "@F"

# Disable send and reply animations in Mail.app
# defaults write com.apple.Mail DisableReplyAnimations YES
# defaults write com.apple.Mail DisableSendAnimations YES

# Disable Resume system-wide
# defaults write NSGlobalDomain NSQuitAlwaysKeepsWindows NO

# echo "Disable the “reopen windows when logging back in” option"
# This works, although the checkbox will still appear to be checked.
# defaults write com.apple.loginwindow TALLogoutSavesState NO
# defaults write com.apple.loginwindow LoginwindowLaunchesRelaunchApps NO

# echo "Enable Dashboard dev mode (allows keeping widgets on the desktop)"
# defaults write com.apple.dashboard devmode YES

#echo "Reset Launchpad"
#[ -e ~/Library/Application\ Support/Dock/*.db ] && rm ~/Library/Application\ Support/Dock/*.db

# echo "Disable local Time Machine backups"
# hash tmutil &> /dev/null && sudo tmutil disablelocal

#echo "Remove Dropbox’s green checkmark icons in Finder"
#file=/Applications/Dropbox.app/Contents/Resources/check.icns
#[ -e "$file" ] && mv -f "$file" "$file.bak"
#unset file

#Fix for the ancient UTF-8 bug in QuickLook (http://mths.be/bbo)
# Commented out, as this is known to cause problems when saving files in Adobe Illustrator CS5 :(
#echo "0x08000100:0" > ~/.CFUserTextEncoding

echo "Kill affected applications"
for app in Finder Dock SystemUIServer; do killall "$app" >/dev/null 2>&1; done
