#!/usr/bin/env ruby

# Do initial config shiz
PROJECT_PATH = "/Users/logaan/programming/erlang/string_tree"
LOG_PATH = File.join(PROJECT_PATH, ".autotest_log")
PLIST_PATH = File.join(PROJECT_PATH, ".autotest.plist")
MAKE_PATH = File.join(PROJECT_PATH, "make")

LAUNCHD_PLIST = <<-CONF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>Label</key>
        <string>erlang_autotest</string>
        <key>ProgramArguments</key>
        <array>
                <string>#{MAKE_PATH}</string>
                <string>test</string>
        </array>
        <key>StandardErrorPath</key>
        <string>#{LOG_PATH}</string>
        <key>StandardOutPath</key>
        <string>#{LOG_PATH}</string>
        <key>WatchPaths</key>
        <array>
                <string>#{PROJECT_PATH}/string_tree.erl</string>
        </array>
        <key>WorkingDirectory</key>
        <string>#{PROJECT_PATH}</string>
</dict>
</plist>
CONF

# Write plist file
File.open(PLIST_PATH, 'w') {|f| f.write(LAUNCHD_PLIST) }

# Tell launchd to load the file
system "launchctl unload #{PLIST_PATH}"
system "launchctl load #{PLIST_PATH}"

# Start watching any output
system "touch #{LOG_PATH}"
system "clear"
system "tail -f #{LOG_PATH}"

