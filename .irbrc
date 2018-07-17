require 'irb/completion'
require 'irbtools'
require 'pp'
require 'rubygems'
require 'wirble'
require 'interactive_editor'

IRB.conf[:SAVE_HISTORY] = 100000

Wirble.init
Wirble.colorize
