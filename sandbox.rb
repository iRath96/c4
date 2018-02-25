require "open3"
require "diffy"

$BIN = "#{__dir__}/build/debug/c4"

Dir.mkdir(".test") rescue nil
Dir.chdir(".test")
test = "../#{test}"

# "--optimize",
arguments = [ $BIN, "/Users/alex/Desktop/test.c" ].compact
stdout, stderr, status = Open3.capture3(*arguments)
puts status.inspect
if status.exitstatus > 0
  puts stderr
  exit 1
end

puts `llc -relocation-model=pic -filetype=obj test.ll; gcc -fPIC test.o -o test`
