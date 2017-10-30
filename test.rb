require "open3"
require "diffy"

$BIN = "./build/debug/c4"

def checkmark
  "\033[32m✓\033[0m"
end

def cross
  "\033[31mx\033[0m"
end

def show_diff(name, expected, actual)
  if actual == expected
    puts "  #{checkmark} #{name}"
  else
    puts "  #{cross} #{name}"
    puts
    puts Diffy::Diff.new(expected, actual, context: 2).to_s(:color)
    puts
  end
end

$failures = []
$tests = Dir["tests/lexer/*"]
$tests.each do |test|
  stdout, stderr, status = Open3.capture3($BIN, "--tokenize", "#{test}/input.c")
  expected_stdout = File.read("#{test}/stdout")
  expected_stderr = File.read("#{test}/stderr")
  expected_status = expected_stderr.empty? ? 0 : 1

  expectation = [ expected_stdout, expected_stderr, expected_status ]
  test_success = [ stdout, stderr, status.exitstatus ] == expectation

  $failures << test unless test_success

  puts
  puts "\033[#{test_success ? 32 : 31}m•\033[0m #{test}"
  puts

  show_diff "stdout", expected_stdout, stdout
  show_diff "stderr", expected_stderr, stderr
  show_diff "status", expected_status, status.exitstatus

  puts

  #puts status.exitstatus.inspect
  #puts expected_status.inspect
end

exit 0 if $failures.empty?

puts
puts "#{$failures.count} test(s) have failed:"
puts $failures.map { |path| "- #{path}\n" }.join
puts

exit 1
