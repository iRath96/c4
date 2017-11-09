require "open3"
require "diffy"

$BIN = "./build/debug/c4"

def checkmark
  "\033[32m✓\033[0m"
end

def cross
  "\033[31mx\033[0m"
end

def format_ms(time)
  "#{(time * 1000).round(1)} ms"
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

$start_time = Time.now

$failures = []
$tests = Dir["tests/*/*"]
$tests.each do |test|
  argument = case test.split("/")[1]
  when "lexer" then "--tokenize"
  when "parser" then "--parse"
  end

  test_start_time = Time.now
  stdout, stderr, status = Open3.capture3($BIN, argument, "#{test}/input.c")
  test_runtime = Time.now - test_start_time

  expected_stdout = File.read("#{test}/stdout")
  expected_stderr = File.read("#{test}/stderr")
  expected_status = expected_stderr.empty? ? 0 : 1

  expectation = [ expected_stdout, expected_stderr, expected_status ]
  test_success = [ stdout, stderr, status.exitstatus ] == expectation

  $failures << test unless test_success

  puts
  puts "\033[#{test_success ? 32 : 31}m•\033[0m #{test} (#{format_ms test_runtime})"
  puts

  show_diff "stdout", expected_stdout, stdout
  show_diff "stderr", expected_stderr, stderr
  show_diff "status", expected_status, status.exitstatus

  puts

  #puts status.exitstatus.inspect
  #puts expected_status.inspect
end

total_runtime = Time.now - $start_time

if $failures.empty?
  puts
  puts "\033[32m#{$tests.count} test(s) passed (#{format_ms total_runtime})\033[0m"
  puts

  exit 0
end

puts
puts "\033[31m#{$failures.count} test(s) have failed (#{format_ms total_runtime})\033[0m"
puts $failures.map { |path| "- #{path}\n" }.join
puts

exit 1
