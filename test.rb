require "open3"
require "diffy"

$BIN = "#{__dir__}/build/debug/c4"

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
$fail_output = []

$tests = Dir["tests/*/*"]
$tests.each do |test|
  test_name = test

  argument = case test.split("/")[1]
  when "lexer" then "--tokenize"
  when "parser" then "--parse"
  when "sema" then "--parse"
  when "ast" then "--print-ast"
  when "compile" then nil
  end

  unless argument
    # ensure we run our compilation from a temporary folder
    Dir.mkdir(".test") rescue nil
    Dir.chdir(".test")
    test = "../#{test}"
  end
  
  arguments = [ $BIN, (argument or "--optimize"), "#{test}/input.c" ].compact
  #puts arguments * " "

  test_start_time = Time.now
  stdout, stderr, status = Open3.capture3(*arguments)
  test_runtime = Time.now - test_start_time

  if argument
    expected_stdout = File.read("#{test}/stdout")
    expected_stderr = File.read("#{test}/stderr")
    expected_status = (expected_stderr.empty?? 0 : 1)
  else # compile
    expected_program_stdout = File.read("#{test}/stdout")
    expected_stdout = ""
    expected_stderr = ""
    expected_status = 0
  end

  expectation = [ expected_stdout, expected_stderr, expected_status ]
  result = [ stdout, stderr, status.exitstatus ]

  unless argument # compile
    llc_output = `llc -relocation-model=pic -filetype=obj input.ll; gcc -fPIC input.o -o test`

    program_stdout = `./test`
    expectation << expected_program_stdout
    result << program_stdout

    Dir.chdir("..")
  end

  test_success = result == expectation
  $failures << test unless test_success

  cb = -> {
    puts
    puts "\033[#{test_success ? 32 : 31}m•\033[0m #{test_name} (#{format_ms test_runtime})"
    puts

    show_diff "stdout", expected_stdout, stdout
    show_diff "stderr", expected_stderr, stderr
    show_diff "status", expected_status, status.exitstatus
    show_diff("result", expected_program_stdout, program_stdout) unless argument

    puts
  }

  if test_success
    cb.call
  else
    $fail_output << cb
  end

  #puts status.exitstatus.inspect
  #puts expected_status.inspect
end

$fail_output.each { |i| i.call }

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
