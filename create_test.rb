require "open3"

$BIN = "./build/debug/c4"

puts "Only works for lexer tests at the moment."
puts

print "test name > "
name = gets.strip

print "path to input.c > "
filename = gets.strip

puts
test_folder = "tests/lexer/#{name}"
test_input_path = "#{test_folder}/input.c"
puts "Copying file into #{test_folder}"

Dir.mkdir test_folder
File.write(test_input_path, File.read(filename))

stdout, stderr, status = Open3.capture3($BIN, "--tokenize", test_input_path)
expected_status = stderr.empty? ? 0 : 1

raise "Status must be 1 if error occurs, 0 otherwise" unless expected_status == status.exitstatus

puts "Saving expected output"

File.write("#{test_folder}/stdout", stdout)
File.write("#{test_folder}/stderr", stderr)

puts
