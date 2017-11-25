require "open3"

$BIN = "./build/debug/c4"

print "test name > "
name = gets.strip

print "path to input.c > "
filename = gets.strip

puts
test_folder = "tests/#{name}"
test_input_path = "#{test_folder}/input.c"
puts "Copying file into #{test_folder}"

argument = case name.split("/").first
when "lexer" then "--tokenize"
when "parser" then "--parse"
when "ast" then "--print-ast"
end

Dir.mkdir(test_folder) rescue nil
File.write(test_input_path, File.read(filename))

stdout, stderr, status = Open3.capture3($BIN, argument, test_input_path)
expected_status = stderr.empty? ? 0 : 1

raise "Status must be 1 if error occurs, 0 otherwise" unless expected_status == status.exitstatus

puts "Saving expected output"

File.write("#{test_folder}/stdout", stdout)
File.write("#{test_folder}/stderr", stderr)

puts
