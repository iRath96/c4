require "open3"

$productions = Hash.new { |h,k| h[k] = [] }

def generate()
  symbol = $production.shift

  optional = symbol.start_with???
  symbol = symbol[1..-1] if optional
  symbol = symbol[1..-1] if symbol.start_with? ?\\

  return if optional and rand(4) == 0

  unless $productions.has_key?(symbol)
    # terminal
    $terminals << symbol
    return
  end

  productions = $productions[symbol]

  production = productions.sample
  may_delete = production != productions[0] && production.count > 1

  $productions[symbol].delete(production) if may_delete and rand(8) == 0

  $production = [ *production, *$production ]
end

File.read("#{File.dirname(__FILE__)}/grammar.txt")
  .split("\n")
  .map { |l| l.strip }
  .select { |l| not(l.start_with??# or l.empty?) }
  .each do |line|
    nt, prod = line.split(" -> ", 2)
    $productions[nt] << prod.split(" ")
  end

$original_productions = $productions

$max_terminals = 16000
def single_attempt
  while true
    $productions = Hash.new
    $original_productions.each { |(k,v)| $productions[k] = v.clone }

    $terminals = []
    $production = [ "translation-unit" ]

    until $production.empty?
      generate()
      break if $production.count > 8000
    end

    #puts "< #{$terminals.join ' '}" if $terminals.count < 100
    #puts ">" if $production.count > 8000

    next if $terminals.count < 100
    next if $production.count > 8000

    #puts ">>" if $terminals.count > $max_terminals

    next if $terminals.count > $max_terminals
    return $terminals.join(" ")
  end
end

$attempts = 300
$BIN = "#{File.dirname(__FILE__)}/../build/debug/c4"
while true
  out_path = "#{File.dirname(__FILE__)}/test.c"

  code = single_attempt
  print "#{code.length} B\t#{$attempts-1} remaining\n"

  File.write(out_path, code)
  stdout, stderr, status = Open3.capture3($BIN, "--debug", "--no-sema", "--print-ast", out_path)

  unless status.exitstatus == 0
    puts code
    puts stdout
    exit 1
  end

  $attempts -= 1
  exit 0 if $attempts <= 0
end


