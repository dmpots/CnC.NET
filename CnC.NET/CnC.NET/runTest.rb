#!/usr/bin/env ruby

outputFile = '__TESTOUTPUT'
outF = File.open(outputFile,"w")
outF.puts("RUNNING UNIT TESTS")
Dir["test/*.cnc"].each do |test|
  print(test)
  outF.puts("<<<<<< #{test} >>>>>>")
  outF.puts(`bin/Debug/CNC.NET.exe #{test}`)
  
  if $?.success? then
    puts "...OK"
  else
    puts "...FAILED"
  end
end