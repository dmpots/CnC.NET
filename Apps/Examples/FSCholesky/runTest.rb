#!/usr/bin/env ruby

puts "hello"

exe   = "bin/Release/FSCholesky.exe"
tests = %w(6 50 100 500 1000 2000)

blockSizes = {
  "6"  => [6,3],
  "50" => [50,25],
  "100" => [100,50],
  "500" => [500,250],
  "1000" => [1000,500],
  "2000" => [2000,1000]
}

tests = %w(2000)
tests.each do |size|  
  baseName = "m"+size+".in"
  testFile = File.join("InputFiles",baseName)
  
  blockSizes[size].each do |bs|
    puts "SIZE: #{size} BLOCKSIZE: #{bs}"
    puts `#{exe} -v -n #{size} -b #{bs}  #{testFile}`
    res  = `diff -q OUT.m OutputFiles/m#{size}.out`
    if res.empty? then
      puts ">>>>> TEST SUCCEEDED"
    else
      puts ">>>>> TEST FAILED -- #{res}"
    end
  end
  
  
  
end
