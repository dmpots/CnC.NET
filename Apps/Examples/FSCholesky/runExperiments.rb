#!/usr/bin/env ruby


blockSizes = [25, 50, 125, 250, 500, 1000, 2000]
numThreads = [1,2,3,4,5,6,7,8]
trials = [1]

blockSizes.each do |bs|
  puts "BLOCK: #{bs}"
  numThreads.each do |nt|
    trials.each do    
      puts `bin/Release/FSCholesky.exe -n 2000 -b #{bs} -t #{nt} InputFiles/m2000.in`
      $stdout.flush
    end
  end
end

