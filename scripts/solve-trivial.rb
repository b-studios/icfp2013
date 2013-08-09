#
# rvm pkg install openssl
# rvm reinstall ruby-2.0.0-p0 --with-gcc=gcc-4.2 --with-openssl-dir=$rvm_path/usr

class Strategy
  def id; end
  def exec; end

  def evalid(args)
    cmd = "./commandline.rb evalid '#{id}' '#{encodeArgs(args)}'"
    result = `#{cmd}`
    parse(result)
  end

  def encodeArgs(args)
    "0x%016X" % args
  end

  def parse(result)
    result
  end
end

class Stupid < Strategy
  def id; "cVBdX88Lz74jTfLTSj2YseZW" end
  def exec
    puts evalid(0xFFFFFFFFFFFFFFFF)
  end
end

Stupid.new.exec

