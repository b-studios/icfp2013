#!/usr/bin/ruby

require "rubygems"
require "thor"
require "uri"
require "net/http"
require "json"

class IcfpCli < Thor



  # should be cached later on
  desc "list", "lists our problems"
  def list()
    puts performRequest('myproblems')
  end

  desc "eval PROGRAM ARGS...", "evaluates a given PROGRAM"
  def eval(program, *args)

    puts performRequest('eval', {
      :program => program,
      :arguments => args
    })
  end

  desc "evalid ID ARGS...", "evaluates the secret program (with id ID) - USE WITH CARE!"
  def evalid(id, *args)

    puts performRequest('eval', {
      :id => id,
      :arguments => args
    })
  end

  desc "guess ID PROGRAM", "guesses the solution for the program with id ID - USE WITH CARE!"
  def guess(id, program)

    puts performRequest('guess', {
      :id => id,
      :program => program
    })
  end
  
  desc "bonus", "generates a bonus program"
  def bonus
    performRequest('train', {
      :size => 42
    })
  end
  
  desc "bonuslist COUNT", "generates a list of bonus programs"
  def bonuslist(count)
    bonuses = []
    (1 .. count.to_i).each do
      resp = askTrain(42)
      bonuses << JSON.parse(resp.body) if resp.code == '200'
      Kernel.sleep(3)    
    end
    
    puts bonuses.map {|b| "#{b['id']} #{b['challenge']}"}.join("\n")
  end
  
  desc "proglist COUNT PROBLEM-SIZE", "generates a list of programs of that size"
  def proglist(count, prob_size)
  
    count, prob_size = count.to_i, prob_size.to_i
    
    (1 .. count).each do
      resp = nil
      begin
        resp = askTrain(prob_size)
        
        if resp.code == '200'
          b = JSON.parse(resp.body)
          puts "#{b['id']} #{b['challenge']}"
        else
          Kernel.sleep(3)
        end
      end while resp.code != '200'
    end
  end
  
  private

  def secret
    '02768XDijvjky5OOedNdAnRxokV6hSA8aaFT1doK'
  end

  def askTrain(size, ops = "")
    http = Net::HTTP.new(uri.host, uri.port)    
    http.post(buildPath('train'), JSON.generate({
      :size => size,
      :operators => "[#{ops}]"
    }))
  end

  def performRequest(path, payload = nil)
    payload = JSON.generate(payload) unless payload == nil

    puts payload
    http = Net::HTTP.new(uri.host, uri.port)    
    handleResponse http.post(buildPath(path), payload)
  end

  def handleResponse(response)

    if response.code == '200'
      JSON.pretty_generate JSON.parse(response.body)
    else
      response.msg
    end

  end

  def uri
    URI('http://icfpc2013.cloudapp.net')
  end

  def buildPath(path)
    "/#{path}?auth=#{secret}vpsH1H"
  end

end
 
IcfpCli.start(ARGV)
